(*
 * Copyright (c) 2010-2015 Anil Madhavapeddy <anil@recoil.org>
 * Copyright (C) 2015      Thomas Gazagnaire <thomas@gazagnaire.org>
 * Copyright (c) 2018      Martin Lucina <martin@lucina.net>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

open Result
open Mirage_net
open Lwt.Infix
open OS.Solo5

let src = Logs.Src.create "netif" ~doc:"Mirage Solo5 network module"
module Log = (val Logs.src_log src : Logs.LOG)

type +'a io = 'a Lwt.t

type t = {
  id: string;
  mutable active: bool;
  mac: Macaddr.t;
  stats: stats;
}

type error = [
  | Mirage_net.error
  | `Invalid_argument
  | `Unspecified_error
  | `Exn of exn
]

let pp_error ppf = function
  | #Mirage_net.error as e -> Mirage_net.pp_error ppf e
  | `Invalid_argument      -> Fmt.string ppf "Invalid argument"
  | `Unspecified_error     -> Fmt.string ppf "Unspecified error"
  | `Exn e                 -> Fmt.exn ppf e

type solo5_net_info = {
  mac_address: string;
  mtu: int;
}

external solo5_net_info:
  unit -> solo5_net_info = "mirage_solo5_net_info"
external solo5_net_read:
  Cstruct.buffer -> int -> solo5_result * int = "mirage_solo5_net_read"
external solo5_net_write:
  Cstruct.buffer -> int -> solo5_result = "mirage_solo5_net_write"

let devices = Hashtbl.create 1

let connect devname =
  let ni = solo5_net_info () in
  match Macaddr.of_bytes ni.mac_address with
  | None -> Lwt.fail_with "Netif: Could not get MAC address"
  | Some mac ->
     Log.info (fun f -> f "Plugging into %s with mac %s"
                        devname (Macaddr.to_string mac));
     let active = true in
     (* XXX: hook up ni.mtu *)
     let t = {
         id=devname; active; mac;
         stats= { rx_bytes=0L;rx_pkts=0l; tx_bytes=0L; tx_pkts=0l } }
     in
     Hashtbl.add devices devname t;
     Lwt.return t

let disconnect t =
  Log.info (fun f -> f "Disconnect %s" t.id);
  t.active <- false;
  Lwt.return_unit

type macaddr = Macaddr.t
type page_aligned_buffer = Io_page.t
type buffer = Cstruct.t

(* Input a frame, and block if nothing is available *)
let rec read t buf =
  let process () =
    let r = match solo5_net_read buf.Cstruct.buffer buf.Cstruct.len with
      | (SOLO5_R_OK, len)    ->
        t.stats.rx_pkts <- Int32.succ t.stats.rx_pkts;
        t.stats.rx_bytes <- Int64.add t.stats.rx_bytes (Int64.of_int len);
        let buf = Cstruct.sub buf 0 len in
        Ok buf
      | (SOLO5_R_AGAIN, _)   -> Error `Continue
      | (SOLO5_R_EINVAL, _)  -> Error `Invalid_argument
      | (SOLO5_R_EUNSPEC, _) -> Error `Unspecified_error
    in
    Lwt.return r
  in
  process () >>= function
  | Ok buf                   -> Lwt.return (Ok buf)
  | Error `Continue          ->
    OS.Main.wait_for_work () >>= fun () -> read t buf
  | Error `Canceled          -> Lwt.return (Error `Canceled)
  | Error `Invalid_argument  -> Lwt.return (Error `Invalid_argument)
  | Error `Unspecified_error -> Lwt.return (Error `Unspecified_error)

let safe_apply f x =
  Lwt.catch
    (fun () -> f x)
    (fun exn ->
       Log.err (fun f -> f "[listen] error while handling %s, continuing. bt: %s"
                           (Printexc.to_string exn) (Printexc.get_backtrace ()));
       Lwt.return_unit)

(* Loop and listen for packets permanently *)
(* this function has to be tail recursive, since it is called at the
   top level, otherwise memory of received packets and all reachable
   data is never claimed.  take care when modifying, here be dragons! *)
let rec listen t fn =
  match t.active with
  | true ->
    let buf = Cstruct.create 1514 in (* XXX: hook up ni.mtu *)
    let process () =
      read t buf >|= function
      | Ok buf                   ->
        Lwt.async (fun () -> safe_apply fn buf) ; Ok ()
      | Error `Canceled          -> Error `Disconnected
      | Error `Invalid_argument  -> Error `Invalid_argument
      | Error `Unspecified_error -> Error `Unspecified_error
    in
    process () >>= (function
      | Ok () -> (listen[@tailcall]) t fn
      | Error e -> Lwt.return (Error e))
  | false -> Lwt.return (Ok ())

(* Transmit a packet from a Cstruct.t *)
let write t buf =
  let open Cstruct in
  let r = match solo5_net_write buf.buffer buf.len with
    | SOLO5_R_OK      ->
      t.stats.tx_pkts <- Int32.succ t.stats.tx_pkts;
      t.stats.tx_bytes <- Int64.add t.stats.tx_bytes (Int64.of_int buf.len);
      Ok ()
    | SOLO5_R_AGAIN   -> assert false (* Not returned by solo5_net_write() *)
    | SOLO5_R_EINVAL  -> Error `Invalid_argument
    | SOLO5_R_EUNSPEC -> Error `Unspecified_error
  in
  Lwt.return r

let writev t = function
  | []       -> Lwt.return (Ok ())
  | [buffer] -> write t buffer
  | buffers  ->
    write t @@ Cstruct.concat buffers

let mac t = t.mac

let get_stats_counters t = t.stats

let reset_stats_counters t =
  t.stats.rx_bytes <- 0L;
  t.stats.rx_pkts  <- 0l;
  t.stats.tx_bytes <- 0L;
  t.stats.tx_pkts  <- 0l

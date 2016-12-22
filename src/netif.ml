(*
 * Copyright (c) 2010-2015 Anil Madhavapeddy <anil@recoil.org>
 * Copyright (C) 2015      Thomas Gazagnaire <thomas@gazagnaire.org>
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
open V1.Network
open Lwt.Infix

let src = Logs.Src.create "netif" ~doc:"Mirage Solo5 network module"
module Log = (val Logs.src_log src : Logs.LOG)

type +'a io = 'a Lwt.t

type t = {
  id: string;
  mutable active: bool;
  mac: Macaddr.t;
  stats : stats;
}

type error = [
  | V1.Network.error
  | `Partial of int * Cstruct.t
  | `Exn of exn
]

let pp_error ppf = function
  | #V1.Network.error as e -> Mirage_pp.pp_network_error ppf e
  | `Exn e                 -> Fmt.exn ppf e
  | `Partial (len, buf)    ->
    Fmt.pf ppf "Partial write (%d, expected %d)" len buf.Cstruct.len

external solo5_net_mac: unit -> string = "stub_net_mac"
external solo5_net_read: Cstruct.buffer -> int -> int = "stub_net_read"
external solo5_net_write: Cstruct.buffer -> int -> int = "stub_net_write"

let devices = Hashtbl.create 1

let connect devname =
  match Macaddr.of_string (solo5_net_mac ()) with
  | None -> Lwt.fail_with "Netif: Could not get MAC address"
  | Some mac ->
     Log.info (fun f -> f "Plugging into %s with mac %s"
                        devname (Macaddr.to_string mac));
     let active = true in
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
let rec read t page =
  let buf = Io_page.to_cstruct page in
  let process () =
    Lwt.catch (fun () ->
        let r = match solo5_net_read buf.Cstruct.buffer buf.Cstruct.len with
          | (-1) ->  Error `Continue                 (* EAGAIN or EWOULDBLOCK *)
          | 0    -> Error `Disconnected                                (* EOF *)
          | len ->
            t.stats.rx_pkts <- Int32.succ t.stats.rx_pkts;
            t.stats.rx_bytes <- Int64.add t.stats.rx_bytes (Int64.of_int len);
            let buf = Cstruct.sub buf 0 len in
            Ok buf
        in
        Lwt.return r)
      (function
        | exn ->
          Log.err (fun f -> f "[read] error: %s, continuing"
                            (Printexc.to_string exn));
          Lwt.return (Error `Continue))
  in
  process () >>= function
  | Error `Continue -> OS.Main.wait_for_work () >>= fun () -> read t page
  | Error `Disconnected -> Lwt.return (Error `Disconnected)
  | Ok buf -> Lwt.return (Ok buf)

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
    let page = Io_page.get 1 in
    let process () =
      read t page >|= function
      | Ok buf              -> Lwt.async (fun () -> safe_apply fn buf) ; Ok ()
      | Error `Disconnected -> t.active <- false ; Error `Disconnected
    in
    process () >>= (function
        | Ok () -> listen t fn
        | Error e -> Lwt.return (Error e))
  | false -> Lwt.return (Ok ())

let do_write b =
  Lwt.return ()

(* Transmit a packet from a Cstruct.t *)
let write t buffer =
  let open Cstruct in
  Lwt.catch (fun () ->
      let len' = solo5_net_write buffer.buffer buffer.len in
      t.stats.tx_pkts <- Int32.succ t.stats.tx_pkts;
      t.stats.tx_bytes <- Int64.add t.stats.tx_bytes (Int64.of_int buffer.len);
      if len' <> buffer.len then (
        let err = `Partial (len', buffer) in
        Log.err (fun f -> f "%a" pp_error err);
        Lwt.return (Error err))
      else Lwt.return (Ok ()))
    (fun exn -> Lwt.return (Error (`Exn exn)))

let writev t = function
  | []     -> Lwt.return (Ok ())
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

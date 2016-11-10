#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let () =
  let lint_deps_excluding =
    Some ["mirage-types-lwt" ; "mirage-types" ]
  in
  let opams = [ Pkg.opam_file "opam" ~lint_deps_excluding ] in
  Pkg.describe ~opams "mirage-net-solo5" @@ fun c ->
  Ok [ Pkg.mllib "lib/mirage-net-solo5.mllib"; ]

#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let () =
  Pkg.describe "ptime" @@ fun c ->
  Ok [ Pkg.mllib "src/ptime.mllib";
       Pkg.mllib "src/clock/ptime_clock.mllib" ~dst_dir:"clock/";
       Pkg.clib "src/clock/libptime_clock_stubs.clib" ~lib_dst_dir:"clock/";
       Pkg.lib "src/clock/runtime.js" ~dst:"clock/";
       Pkg.mllib ~api:[] "src/top/ptime_top.mllib" ~dst_dir:"top/";
       Pkg.lib "src/ptime_top_init.ml" ~dst:"ptime_top_init.ml";
       Pkg.doc "doc/index.mld" ~dst:"odoc-pages/index.mld";
       Pkg.doc "test/min_clock.ml"; ]

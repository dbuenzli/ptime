#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let jsoo = Conf.with_pkg "js_of_ocaml"

let () =
  Pkg.describe "ptime" @@ fun c ->
  let jsoo = Conf.value c jsoo in
  Ok [ Pkg.mllib "src/ptime.mllib";
       Pkg.mllib ~api:[] "src/ptime_top.mllib";
       Pkg.lib "src/ptime_top_init.ml";
       Pkg.mllib "src-os/ptime_clock.mllib" ~dst_dir:"os/";
       Pkg.lib ~exts:Exts.c_library "src-os/libptime_clock_stubs" ~dst:"os/";
       Pkg.stublibs ~exts:Exts.c_dll_library "src-os/dllptime_clock_stubs";
       Pkg.mllib ~api:[] "src-os/ptime_clock_top.mllib" ~dst_dir:"os/";
       Pkg.mllib ~cond:jsoo "src-jsoo/ptime_clock.mllib" ~dst_dir:"jsoo/";
       Pkg.doc "test-os/min_clock_os.ml";
       Pkg.doc "test-jsoo/min_clock_jsoo.ml";
       Pkg.doc "test-jsoo/min_clock_jsoo.html"; ]

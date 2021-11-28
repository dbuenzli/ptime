#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let jsoo = Conf.with_pkg "js_of_ocaml"

let () =
  Pkg.describe "ptime" @@ fun c ->
  let jsoo = Conf.value c jsoo in
  Ok [ Pkg.mllib "src/ptime.mllib";
       Pkg.lib "src/ptime_top_init.ml";
       Pkg.mllib ~api:[] "src/ptime_top.mllib" ~dst_dir:"top/";
       Pkg.lib ~exts:Exts.interface "src/ptime_clock" ~dst:"clock/";
       Pkg.mllib "src-os/ptime_clock.mllib" ~dst_dir:"clock/os/";
       Pkg.clib "src-os/libptime_clock_stubs.clib" ~lib_dst_dir:"clock/os/";
       Pkg.mllib ~cond:jsoo "src-jsoo/ptime_clock.mllib" ~dst_dir:"clock/jsoo/";
       Pkg.test "test/test";
       Pkg.test "test/test_unix";
       Pkg.test "test/basics";
(* Unable to find a way to convince ocamlbuild to make these work
   because of https://github.com/ocaml/ocamlbuild/issues/122
       Pkg.test "test-os/min_clock_os";
       Pkg.test ~run:false ~cond:jsoo ~auto:false "test-jsoo/min_clock_jsoo.js";
       Pkg.test
         ~run:false ~cond:jsoo ~auto:false "test-jsoo/min_clock_jsoo.html";
*)
       Pkg.doc "test-os/min_clock_os.ml";
       Pkg.doc "test-jsoo/min_clock_jsoo.ml";
       Pkg.doc "test-jsoo/min_clock_jsoo.html"; ]

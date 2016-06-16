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
       Pkg.clib "src-os/libptime_clock_stubs.clib" ~lib_dst_dir:"os/";
       Pkg.mllib ~api:[] "src-os/ptime_clock_top.mllib" ~dst_dir:"os/";
       Pkg.mllib ~cond:jsoo "src-jsoo/ptime_clock.mllib" ~dst_dir:"jsoo/";
       Pkg.test "test/test";
       Pkg.test "test/test_unix";
       Pkg.test "test/basics";
(*     Pkg.test "test-os/min_clock_os";
       Pkg.test ~cond:jsoo ~auto:false "test-jsoo/tests.otarget"; *)
       Pkg.doc "test-os/min_clock_os.ml";
       Pkg.doc "test-jsoo/min_clock_jsoo.ml";
       Pkg.doc "test-jsoo/min_clock_jsoo.html"; ]

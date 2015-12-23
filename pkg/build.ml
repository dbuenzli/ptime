#!/usr/bin/env ocaml
#directory "pkg";;
#use "topkg.ml";;

let jsoo = Env.bool "jsoo"

let () =
  Pkg.describe "ptime" ~builder:(`OCamlbuild []) [
    Pkg.lib "pkg/META";
    Pkg.lib ~exts:Exts.module_library "src/ptime";
    Pkg.lib "src/ptime_top_init.ml";
    Pkg.lib ~exts:Exts.library "src/ptime_top";
    Pkg.lib ~exts:Exts.module_library "src-os/ptime_clock"
      ~dst:"os/ptime_clock";
    Pkg.lib ~exts:Exts.c_library "src-os/libptime_clock_stubs"
      ~dst:"os/libptime_clock_stubs";
    Pkg.stublibs ~exts:Exts.c_dll_library "src-os/dllptime_clock_stubs";
    Pkg.lib ~exts:Exts.library "src-os/ptime_clock_top"
      ~dst:"os/ptime_clock_top";
    Pkg.lib ~cond:jsoo ~exts:Exts.module_library "src-jsoo/ptime_clock"
      ~dst:"jsoo/ptime_clock";
    Pkg.doc "README.md";
    Pkg.doc "CHANGES.md";
    Pkg.doc "test-os/min_clock_os.ml";
    Pkg.doc "test-jsoo/min_clock_jsoo.ml";
    Pkg.doc "test-jsoo/min_clock_jsoo.html";
 ]

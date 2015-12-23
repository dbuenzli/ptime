#!/usr/bin/env ocaml
#directory "pkg";;
#use "topkg.ml";;

let () =
  Pkg.describe "ptime" ~builder:(`OCamlbuild []) [
    Pkg.lib "pkg/META";
    Pkg.lib ~exts:Exts.module_library "src/ptime";
    Pkg.lib "src/ptime_top_init.ml";
    Pkg.lib ~exts:Exts.library "src/ptime_top";
    Pkg.doc "README.md";
    Pkg.doc "CHANGES.md"; ]

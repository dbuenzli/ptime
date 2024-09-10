open B0_kit.V000
open Result.Syntax

(* OCaml library names *)

let b0_std = B0_ocaml.libname "b0.std"
let compiler_libs_toplevel = B0_ocaml.libname "compiler-libs.toplevel"
let unix = B0_ocaml.libname "unix"

let ptime = B0_ocaml.libname "ptime"
let ptime_clock = B0_ocaml.libname "ptime.clock"
let ptime_clock_os = B0_ocaml.libname "ptime.clock.os"
let ptime_top = B0_ocaml.libname "ptime.top"

(* Libraries *)

let ptime_lib =
  let srcs = [`Dir ~/"src"; `X ~/"src/ptime_top_init.ml" ] in
  B0_ocaml.lib ptime ~srcs

let ptime_clock_lib =
  let srcs = [`Dir ~/"src/clock"] in
  B0_ocaml.lib ptime_clock ~srcs ~requires:[ptime] ~exports:[ptime]

let ptime_clock_os_lib =
  B0_ocaml.deprecated_lib ~exports:[ptime_clock] ptime_clock_os

let ptime_top_lib =
  let srcs = [`Dir ~/"src/top"] in
  B0_ocaml.lib ptime_top ~srcs ~requires:[ptime; compiler_libs_toplevel]

(* Tests *)

let test ?(requires = []) =
  B0_ocaml.test ~requires:(ptime :: b0_std :: requires)

let testing_ptime = `File ~/"test/testing_ptime.ml"

let test_ptime =
  let srcs =
      [ testing_ptime;
       `File ~/"test/test_span.ml"; `File ~/"test/test_base.ml";
       `File ~/"test/test_date.ml"; `File ~/"test/test_date_time.ml";
       `File ~/"test/test_rfc3339.ml"; `File ~/"test/test_ptime.ml" ]
  in
  test ~/"test/test_ptime.ml" ~srcs ~requires:[unix]

let test_gmtime =
  let doc = "Test random stamps against Unix.gmtime" in
  let srcs = [testing_ptime] in
  test ~/"test/test_gmtime.ml" ~srcs ~requires:[unix] ~doc

let test_gmtime_all =
  let doc = "Test all second stamps against Unix.gmtime (very long)" in
  let srcs = [testing_ptime;] in
  test ~/"test/test_gmtime_all.ml" ~run:false ~srcs ~requires:[unix] ~doc

let min_clock =
  let doc = "Minimal clock example" in
  test ~/"test/min_clock.ml" ~run:false ~doc ~requires:[ptime_clock]

(* FIXME b0 this makes the whole build bytecode. *)
(* let min_clock_jsoo =
  let doc = "Minimal clock example in JavaScript" in
  let srcs = [`File ~/"test/min_clock.ml"] in
  let meta = B0_meta.(empty |> tag test) in
  let requires = [ptime; ptime_clock] in
  B0_jsoo.html_page "min-clock-jsoo" ~doc ~srcs ~meta ~requires *)

let examples =
  test ~/"test/examples.ml" ~run:false ~doc:"Examples from the API docs"

(* Packs *)

let default =
  let meta =
    B0_meta.empty
    |> ~~ B0_meta.authors ["The ptime programmers"]
    |> ~~ B0_meta.maintainers ["Daniel Bünzli <daniel.buenzl i@erratique.ch>"]
    |> ~~ B0_meta.homepage "https://erratique.ch/software/ptime"
    |> ~~ B0_meta.online_doc "https://erratique.ch/software/ptime/doc/"
    |> ~~ B0_meta.licenses ["ISC"]
    |> ~~ B0_meta.repo "git+https://erratique.ch/repos/ptime.git"
    |> ~~ B0_meta.issues "https://github.com/dbuenzli/ptime/issues"
    |> ~~ B0_meta.description_tags ["time"; "posix"; "system"; "org:erratique"]
    |> ~~ B0_opam.depends
      [ "ocaml", {|>= "4.08.0"|};
        "ocamlfind", {|build|};
        "ocamlbuild", {|build & != "0.9.0"|};
        "topkg", {|build & >= "1.0.3"|};
      ]
    |> ~~ B0_opam.build
      {|[["ocaml" "pkg/pkg.ml" "build" "--dev-pkg" "%{dev}%"]]|}
    |> B0_meta.tag B0_opam.tag
    |> B0_meta.tag B0_release.tag
  in
  B0_pack.make "default" ~doc:"ptime package" ~meta ~locked:true @@
  B0_unit.list ()

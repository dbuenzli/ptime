open Ocamlbuild_plugin
open Command

let os = Ocamlbuild_pack.My_unix.run_and_read "uname -s"

let system_support_lib = match os with
| "Linux\n" -> [A "-cclib"; A "-lrt"]
| _ -> []

let lib_a = Ocamlbuild_pack.Ocamlbuild_config.a
let clock_stubs_lib = "src-clock/os/libptime_clock_stubs." ^ lib_a

let js_rule () =
  let dep = "%.byte" in
  let prod = "%.js" in
  let f env _ =
    let dep = env dep in
    let prod = env prod in
    let tags = tags_of_pathname prod ++ "js_of_ocaml" in
    Cmd (S [A "js_of_ocaml"; T tags; A "-o";
            Px prod; P dep])
  in
  rule "js_of_ocaml: .byte -> .js" ~dep ~prod f

let () =
  dispatch begin function
  | After_rules ->
      js_rule ();

      (* ptime *)

      ocaml_lib ~tag_name:"use_ptime" ~dir:"src" "src/ptime";

      (* ptime_clock_os *)

      flag_and_dep ["link"; "ocaml"; "link_ptime_clock_os_stubs"]
        (A clock_stubs_lib);

      dep ["record_ptime_clock_os_stubs"]
        [clock_stubs_lib];

      flag ["library"; "ocaml"; "byte"; "record_ptime_clock_os_stubs"]
        (S ([A "-dllib"; A "-lptime_clock_stubs"] @ system_support_lib));
      flag ["library"; "ocaml"; "record_ptime_clock_os_stubs"] (* byt + nat *)
        (S ([A "-cclib"; A "-lptime_clock_stubs"] @ system_support_lib));

      ocaml_lib ~tag_name:"use_ptime_clock_os" ~dir:"src-clock/os"
        "src-clock/os/ptime_clock";
      flag ["link"; "ocaml"; "use_ptime_clock_os"]
        (S [A "-ccopt"; A "-Lsrc-clock/os"]);

      (* ptime_clock_jsoo *)

      ocaml_lib ~tag_name:"use_ptime_clock_jsoo" ~dir:"src-clock-jsoo"
        "src-clock/jsoo/ptime_clock";
  | _ -> ()
  end

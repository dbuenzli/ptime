open Ocamlbuild_plugin
open Command

let os = Ocamlbuild_pack.My_unix.run_and_read "uname -s"

let system_support_lib = match os with
| "Linux\n" -> [A "-cclib"; A "-lrt"]
| _ -> []

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
      flag ["link"; "library"; "ocaml"; "byte"; "use_ptime_clock"]
        (S ([A "-dllib"; A "-lptime_clock_stubs"] @ system_support_lib));
      flag ["link"; "library"; "ocaml"; "native"; "use_ptime_clock"]
        (S ([A "-cclib"; A "-lptime_clock_stubs"] @ system_support_lib));
      flag ["link"; "ocaml"; "link_ptime_clock"]
        (A "src-os/libptime_clock_stubs.a");
      dep ["link"; "ocaml"; "use_ptime_clock"]
        ["src-os/libptime_clock_stubs.a"];
  | _ -> ()
  end

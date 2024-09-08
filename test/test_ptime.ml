(*---------------------------------------------------------------------------
   Copyright (c) 2015 The ptime programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_testing

let main () =
  Test.main @@ fun () ->
  Test.Cli.parse ();
  Test_span.tests ();
  Test_base.tests ();
  Test_date.tests ();
  Test_date_time.tests ();
  Test_rfc3339.tests ();
  ()

let () = if !Sys.interactive then () else exit (main ())

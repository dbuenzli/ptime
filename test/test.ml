(*---------------------------------------------------------------------------
   Copyright (c) 2015 The ptime programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let tests () = Testing.run
    [ Test_span.suite;
      Test_base.suite;
      Test_date.suite;
      Test_date_time.suite;
      Test_rfc3339.suite; ]

let run () =
  Test_rand.cmdline ();
  tests ();
  Testing.log_results ()

let () = if run () then exit 0 else exit 1

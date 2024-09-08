(*---------------------------------------------------------------------------
   Copyright (c) 2015 The ptime programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_testing
open Testing_ptime

let test_stamp_to_date_time () =
  Test.test "random Ptime-valid stamps to date-time" @@ fun () ->
  if Sys.word_size > 32 then begin
    T.date_time_gmtime_witness ~__POS__ Ptime.min;
    T.date_time_gmtime_witness ~__POS__ Ptime.(truncate ~frac_s:0 max);
  end;
  for i = 1 to Rand.loop_len () do
    T.date_time_gmtime_witness ~__POS__ (Rand.stamp ())
  done;
  ()

let main () =
  Test.main @@ fun () ->
  Test.Cli.parse ();
  test_stamp_to_date_time ();
  ()

let () = if !Sys.interactive then () else exit (main ())

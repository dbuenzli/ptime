(*---------------------------------------------------------------------------
   Copyright (c) 2015 The ptime programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_testing
open Testing_ptime

let test_bounds () =
  Test.test "calendar date field bounds" @@ fun () ->
  let valid_date ?__POS__ d =
    Test.holds ?__POS__ (Option.is_some (Ptime.of_date ?tz_offset_s:None d))
  in
  let wrong_date ?__POS__ d =
    Test.holds ?__POS__ (Option.is_none (Ptime.of_date ?tz_offset_s:None d))
  in
  (* Check year bounds *)
  wrong_date ~__POS__ (-1, 01, 01);
  valid_date ~__POS__ (0, 01, 01);
  valid_date ~__POS__ (1, 01, 01);
  valid_date ~__POS__ (9999, 01, 01);
  wrong_date ~__POS__ (10000, 01, 01);
  wrong_date ~__POS__ (10001, 01, 01);
  (* Check month bounds *)
  wrong_date ~__POS__ (0, 00, 01);
  valid_date ~__POS__ (0, 01, 01);
  valid_date ~__POS__ (0, 12, 01);
  wrong_date ~__POS__ (0, 13, 01);
  (* Check day bounds in 2015 (month lengths) *)
  (* Jan 2015 *)
  wrong_date ~__POS__ (2015, 01, -1);
  valid_date ~__POS__ (2015, 01, 01);
  valid_date ~__POS__ (2015, 01, 31);
  wrong_date ~__POS__ (2015, 01, 32);
  (* Feb 2015, is not leap *)
  wrong_date ~__POS__ (2015, 02, -1);
  valid_date ~__POS__ (2015, 02, 01);
  valid_date ~__POS__ (2015, 02, 28);
  wrong_date ~__POS__ (2015, 02, 29);
  (* Mar 2015 *)
  wrong_date ~__POS__ (2015, 03, -1);
  valid_date ~__POS__ (2015, 03, 01);
  valid_date ~__POS__ (2015, 03, 31);
  wrong_date ~__POS__ (2015, 03, 32);
  (* Apr 2015 *)
  wrong_date ~__POS__ (2015, 04, -1);
  valid_date ~__POS__ (2015, 04, 01);
  valid_date ~__POS__ (2015, 04, 30);
  wrong_date ~__POS__ (2015, 04, 31);
  (* May 2015 *)
  wrong_date ~__POS__ (2015, 05, -1);
  valid_date ~__POS__ (2015, 05, 01);
  valid_date ~__POS__ (2015, 05, 31);
  wrong_date ~__POS__ (2015, 05, 32);
  (* June 2015 *)
  wrong_date ~__POS__ (2015, 06, -1);
  valid_date ~__POS__ (2015, 06, 01);
  valid_date ~__POS__ (2015, 06, 30);
  wrong_date ~__POS__ (2015, 06, 31);
  (* July 2015 *)
  wrong_date ~__POS__ (2015, 07, -1);
  valid_date ~__POS__ (2015, 07, 01);
  valid_date ~__POS__ (2015, 07, 31);
  wrong_date ~__POS__ (2015, 07, 32);
  (* Aug 2015 *)
  wrong_date ~__POS__ (2015, 08, -1);
  valid_date ~__POS__ (2015, 08, 01);
  valid_date ~__POS__ (2015, 08, 31);
  wrong_date ~__POS__ (2015, 08, 32);
  (* Sept 2015 *)
  wrong_date ~__POS__ (2015, 09, -1);
  valid_date ~__POS__ (2015, 09, 01);
  valid_date ~__POS__ (2015, 09, 30);
  wrong_date ~__POS__ (2015, 09, 31);
  (* Oct 2015 *)
  wrong_date ~__POS__ (2015, 10, -1);
  valid_date ~__POS__ (2015, 10, 01);
  valid_date ~__POS__ (2015, 10, 31);
  wrong_date ~__POS__ (2015, 10, 32);
  (* Nov 2015 *)
  wrong_date ~__POS__ (2015, 11, -1);
  valid_date ~__POS__ (2015, 11, 01);
  valid_date ~__POS__ (2015, 11, 30);
  wrong_date ~__POS__ (2015, 11, 31);
  (* Dec 2015 *)
  wrong_date ~__POS__ (2015, 12, -1);
  valid_date ~__POS__ (2015, 12, 01);
  valid_date ~__POS__ (2015, 12, 31);
  wrong_date ~__POS__ (2015, 12, 32);
  (* 1500 is not leap *)
  valid_date ~__POS__ (1500, 02, 28);
  wrong_date ~__POS__ (1500, 02, 29);
  (* 1700 is not leap *)
  valid_date ~__POS__ (1700, 02, 28);
  wrong_date ~__POS__ (1700, 02, 29);
  (* 1800 is not leap *)
  valid_date ~__POS__ (1800, 02, 28);
  wrong_date ~__POS__ (1800, 02, 29);
  (* 1900 is not leap, Lotus 1-2-3 & Excel bug *)
  valid_date ~__POS__ (1900, 02, 28);
  wrong_date ~__POS__ (1900, 02, 29);
  (* 2000 is leap *)
  valid_date ~__POS__ (2000, 02, 28);
  valid_date ~__POS__ (2000, 02, 29);
  wrong_date ~__POS__ (2000, 02, 30);
  (* 2010 is not leap *)
  valid_date ~__POS__ (2010, 02, 28);
  wrong_date ~__POS__ (2010, 02, 29);
  (* 2012 is leap *)
  valid_date ~__POS__ (2012, 02, 29);
  valid_date ~__POS__ (2012, 02, 29);
  wrong_date ~__POS__ (2012, 02, 30);
  (* 2100 is not leap *)
  valid_date ~__POS__ (2100, 02, 28);
  wrong_date ~__POS__ (2100, 02, 29);
  ()

let test_stamp_trips () =
  Test.test "random valid dates to stamps round trips" @@ fun () ->
  let of_date ?__POS__ d =
    Ptime.of_date ?tz_offset_s:None d |> Test.get_some ?__POS__
  in
  for i = 1 to Rand.loop_len () do
    let date = Rand.date () in
    let trip = Ptime.to_date (of_date date) in
    T.date ~__POS__ date trip
  done;
  ()

let tests () =
  test_bounds ();
  test_stamp_trips ();
  ()

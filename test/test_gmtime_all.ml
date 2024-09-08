(*---------------------------------------------------------------------------
   Copyright (c) 2015 The ptime programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_testing
open Testing_ptime

let exhaustive_min_max =
  if Sys.word_size > 32
  then Ptime.(to_float_s min), Ptime.(to_float_s  (truncate ~frac_s:0 max))
  else Int32.(to_float min_int), Int32.(to_float max_int)

let test_exhaustive () =
  Test.test "each Ptime-valid  second stamp to date-time" @@ fun () ->
  let min, max = exhaustive_min_max in
  let rec loop t =
    if t > max then () else
    let stamp = Ptime.of_float_s t |> Option.get in
    T.date_time_gmtime_witness ~__POS__ stamp;
    loop (t +. 1.0)
  in
  loop min

let main () =
  Test.main @@ fun () ->
  test_exhaustive ();
  ()

let () = if !Sys.interactive then () else exit (main ())

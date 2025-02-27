(*---------------------------------------------------------------------------
   Copyright (c) 2015 The ptime programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_testing
open Testing_ptime

let test_stamp_to_date_time =
  Test.test "random Ptime-valid stamps to date-time" @@ fun () ->
  if Sys.word_size > 32 then begin
    T.date_time_gmtime_witness ~__POS__ Ptime.min;
    T.date_time_gmtime_witness ~__POS__ Ptime.(truncate ~frac_s:0 max);
  end;
  for i = 1 to Rand.loop_len () do
    T.date_time_gmtime_witness ~__POS__ (Rand.stamp ())
  done;
  ()

let exhaustive_min_max =
  if Sys.word_size > 32
  then Ptime.(to_float_s min), Ptime.(to_float_s  (truncate ~frac_s:0 max))
  else Int32.(to_float min_int), Int32.(to_float max_int)

let test_exhaustive =
  Test.test ~long:true "each Ptime-valid second stamp to date-time" @@
  fun () ->
  let min, max = exhaustive_min_max in
  let rec loop t =
    if t > max then () else
    let stamp = Ptime.of_float_s t |> Option.get in
    T.date_time_gmtime_witness ~__POS__ stamp;
    loop (t +. 1.0)
  in
  loop min

let main () = Test.main @@ fun () -> Test.autorun ()
let () = if !Sys.interactive then () else exit (main ())

(*---------------------------------------------------------------------------
   Copyright (c) 2015 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

open Testing
open Testing_ptime

let stamp_of_date_time d =
  (Ptime.of_date_time $ raw_date_time @-> ret_get_option stamp) d

let valid_date_time d =
  ignore ((Ptime.of_date_time $ raw_date_time @-> ret_some stamp) d)

let wrong_date_time d =
  ignore ((Ptime.of_date_time $ raw_date_time @-> ret_none stamp) d)

let span_of_d_ps s = match Ptime.Span.of_d_ps s with
| None -> invalid_arg ""
| Some s -> s

let time_bounds = test "Date-time time field bounds" @@ fun () ->
  let min_date = Ptime.to_date Ptime.min in
  let min_utc t = min_date, (t, 0) in
  (* Check hour bounds *)
  wrong_date_time (min_utc (-2, 00, 00));
  wrong_date_time (min_utc (-1, 00, 00));
  valid_date_time (min_utc (00, 00, 00));
  valid_date_time (min_utc (01, 00, 00));
  valid_date_time (min_utc (23, 00, 00));
  wrong_date_time (min_utc (24, 00, 00));
  (* Check minute bounds *)
  wrong_date_time (min_utc (00, -2, 00));
  wrong_date_time (min_utc (00, -1, 00));
  valid_date_time (min_utc (00, 00, 00));
  valid_date_time (min_utc (00, 01, 00));
  valid_date_time (min_utc (00, 59, 00));
  wrong_date_time (min_utc (00, 60, 00));
  (* Check second bounds *)
  wrong_date_time (min_utc (00, 00, -2));
  wrong_date_time (min_utc (00, 00, -1));
  valid_date_time (min_utc (00, 00, 00));
  valid_date_time (min_utc (00, 00, 01));
  valid_date_time (min_utc (00, 00, 59));
  valid_date_time (min_utc (00, 00, 60));
  wrong_date_time (min_utc (00, 00, 61));
  ()

let tz = test "Testing date-time time zone calculations" @@ fun () ->
  (* Timestamps with tz offsets around Ptime.{max,min} *)
  wrong_date_time ((0000, 01, 01), ((00, 00, 00), +1));
  valid_date_time ((0000, 01, 01), ((00, 00, 00), +0));
  valid_date_time ((0000, 01, 01), ((00, 00, 00), -1));
  wrong_date_time ((9999, 12, 31), ((23, 59, 59), -1));
  wrong_date_time ((9999, 12, 31), ((23, 59, 60), +0));
  valid_date_time ((9999, 12, 31), ((23, 59, 60), +1));
  (* Convert time zones *)
  let nyc_tz = -4 * 3600 in
  let cam_tz = +1 * 3600 in
  let lau_tz = +2 * 3600 in
  let new_york  = ((2015, 06, 27), ((18, 30, 01), nyc_tz)) in
  let cambridge = ((2015, 06, 27), ((23, 30, 01), cam_tz)) in
  let lausanne  = ((2015, 06, 28), ((00, 30, 01), lau_tz)) in
  let nyc_stamp = stamp_of_date_time new_york in
  let cam_stamp = stamp_of_date_time cambridge in
  let lau_stamp = stamp_of_date_time lausanne in
  eq_stamp nyc_stamp cam_stamp;
  eq_stamp cam_stamp lau_stamp;
  eq_date_time (Ptime.to_date_time ~tz_offset_s:nyc_tz nyc_stamp) new_york;
  eq_date_time (Ptime.to_date_time ~tz_offset_s:cam_tz nyc_stamp) cambridge;
  eq_date_time (Ptime.to_date_time ~tz_offset_s:lau_tz nyc_stamp) lausanne;
  ()

let subsecond = test "Subsecond stamp to date-time" @@ fun () ->
  let add, sub =
    let add t ps = Ptime.(add_span t (span_of_d_ps (0, ps))) in
    let sub t ps = Ptime.(sub_span t (span_of_d_ps (0, ps))) in
    add $ stamp @-> int64 @-> ret_get_option stamp,
    sub $ stamp @-> int64 @-> ret_get_option stamp
  in
  let b0 = sub Ptime.epoch 750_000_000_000L in
  let b1 = sub Ptime.epoch 500_000_000_000L in
  let b2 = sub Ptime.epoch 250_000_000_000L in
  let b = (1969, 12, 31), ((23, 59, 59), +0) in
  eq_date_time b (Ptime.to_date_time b0);
  eq_date_time b (Ptime.to_date_time b1);
  eq_date_time b (Ptime.to_date_time b2);
  let a0 = add Ptime.epoch 750_000_000_000L in
  let a1 = add Ptime.epoch 500_000_000_000L in
  let a2 = add Ptime.epoch 250_000_000_000L in
  let a = (1970, 01, 01), ((00, 00, 00), +0) in
  eq_date_time a (Ptime.to_date_time a0);
  eq_date_time a (Ptime.to_date_time a1);
  eq_date_time a (Ptime.to_date_time a2);
  ()

let leap_sec = test "Testing leap second date-times" @@ fun () ->
  let after_leap_sec = (1999, 01, 01), ((00, 00, 00), 0) in
  let t0 = stamp_of_date_time ((1998, 12, 31), ((23, 59, 59), 0)) in
  let t1 = stamp_of_date_time ((1998, 12, 31), ((23, 59, 60), 0)) in
  let t2 = stamp_of_date_time after_leap_sec in
  eq_stamp t1 t2; (* leap sec is represented by second that comes after *)
  eq_stamp_opt (Some t1) Ptime.(add_span t0 (Span.of_int_s 1));
  eq_date_time after_leap_sec (Ptime.to_date_time t1);
  eq_date_time after_leap_sec (Ptime.to_date_time t2);
  eq_span (Ptime.diff t2 t0) (Ptime.Span.of_int_s 1);
  eq_span (Ptime.diff t1 t0) (Ptime.Span.of_int_s 1);
  eq_span (Ptime.diff t2 t1) (Ptime.Span.of_int_s 0);
  ()

let stamp_trips = test "Random stamps to date-time round trips" @@ fun () ->
  let stamp_of_posix_s =
    Ptime.of_float_s $ pp_float @-> (ret_get_option stamp)
  in
  let trip ?tz_offset_s t =
    let back = stamp_of_posix_s (floor (Ptime.to_float_s t)) in
    let trip = stamp_of_date_time (Ptime.to_date_time ?tz_offset_s t) in
    eq_stamp back trip
  in
  for i = 1 to Test_rand.loop_len () do
    trip ~tz_offset_s:0 (* UTC *) (Test_rand.float_stamp ());
    trip ~tz_offset_s:(Test_rand.tz_offset_s ()) (Test_rand.float_stamp ())
  done

let round_trips =
  test "Random valid date-times to stamp round trips" @@ fun () ->
  let is_leap_sec = function
  | (_, _, _), ((_, _, 60), _) -> true
  | _ -> false
  in
  let rec rand_date_time_stamp () = (* biased *)
    let date = Test_rand.date () in
    let time = Test_rand.time () in
    let tz = Test_rand.tz_offset_s () in
    let dt = (date, (time, tz)) in
    match Ptime.of_date_time dt with
    | Some _ -> dt
    | None ->
        let dt = date, (time, 0) (* try in UTC *) in
        begin match Ptime.of_date_time dt with
        | None -> rand_date_time_stamp () (* start again *)
        | Some _ -> dt
        end
  in
  let add_posix_s =
    let span = Ptime.Span.of_float_s $ pp_float @-> ret_get_option span in
    let add_posix_s t s = Ptime.(add_span t (span s)) in
    add_posix_s $ stamp @-> pp_float @-> ret_get_option stamp
  in
  for i = 1 to Test_rand.loop_len () do
    let (_, (_, tz_offset_s) as dt) = rand_date_time_stamp () in
    let stamp = stamp_of_date_time dt in
    if not (is_leap_sec dt)
    then eq_date_time dt (Ptime.to_date_time ~tz_offset_s stamp)
    else begin
      (* Verify we map the leap sec on the the second after. *)
      let before_leap_dt = match dt with
      | date, ((hh, ss, 60), tz) -> date, ((hh, ss, 59), tz)
      | _ -> assert false
      in
      let stamp' = add_posix_s (stamp_of_date_time before_leap_dt) 1. in
      eq_stamp stamp stamp'
    end
  done;
  ()

let suite = suite "Ptime date-time tests"
    [ time_bounds;
      tz;
      subsecond;
      leap_sec;
      stamp_trips;
      round_trips; ]

(*---------------------------------------------------------------------------
   Copyright (c) 2015 Daniel C. Bünzli.
   All rights reserved.

   Redistribution and use in source and binary forms, with or without
   modification, are permitted provided that the following conditions
   are met:

   1. Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

   2. Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

   3. Neither the name of Daniel C. Bünzli nor the names of
      contributors may be used to endorse or promote products derived
      from this software without specific prior written permission.

   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
   LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
   DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
   THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
   (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
   OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
  ---------------------------------------------------------------------------*)

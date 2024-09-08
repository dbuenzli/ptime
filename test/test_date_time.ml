(*---------------------------------------------------------------------------
   Copyright (c) 2015 The ptime programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_testing
open Testing_ptime

let stamp_of_date_time ?__POS__ d =
  Ptime.of_date_time d |> Test.get_some ?__POS__

let valid_date_time ?__POS__ d =
  Test.holds ?__POS__ (Option.is_some (Ptime.of_date_time d))

let wrong_date_time ?__POS__ d =
  Test.holds ?__POS__ (Option.is_none (Ptime.of_date_time d))

let test_time_bounds () =
  Test.test "date-time time field bounds" @@ fun () ->
  let min_date = Ptime.to_date Ptime.min in
  let min_utc t = min_date, (t, 0) in
  (* Check hour bounds *)
  wrong_date_time ~__POS__ (min_utc (-2, 00, 00));
  wrong_date_time ~__POS__ (min_utc (-1, 00, 00));
  valid_date_time ~__POS__ (min_utc (00, 00, 00));
  valid_date_time ~__POS__ (min_utc (01, 00, 00));
  valid_date_time ~__POS__ (min_utc (23, 00, 00));
  wrong_date_time ~__POS__ (min_utc (24, 00, 00));
  (* Check minute bounds *)
  wrong_date_time ~__POS__ (min_utc (00, -2, 00));
  wrong_date_time ~__POS__ (min_utc (00, -1, 00));
  valid_date_time ~__POS__ (min_utc (00, 00, 00));
  valid_date_time ~__POS__ (min_utc (00, 01, 00));
  valid_date_time ~__POS__ (min_utc (00, 59, 00));
  wrong_date_time ~__POS__ (min_utc (00, 60, 00));
  (* Check second bounds *)
  wrong_date_time ~__POS__ (min_utc (00, 00, -2));
  wrong_date_time ~__POS__ (min_utc (00, 00, -1));
  valid_date_time ~__POS__ (min_utc (00, 00, 00));
  valid_date_time ~__POS__ (min_utc (00, 00, 01));
  valid_date_time ~__POS__ (min_utc (00, 00, 59));
  valid_date_time ~__POS__ (min_utc (00, 00, 60));
  wrong_date_time ~__POS__ (min_utc (00, 00, 61));
  ()

let test_tz () =
  Test.test "testing date-time time zone calculations" @@ fun () ->
  (* Timestamps with tz offsets around Ptime.{max,min} *)
  wrong_date_time ~__POS__ ((0000, 01, 01), ((00, 00, 00), +1));
  valid_date_time ~__POS__ ((0000, 01, 01), ((00, 00, 00), +0));
  valid_date_time ~__POS__ ((0000, 01, 01), ((00, 00, 00), -1));
  wrong_date_time ~__POS__ ((9999, 12, 31), ((23, 59, 59), -1));
  wrong_date_time ~__POS__ ((9999, 12, 31), ((23, 59, 60), +0));
  valid_date_time ~__POS__ ((9999, 12, 31), ((23, 59, 60), +1));
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
  T.stamp ~__POS__ nyc_stamp cam_stamp;
  T.stamp ~__POS__ cam_stamp lau_stamp;
  T.date_time ~__POS__
    (Ptime.to_date_time ~tz_offset_s:nyc_tz nyc_stamp) new_york;
  T.date_time ~__POS__
    (Ptime.to_date_time ~tz_offset_s:cam_tz nyc_stamp) cambridge;
  T.date_time ~__POS__
    (Ptime.to_date_time ~tz_offset_s:lau_tz nyc_stamp) lausanne;
  ()

let test_subsecond () =
  Test.test "subsecond stamp to date-time" @@ fun () ->
  let span_of_d_ps ?__POS__ s =
    Ptime.Span.of_d_ps s |> Test.get_some ?__POS__
  in
  let add, sub =
    let add t ps = Ptime.(add_span t (span_of_d_ps (0, ps))) |> Test.get_some in
    let sub t ps = Ptime.(sub_span t (span_of_d_ps (0, ps))) |> Test.get_some in
    add, sub
  in
  let b0 = sub Ptime.epoch 750_000_000_000L in
  let b1 = sub Ptime.epoch 500_000_000_000L in
  let b2 = sub Ptime.epoch 250_000_000_000L in
  let b = (1969, 12, 31), ((23, 59, 59), +0) in
  T.date_time ~__POS__ b (Ptime.to_date_time b0);
  T.date_time ~__POS__ b (Ptime.to_date_time b1);
  T.date_time ~__POS__ b (Ptime.to_date_time b2);
  let a0 = add Ptime.epoch 750_000_000_000L in
  let a1 = add Ptime.epoch 500_000_000_000L in
  let a2 = add Ptime.epoch 250_000_000_000L in
  let a = (1970, 01, 01), ((00, 00, 00), +0) in
  T.date_time ~__POS__ a (Ptime.to_date_time a0);
  T.date_time ~__POS__ a (Ptime.to_date_time a1);
  T.date_time ~__POS__ a (Ptime.to_date_time a2);
  ()

let test_leap_sec () =
  Test.test "testing leap second date-times" @@ fun () ->
  let after_leap_sec = (1999, 01, 01), ((00, 00, 00), 0) in
  let t0 = stamp_of_date_time ((1998, 12, 31), ((23, 59, 59), 0)) in
  let t1 = stamp_of_date_time ((1998, 12, 31), ((23, 59, 60), 0)) in
  let t2 = stamp_of_date_time after_leap_sec in
  T.stamp ~__POS__ t1 t2
      (* leap sec is represented by second that comes after *);
  T.stamp_option ~__POS__ (Some t1) Ptime.(add_span t0 (Span.of_int_s 1));
  T.date_time ~__POS__ after_leap_sec (Ptime.to_date_time t1);
  T.date_time ~__POS__ after_leap_sec (Ptime.to_date_time t2);
  T.span ~__POS__ (Ptime.diff t2 t0) (Ptime.Span.of_int_s 1);
  T.span ~__POS__ (Ptime.diff t1 t0) (Ptime.Span.of_int_s 1);
  T.span ~__POS__ (Ptime.diff t2 t1) (Ptime.Span.of_int_s 0);
  ()

let test_stamp_trips () =
  Test.test "random stamps to date-time round trips" @@ fun () ->
  let stamp_of_posix_s s = Ptime.of_float_s s |> Test.get_some in
  let trip ?tz_offset_s t =
    let back = stamp_of_posix_s (floor (Ptime.to_float_s t)) in
    let trip = stamp_of_date_time (Ptime.to_date_time ?tz_offset_s t) in
    T.stamp ~__POS__ back trip
  in
  for i = 1 to Rand.loop_len () do
    trip ~tz_offset_s:0 (* UTC *) (Rand.float_stamp ());
    trip ~tz_offset_s:(Rand.tz_offset_s ()) (Rand.float_stamp ())
  done

let test_round_trips () =
  Test.test "random valid date-times to stamp round trips" @@ fun () ->
  let is_leap_sec = function
  | (_, _, _), ((_, _, 60), _) -> true
  | _ -> false
  in
  let rec rand_date_time_stamp () = (* biased *)
    let date = Rand.date () in
    let time = Rand.time () in
    let tz = Rand.tz_offset_s () in
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
    let span s = Ptime.Span.of_float_s s |> Test.get_some in
    let add_posix_s t s = Ptime.(add_span t (span s)) |> Test.get_some in
    add_posix_s
  in
  for i = 1 to Rand.loop_len () do
    let (_, (_, tz_offset_s) as dt) = rand_date_time_stamp () in
    let stamp = stamp_of_date_time dt in
    if not (is_leap_sec dt)
    then begin
      let ((y, _, _), _ as dt') = Ptime.to_date_time ~tz_offset_s stamp in
      assert (Ptime.to_year ~tz_offset_s stamp = y);
      T.date_time ~__POS__ dt dt'
    end
    else begin
      (* Verify we map the leap sec on the the second after. *)
      let before_leap_dt = match dt with
      | date, ((hh, ss, 60), tz) -> date, ((hh, ss, 59), tz)
      | _ -> assert false
      in
      let stamp' = add_posix_s (stamp_of_date_time before_leap_dt) 1. in
      T.stamp ~__POS__ stamp stamp'
    end
  done;
  ()

let test_weekday () =
  Test.test "Ptime.{weekday_num,weekday}" @@ fun () ->
  let module Weekday = struct
    type t = Ptime.weekday
    let equal = ( = )
    let pp ppf v = Format.pp_print_string ppf @@ match v with
      | `Mon -> "`Mon" | `Tue -> "`Tue" | `Wed -> "`Wed" | `Thu -> "`Thu"
      | `Fri -> "`Fri" | `Sat -> "`Sat" | `Sun -> "`Sun"
  end
  in
  let weekday ?__POS__ = Test.eq ?__POS__ (module Weekday) in
  let eq ?__POS__ ?tz_offset_s c wday =
    let s = stamp_of_date_time (c, ((0, 0, 0), 0)) in
    weekday ?__POS__ (Ptime.weekday ?tz_offset_s s) wday
  in
  eq ~__POS__ (1970, 01, 01) `Thu;
  eq ~__POS__ ~tz_offset_s:(-1) (1970, 01, 01) `Wed;
  eq ~__POS__ ~tz_offset_s:86400 (1970, 01, 01) `Fri;
  eq ~__POS__ (1871, 03, 18) `Sat;
  eq ~__POS__ ~tz_offset_s:(-1) (1871, 03, 18) `Fri;
  eq ~__POS__ ~tz_offset_s:86400 (1871, 03, 18) `Sun;
  eq ~__POS__ (1995, 09, 12) `Tue;
  eq ~__POS__ ~tz_offset_s:(-1) (1995, 09, 12) `Mon;
  eq ~__POS__ ~tz_offset_s:86400 (1995, 09, 12) `Wed;
  eq ~__POS__ ~tz_offset_s:172800 (1995, 09, 12) `Thu;
  ()

let tests () =
  test_time_bounds ();
  test_tz ();
  test_subsecond ();
  test_leap_sec ();
  test_stamp_trips ();
  test_round_trips ();
  test_weekday ();
  ()

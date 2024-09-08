(*---------------------------------------------------------------------------
   Copyright (c) 2015 The ptime programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_std
open B0_testing
open Testing_ptime

let stamp_of_s ?__POS__ v = Ptime.of_float_s v |> Test.get_some ?__POS__

let test_stamp_conversions () =
  Test.test "stamp to RFC 3339 conversions" @@ fun () ->
  let stamp_of_date_time ?__POS__ v =
    Ptime.of_date_time v |> Test.get_some ?__POS__
  in
  let dt ?space ?frac_s ?tz_offset_s dt =
    Ptime.to_rfc3339 ?space ?frac_s ?tz_offset_s (stamp_of_date_time dt)
  in
  let stamp ?space ?frac_s ?tz_offset_s s =
    Ptime.to_rfc3339 ?space ?frac_s ?tz_offset_s s
  in
  let dt0 = (1999, 01, 02), ((01, 02, 03), 0) in
  Test.string ~__POS__ "1999-01-02T01:02:03Z"
    (dt ~tz_offset_s:0 dt0);
  Test.string ~__POS__ "1999-01-02 01:02:03Z"
    (dt ~tz_offset_s:0 ~space:true dt0);
  Test.string ~__POS__ "1999-01-02T01:02:03-00:00"
    (dt dt0);
  Test.string ~__POS__ "1999-01-02 01:02:03-00:00"
    (dt ~space:true dt0);
  Test.string ~__POS__ "1999-01-02T02:03:03+01:01"
    (dt ~tz_offset_s:3660 dt0);
  Test.string ~__POS__ "1999-01-02T00:01:03-01:01"
    (dt ~tz_offset_s:(-3660) dt0);
  Test.string ~__POS__ "1999-01-02T01:02:03-00:00"
    (dt ~tz_offset_s:1 dt0);
  Test.string ~__POS__ "1999-01-02T01:02:03-00:00"
    (dt ~tz_offset_s:12960000 dt0);
  Test.string ~__POS__ "1969-12-31T23:59:59.75Z"
    (stamp ~frac_s:2 ~tz_offset_s:0 (stamp_of_s (-.(1. /. 4.))));
  Test.string ~__POS__ "1969-12-31T23:59:59.25Z"
    (stamp ~frac_s:2 ~tz_offset_s:0 (stamp_of_s (-1. +. (1. /. 4.))));
  Test.string ~__POS__ "1970-01-01T00:00:01.001953125Z"
    (stamp ~frac_s:9 ~tz_offset_s:0 (stamp_of_s ( 1. +. (1. /. (2. ** 9.)))));
  Test.string ~__POS__ "1969-12-31T23:59:59.001953125Z"
    (stamp ~frac_s:9 ~tz_offset_s:0 (stamp_of_s (-1. +. (1. /. (2. ** 9.)))));
  Test.string ~__POS__ "1970-01-01T00:00:01.125Z"
    (stamp ~frac_s:3 ~tz_offset_s:0 (stamp_of_s ( 1. +. (1. /. (2. ** 3.)))));
  Test.string ~__POS__ "1969-12-31T23:59:59.125Z"
    (stamp ~frac_s:3 ~tz_offset_s:0 (stamp_of_s (-1. +. (1. /. (2. ** 3.)))));
  Test.string ~__POS__ "1970-01-01T00:00:01.5Z"
    (stamp ~frac_s:1 ~tz_offset_s:0 (stamp_of_s ( 1. +. (1. /. (2. ** 1.)))));
  Test.string ~__POS__ "1969-12-31T23:59:59.5Z"
    (stamp ~frac_s:1 ~tz_offset_s:0 (stamp_of_s (-1. +. (1. /. (2. ** 1.)))));
  Test.string ~__POS__ "1970-01-01T00:00:02.001953125Z"
    (stamp ~frac_s:9 ~tz_offset_s:0 (stamp_of_s ( 2. +. (1. /. (2. ** 9.)))));
  Test.string ~__POS__ "1969-12-31T23:59:58.001953125Z"
    (stamp ~frac_s:9 ~tz_offset_s:0 (stamp_of_s (-2. +. (1. /. (2. ** 9.)))));
  Test.string ~__POS__ "1970-01-01T00:00:02.000000000Z"
    (stamp ~frac_s:9 ~tz_offset_s:0 (stamp_of_s ( 2.)));
  Test.string ~__POS__ "1969-12-31T23:59:58.000000000Z"
    (stamp ~frac_s:9 ~tz_offset_s:0 (stamp_of_s (-2.)));
  Test.string ~__POS__ "1970-01-01T00:00:02.0Z"
    (stamp ~frac_s:1 ~tz_offset_s:0 (stamp_of_s ( 2.)));
  Test.string ~__POS__ "1969-12-31T23:59:58.0Z"
    (stamp ~frac_s:1 ~tz_offset_s:0 (stamp_of_s (-2.)));
  Test.string ~__POS__ "1970-01-01T00:00:02Z"
    (stamp ~frac_s:0 ~tz_offset_s:0 (stamp_of_s ( 2.)));
  Test.string ~__POS__ "1969-12-31T23:59:58Z"
    (stamp ~frac_s:0 ~tz_offset_s:0 (stamp_of_s (-2.)));
  Test.string ~__POS__ "1969-12-31T23:59:58Z"
    (stamp ~frac_s:(-1) ~tz_offset_s:0 (stamp_of_s (-2.)));
  Test.string ~__POS__ "9999-12-31T23:59:59.999999999999Z"
    (stamp ~frac_s:12 ~tz_offset_s:0 Ptime.max);
  Test.string ~__POS__ "0000-01-01T00:00:00.000000000000Z"
    (stamp ~frac_s:12 ~tz_offset_s:0 Ptime.min);
  Test.string ~__POS__ "0000-01-01T00:00:00.000000000000Z"
    (stamp ~frac_s:13 ~tz_offset_s:0 Ptime.min);
  ()

let test_parse () =
  Test.test "RFC 3339 to stamp conversions" @@ fun () ->
  let test_result =
    let ok =
      let equal (t, tz, c) (t', tz',c') =
        Ptime.equal t t' && tz = tz' && c = c'
      in
      let pp ppf (t, tz, count) =
        Fmt.pf ppf "(%a, %a, %d)"
          Ptime.dump t (Test.Fmt.option Fmt.int) tz count
      in
      Test.Eq.make ~equal ~pp ()
    in
    let error =
      let pp ppf = function `RFC3339 ((s, e), err) ->
        Fmt.pf ppf "@[<1>%d-%d:@ @[%a@]@]" s e Ptime.pp_rfc3339_error err
      in
      Test.Eq.make ~pp ()
    in
    fun ?__POS__ -> Test.result' ?__POS__ ~ok ~error
  in
  let edigit = `Exp_chars ['0'; '1'; '2'; '3'; '4'; '5'; '6'; '7'; '8'; '9'] in
  let etz = `Exp_chars ['+'; '-'; 'Z'; 'z'] in
  let etz_strict = `Exp_chars ['+'; '-'; 'Z'] in
  let edtsep = `Exp_chars ['T';'t';' '] in
  let edtsep_strict = `Exp_chars ['T'] in
  let p ?strict ?sub ?start ?len s = Ptime.of_rfc3339 ?strict ?sub ?start s in
  let err (s,e) err = Error (`RFC3339 ((s, e), err)) in
  let err_pos pos e = err (pos, pos) e in
  let ok s ~tz ~count = Ok (stamp_of_s s, tz, count) in
  test_result ~__POS__
    (p "1970-01-01T00:00:02.001953125Z")
    (ok ( 2. +. (1. /. (2. ** 9.))) ~tz:(Some 0) ~count:30);
  test_result ~__POS__
    (p "1970-01-01T00:00:02.001953125-00:00")
    (ok ( 2. +. (1. /. (2. ** 9.))) ~tz:None ~count:35);
  test_result ~__POS__
    (p "1969-12-31T23:59:58.001953125Z")
    (ok (-2. +. (1. /. (2. ** 9.))) ~tz:(Some 0) ~count:30);
  test_result ~__POS__
    (p "1969-12-31T23:59:58.001953125-00:00")
    (ok (-2. +. (1. /. (2. ** 9.))) ~tz:None ~count:35);
  test_result ~__POS__
    (p "1969-13-31T23:59:58.5Z")
    (err (0, 21) `Invalid_stamp);
  test_result ~__POS__
    (p "1969-12-31T23:59:58.Z")
    (err_pos 20 edigit);
  test_result ~__POS__
    (p "1969-12-31T23:59:58.5")
    (err_pos 20 `Eoi);
  test_result ~__POS__
    (p "1969-12-31T23:59:58.5a")
    (err_pos 21 etz);
  test_result ~__POS__
    (p "1969-12-31T23:59:58.5Za")
    (err_pos 22 `Trailing_input);
  test_result ~__POS__
    (p "1969-12-31t23:59:58.5Z")
    (ok (-1.5) ~tz:(Some 0) ~count:22);
  test_result ~__POS__
    (p "1969-12-31 23:59:58.5z")
    (ok (-1.5) ~tz:(Some 0) ~count:22);
  test_result ~__POS__
    (p "1969-12-31T23:59:58.5Z")
    (ok (-1.5) ~tz:(Some 0) ~count:22);
  test_result ~__POS__
    (p "1969-12-31a23:59:58.5Z")
    (err_pos 10 edtsep);
  test_result ~__POS__
    (p ~strict:true "1969-12-31 23:59:58.5Z")
    (err_pos 10 edtsep_strict);
  test_result ~__POS__
    (p ~strict:true "1969-12-31t23:59:58.5Z")
    (err_pos 10 edtsep_strict);
  test_result ~__POS__
    (p ~strict:true "1969-12-31T23:59:58.5z")
    (err_pos 21 etz_strict);
  test_result ~__POS__
    (p "1970-01-01T00:00:00.5+00:01")
    (ok (-59.5) ~tz:(Some 60) ~count:27);
  test_result ~__POS__
    (p "1970-01-01T00:00:00.5+01:01")
    (ok (-3659.5) ~tz:(Some 3660) ~count:27);
  test_result ~__POS__
    (p "1970-01-01T00:00:00.5-00:01")
    (ok (60.5) ~tz:(Some ~-60) ~count:27);
  test_result ~__POS__
    (p "1970-01-01T00:00:00.00+01:01")
    (ok (-3660.00) ~tz:(Some 3660) ~count:28);
  test_result ~__POS__
    (p "1970-01-01T00:00:00.25+01:01")
    (ok (-3659.75) ~tz:(Some 3660) ~count:28);
  test_result ~__POS__
    (p "1970-01-01T00:00:00.25-00:01")
    (ok (60.25) ~tz:(Some ~-60) ~count:28);
  test_result ~__POS__
    (p "1970-01-01T00:00:00-23:59")
    (ok (86340.) ~tz:(Some ~-86340) ~count:25);
  test_result ~__POS__
    (p "1970-01-01T00:00:00-23:59")
    (ok (86340.) ~tz:(Some ~-86340) ~count:25);
  test_result ~__POS__
    (p "1970-01-01T00:00:00+23:59")
    (ok (-86340.) ~tz:(Some 86340) ~count:25);
  test_result ~__POS__
    (p "1970-01-01T00:00:00+23:59")
    (ok (-86340.) ~tz:(Some 86340) ~count:25);
  test_result ~__POS__
    (p "1970-01-01T00:00:00.5-01:01")
    (ok (3660.5) ~tz:(Some ~-3660) ~count:27);
  test_result ~__POS__
    (p "1970-01-01T00:00:00.5-24:01")
    (err (22, 23) `Invalid_stamp);
  test_result ~__POS__
    (p "1970-01-01T00:00:00.5-01:60")
    (err (25, 26) `Invalid_stamp);
  test_result ~__POS__
    (p ~sub:true ~start:1 "X1969-12-31T23:59:58.5ZX")
    (ok (-1.5) ~tz:(Some 0) ~count:22);
  test_result ~__POS__
    (p ~start:1 "X1969-12-31T23:59:58.5ZX")
    (err_pos 23 `Trailing_input);
  test_result ~__POS__
    (p "1969X12-31T23:59:58Z")
    (err_pos 4 (`Exp_chars ['-']));
  test_result ~__POS__
    (p "1969-12X31T23:59:58Z")
    (err_pos 7 (`Exp_chars ['-']));
  test_result ~__POS__
    (p "1969-12-31T23X59:58Z")
    (err_pos 13 (`Exp_chars [':']));
  test_result ~__POS__
    (p "1969-12-31T23:59X58Z")
    (err_pos 16 (`Exp_chars [':']));
  test_result ~__POS__
    (p ~strict:true "1969-12-31T23:59:58+00X00")
    (err_pos 22 (`Exp_chars [':']));
  test_result ~__POS__
    (p "1969-12-31T23:59:58+00X00")
    (err_pos 22 `Trailing_input);
  test_result ~__POS__
    (p ~start:(-1) "1970-01-01")
    (err_pos (-1) `Eoi);
  test_result ~__POS__
    (p ~start:11 "1970-01-01")
    (err_pos 11 `Eoi);
  test_result ~__POS__
    (p "")
    (err_pos 0 `Eoi);
  test_result ~__POS__
    (p "0000-01-01T00:00:00+00:01")
    (err (0, 24) `Invalid_stamp);
  test_result ~__POS__
    (p "9999-12-31T23:59:59-00:01")
    (err (0, 24) `Invalid_stamp);
  test_result ~__POS__
    (p "1900-02-29T01:02:03Z")
    (err (0, 19) `Invalid_stamp);
  test_result ~__POS__
    (p "01-02-29T01:02:03Z")
    (err_pos 2 edigit);
  test_result ~__POS__
    (p "1970-01-01T00:00:00.00+0101")
    (ok (-3660.00) ~tz:(Some 3660) ~count:27);
  test_result ~__POS__
    (p "1970-01-01T00:00:00.00+01")
    (ok (-3600.00) ~tz:(Some 3600) ~count:25);
  ()

let test_stamp_trips () =
  Test.test "random stamps to RFC 3339 round trips" @@ fun () ->
  let stamp_of_rfc3339 ?__POS__ s =
    Ptime.of_rfc3339 s |> Ptime.rfc3339_string_error |> Test.get_ok ?__POS__
  in
  let trip ?__POS__:pos ?tz_offset_s t =
    let back = stamp_of_s ?__POS__:pos (floor (Ptime.to_float_s t)) in
    let trip, tz, _ =
      stamp_of_rfc3339 ?__POS__:pos (Ptime.to_rfc3339 ?tz_offset_s t)
    in
    T.stamp ~__POS__ back trip;
  in
  for i = 1 to Rand.loop_len () do
    trip ~__POS__ ?tz_offset_s:(Some 0)(* UTC *) (Rand.float_stamp ());
    trip ~__POS__ ?tz_offset_s:None (* Unknown *) (Rand.float_stamp ());
    trip ~__POS__ ?tz_offset_s:(Some (Rand.tz_offset_s ()))
      (Rand.float_stamp ())
  done;
  ()

let tests () =
  test_stamp_conversions ();
  test_parse ();
  test_stamp_trips ();
  ()

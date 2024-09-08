(*---------------------------------------------------------------------------
   Copyright (c) 2015 The ptime programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_testing
open Testing_ptime

let p s = Ptime.Span.of_d_ps s |> Test.get_some
let n s = Ptime.Span.(neg (p s))
let pps ps = p (0, ps)
let nps ps = n (0, ps)

let test_conversions () =
  Test.test "span constants and conversions" @@ fun () ->
  (* Ints *)
  let trip_int ?__POS__ secs =
    Test.option ?__POS__ ~some:Test.Eq.int
      Ptime.Span.(to_int_s (of_int_s secs)) (Some secs)
  in
  T.span ~__POS__ (Ptime.Span.of_int_s 0) Ptime.Span.zero;
  T.span ~__POS__ (Ptime.Span.of_int_s 1) (pps 1_000_000_000_000L);
  T.span ~__POS__ (Ptime.Span.of_int_s (-1)) (nps 1_000_000_000_000L);
  T.span ~__POS__ (Ptime.Span.of_int_s 86_400) (p (1, 0L));
  T.span ~__POS__ (Ptime.Span.of_int_s (-86_400)) (n (1, 0L));
  trip_int ~__POS__ (86_400);
  trip_int ~__POS__ (-86_400);
  trip_int ~__POS__ (234_322_342);
  trip_int ~__POS__ (-234_322_352);
  trip_int ~__POS__ (1);
  trip_int ~__POS__ (-1);
  trip_int ~__POS__ (0);
  (* Floats *)
  let trip_float ?__POS__ secs =
    let of_float secs = Ptime.Span.of_float_s secs |> Test.get_some ?__POS__ in
    Test.float ?__POS__ (Ptime.Span.to_float_s (of_float secs)) secs
  in
  T.span_option ~__POS__
    (Ptime.Span.of_float_s 1.0000000000005) (Some (pps 1_000_000_000_000L));
  T.span_option ~__POS__
    (Ptime.Span.of_float_s (-1.0000000000005)) (Some (nps 1_000_000_000_000L));
  T.span_option ~__POS__
    (Ptime.Span.of_float_s 0.) (Some Ptime.Span.zero);
  T.span_option ~__POS__
    (Ptime.Span.of_float_s (min_float)) (Some Ptime.Span.zero);
  T.span_option ~__POS__
    (Ptime.Span.of_float_s (-.min_float))(Some Ptime.Span.zero);
  T.span_option ~__POS__
    (Ptime.Span.of_float_s max_float) None;
  T.span_option ~__POS__
    (Ptime.Span.of_float_s (-.max_float)) None;
  T.span_option ~__POS__
    (Ptime.Span.of_float_s nan) None;
  T.span_option ~__POS__
    (Ptime.Span.of_float_s infinity) None;
  T.span_option ~__POS__
    (Ptime.Span.of_float_s (-.infinity)) None;
  trip_float ~__POS__ 0.;
  trip_float ~__POS__ (-0.);
  trip_float ~__POS__ 1.;
  trip_float ~__POS__ (-1.);
  trip_float ~__POS__ (float (1 lsl 30 - 1));
  trip_float ~__POS__ (float (- (1 lsl 30)));
  T.span_option ~__POS__
    (Ptime.Span.of_d_ps (23, -1L)) None;
  T.span_option ~__POS__
    (Ptime.Span.of_d_ps (23, 86_400_000_000_000_000L)) None;
  ()

let test_predicates () =
  Test.test "span predicates" @@ fun () ->
  Test.bool ~__POS__ (Ptime.Span.equal Ptime.Span.zero Ptime.Span.zero) true;
  Test.bool ~__POS__ (Ptime.Span.equal Ptime.Span.zero (pps 1L)) false;
  Test.bool ~__POS__ (Ptime.Span.equal Ptime.Span.zero (nps 1L)) false;
  Test.bool ~__POS__ (Ptime.Span.equal (p (30, 3434L)) (p (30, 3434L))) true;
  Test.bool ~__POS__ (Ptime.Span.equal (p (30, 3434L)) (p (30, 3435L))) false;
  Test.bool ~__POS__ (Ptime.Span.equal (n (30, 3434L)) (n (30, 3434L))) true;
  Test.bool ~__POS__ (Ptime.Span.equal (n (30, 3434L)) (n (30, 3435L))) false;
  Test.bool ~__POS__ (Ptime.Span.equal (n (30, 3434L)) (p (30, 3434L))) false;
  Test.int ~__POS__ (Ptime.Span.compare Ptime.Span.zero Ptime.Span.zero) 0;
  Test.int ~__POS__ (Ptime.Span.compare Ptime.Span.zero (pps 1L)) (-1);
  Test.int ~__POS__ (Ptime.Span.compare Ptime.Span.zero (nps 1L)) 1;
  Test.int ~__POS__ (Ptime.Span.compare (n (30, 3434L)) (n (30, 3434L))) 0;
  Test.int ~__POS__ (Ptime.Span.compare (n (30, 3434L)) (n (30, 3435L))) 1;
  Test.int ~__POS__ (Ptime.Span.compare (n (30, 3434L)) (p (30, 3435L))) (-1);
  Test.int ~__POS__ (Ptime.Span.compare (n (30, 3434L)) (n (30, 3433L))) (-1);
  Test.int ~__POS__ (Ptime.Span.compare (n (30, 3434L)) (p (30, 3433L))) (-1);
  ()

let test_arithmetic () =
  Test.test "span arithmetic" @@ fun () ->
  T.span ~__POS__
    (Ptime.Span.add (pps 86_399_999_999_999_999L) (pps 1L)) (p (1, 0L));
  T.span ~__POS__
    (Ptime.Span.add (nps 86_399_999_999_999_999L) (nps 1L)) (n (1, 0L));
  T.span ~__POS__
    (Ptime.Span.sub Ptime.Span.zero (pps 1L)) (nps 1L);
  T.span ~__POS__
    (Ptime.Span.sub Ptime.Span.zero (nps 1L)) (pps 1L);
  T.span ~__POS__
    (Ptime.Span.add (nps 1L) (pps 1L)) Ptime.Span.zero;
  T.span ~__POS__
    (Ptime.Span.abs (n (3, 342L))) (p (3, 342L));
  T.span ~__POS__
    (Ptime.Span.abs (p (3, 342L))) (p (3, 342L));
  ()

let test_rounding () =
  Test.test "span rounding" @@ fun () ->
  let r ~frac a b =
    T.span ~__POS__ (Ptime.Span.round ~frac_s:frac (p (3, a))) (p (3, b));
    T.span ~__POS__ (Ptime.Span.round ~frac_s:frac (p (3, a))) (p (3, b))
  in
  let r_carry ~frac a =
    T.span ~__POS__ (Ptime.Span.round ~frac_s:frac (p (3, a))) (p (4, 0L));
    T.span ~__POS__ (Ptime.Span.round ~frac_s:frac (p (3, a))) (p (4, 0L))
  in
  let t ~frac a b =
    T.span ~__POS__ (Ptime.Span.truncate ~frac_s:frac (p (3, a))) (p (3, b));
    T.span ~__POS__ (Ptime.Span.truncate ~frac_s:frac (n (3, a))) (n (3, b))
  in
  for i = 0 to 12 do r ~frac:i 0L 0L done;
  r_carry ~frac:(-1) 86_399_500_000_000_000L;
  r_carry ~frac:0 86_399_500_000_000_000L;
  r       ~frac:0 86_399_499_999_999_999L 86_399_000_000_000_000L;
  r       ~frac:0 10_001_500_000_000_000L 10_002_000_000_000_000L;
  r       ~frac:0 10_001_499_999_999_999L 10_001_000_000_000_000L;
  r_carry ~frac:1 86_399_950_000_000_000L;
  r       ~frac:1 86_399_949_999_999_999L 86_399_900_000_000_000L;
  r       ~frac:1 10_001_150_000_000_000L 10_001_200_000_000_000L;
  r       ~frac:1 10_001_149_999_999_999L 10_001_100_000_000_000L;
  r_carry ~frac:2 86_399_995_000_000_000L;
  r       ~frac:2 86_399_994_999_999_999L 86_399_990_000_000_000L;
  r       ~frac:2 10_001_115_000_000_000L 10_001_120_000_000_000L;
  r       ~frac:2 10_001_114_999_999_999L 10_001_110_000_000_000L;
  r_carry ~frac:3 86_399_999_500_000_000L;
  r       ~frac:3 86_399_999_499_999_999L 86_399_999_000_000_000L;
  r       ~frac:3 10_001_111_500_000_000L 10_001_112_000_000_000L;
  r       ~frac:3 10_001_111_499_999_999L 10_001_111_000_000_000L;
  r_carry ~frac:4 86_399_999_950_000_000L;
  r       ~frac:4 86_399_999_949_999_999L 86_399_999_900_000_000L;
  r       ~frac:4 10_001_111_150_000_000L 10_001_111_200_000_000L;
  r       ~frac:4 10_001_111_149_999_999L 10_001_111_100_000_000L;
  r_carry ~frac:5 86_399_999_995_000_000L;
  r       ~frac:5 86_399_999_994_999_999L 86_399_999_990_000_000L;
  r       ~frac:5 10_001_111_115_000_000L 10_001_111_120_000_000L;
  r       ~frac:5 10_001_111_114_999_999L 10_001_111_110_000_000L;
  r_carry ~frac:6 86_399_999_999_500_000L;
  r       ~frac:6 86_399_999_999_499_999L 86_399_999_999_000_000L;
  r       ~frac:6 10_001_111_111_500_000L 10_001_111_112_000_000L;
  r       ~frac:6 10_001_111_111_499_999L 10_001_111_111_000_000L;
  r_carry ~frac:7 86_399_999_999_950_000L;
  r       ~frac:7 86_399_999_999_949_999L 86_399_999_999_900_000L;
  r       ~frac:7 10_001_111_111_150_000L 10_001_111_111_200_000L;
  r       ~frac:7 10_001_111_111_149_999L 10_001_111_111_100_000L;
  r_carry ~frac:8 86_399_999_999_995_000L;
  r       ~frac:8 86_399_999_999_994_999L 86_399_999_999_990_000L;
  r       ~frac:8 10_001_111_111_115_000L 10_001_111_111_120_000L;
  r       ~frac:8 10_001_111_111_114_999L 10_001_111_111_110_000L;
  r_carry ~frac:9 86_399_999_999_999_500L;
  r       ~frac:9 86_399_999_999_999_499L 86_399_999_999_999_000L;
  r       ~frac:9 10_001_111_111_111_500L 10_001_111_111_112_000L;
  r       ~frac:9 10_001_111_111_111_499L 10_001_111_111_111_000L;
  r_carry ~frac:10 86_399_999_999_999_950L;
  r       ~frac:10 86_399_999_999_999_949L 86_399_999_999_999_900L;
  r       ~frac:10 10_001_111_111_111_150L 10_001_111_111_111_200L;
  r       ~frac:10 10_001_111_111_111_149L 10_001_111_111_111_100L;
  r_carry ~frac:11 86_399_999_999_999_995L;
  r       ~frac:11 86_399_999_999_999_994L 86_399_999_999_999_990L;
  r       ~frac:11 10_001_111_111_111_115L 10_001_111_111_111_120L;
  r       ~frac:11 10_001_111_111_111_114L 10_001_111_111_111_110L;
  r       ~frac:12 86_399_999_999_999_999L 86_399_999_999_999_999L;
  r       ~frac:12 10_001_111_111_111_115L 10_001_111_111_111_115L;
  r       ~frac:12 10_001_111_111_111_114L 10_001_111_111_111_114L;
  r       ~frac:13 10_001_111_111_111_114L 10_001_111_111_111_114L;
  for i = 0 to 12 do t ~frac:i 0L 0L done;
  t ~frac:(-1) 86_399_999_999_999_999L 86_399_000_000_000_000L;
  t ~frac:0 86_399_999_999_999_999L 86_399_000_000_000_000L;
  t ~frac:1 86_399_999_999_999_999L 86_399_900_000_000_000L;
  t ~frac:2 86_399_999_999_999_999L 86_399_990_000_000_000L;
  t ~frac:3 86_399_999_999_999_999L 86_399_999_000_000_000L;
  t ~frac:4 86_399_999_999_999_999L 86_399_999_900_000_000L;
  t ~frac:5 86_399_999_999_999_999L 86_399_999_990_000_000L;
  t ~frac:6 86_399_999_999_999_999L 86_399_999_999_000_000L;
  t ~frac:7 86_399_999_999_999_999L 86_399_999_999_900_000L;
  t ~frac:8 86_399_999_999_999_999L 86_399_999_999_990_000L;
  t ~frac:9 86_399_999_999_999_999L 86_399_999_999_999_000L;
  t ~frac:10 86_399_999_999_999_999L 86_399_999_999_999_900L;
  t ~frac:11 86_399_999_999_999_999L 86_399_999_999_999_990L;
  t ~frac:12 86_399_999_999_999_999L 86_399_999_999_999_999L;
  t ~frac:13 86_399_999_999_999_999L 86_399_999_999_999_999L;
  ()

let test_pretty_printing () =
  Test.test "span retty printing" @@ fun () ->
  let fmt s = Format.asprintf "%a" Ptime.Span.pp s in
  let n s = fmt @@ Ptime.Span.(neg (p s)) in
  let p s = fmt @@ p s in
  let pps ps = p (0, ps) in
  let nps ps = n (0, ps) in
  (* y d *)
  Test.string ~__POS__ (p (366, 0L)) "1y1d";
  Test.string ~__POS__ (n (366, 0L)) "-1y1d";
  Test.string ~__POS__ (p (1461, 0L)) "4y";
  Test.string ~__POS__ (n (1461, 0L)) "-4y";
  Test.string ~__POS__ (p (1461, 43_200_000_000_000_000L)) "4y1d";
  Test.string ~__POS__ (n (1461, 43_200_000_000_000_000L)) "-4y1d";
  Test.string ~__POS__ (p (1461, 43_199_199_199_199_199L)) "4y";
  Test.string ~__POS__ (n (1461, 43_199_199_199_199_199L)) "-4y";
  Test.string ~__POS__ (p (1462, 43_200_000_000_000_000L)) "4y2d";
  Test.string ~__POS__ (n (1462, 43_200_000_000_000_000L)) "-4y2d";
  Test.string ~__POS__ (p (1462, 43_199_199_199_199_199L)) "4y1d";
  Test.string ~__POS__ (n (1462, 43_199_199_199_199_199L)) "-4y1d";
  (* d h *)
  Test.string ~__POS__ (p (365, 84_600_000_000_000_000L)) "1y1d";
  Test.string ~__POS__ (n (365, 84_600_000_000_000_000L)) "-1y1d";
  Test.string ~__POS__ (p (365, 84_599_999_999_999_999L)) "1y";
  Test.string ~__POS__ (n (365, 84_599_999_999_999_999L)) "-1y";
  Test.string ~__POS__ (p (365, 19_800_000_000_000_000L)) "1y";
  Test.string ~__POS__ (n (365, 19_800_000_000_000_000L)) "-1y";
  Test.string ~__POS__ (p (365, 19_799_999_999_999_999L)) "365d5h";
  Test.string ~__POS__ (n (365, 19_799_999_999_999_999L)) "-365d5h";
  Test.string ~__POS__ (p (1, 84_600_000_000_000_000L)) "2d";
  Test.string ~__POS__ (n (1, 84_600_000_000_000_000L)) "-2d";
  Test.string ~__POS__ (p (1, 84_599_999_999_999_999L)) "1d23h";
  Test.string ~__POS__ (n (1, 84_599_999_999_999_999L)) "-1d23h";
  Test.string ~__POS__ (p (2, 0L)) "2d";
  Test.string ~__POS__ (n (2, 0L)) "-2d";
  (* h m *)
  Test.string ~__POS__ (pps 86_370_000_000_000_000L) "1d";
  Test.string ~__POS__ (nps 86_370_000_000_000_000L) "-1d";
  Test.string ~__POS__ (pps 86_369_999_999_999_999L) "23h59min";
  Test.string ~__POS__ (nps 86_369_999_999_999_999L) "-23h59min";
  Test.string ~__POS__ (pps 3660_000_000_000_000L) "1h1min";
  Test.string ~__POS__ (nps 3660_000_000_000_000L) "-1h1min";
  Test.string ~__POS__ (pps 3630_000_000_000_000L) "1h1min";
  Test.string ~__POS__ (pps 3629_999_999_999_999L) "1h";
  Test.string ~__POS__ (nps 3629_999_999_999_999L) "-1h";
  Test.string ~__POS__ (pps 3600_000_000_000_000L) "1h";
  Test.string ~__POS__ (nps 3600_000_000_000_000L) "-1h";
  (* m s *)
  Test.string ~__POS__ (pps 3599_500_000_000_000L) "1h";
  Test.string ~__POS__ (nps 3599_500_000_000_000L) "-1h";
  Test.string ~__POS__ (pps 3599_499_999_999_999L) "59min59s";
  Test.string ~__POS__ (nps 3599_499_999_999_999L) "-59min59s";
  Test.string ~__POS__ (pps 60_500_000_000_000L) "1min1s";
  Test.string ~__POS__ (nps 60_500_000_000_000L) "-1min1s";
  Test.string ~__POS__ (pps 60_499_000_000_000L) "1min";
  Test.string ~__POS__ (nps 60_499_000_000_000L) "-1min";
  Test.string ~__POS__ (pps 60_000_000_000_000L) "1min";
  Test.string ~__POS__ (nps 60_000_000_000_000L) "-1min";
  (* s *)
  Test.string ~__POS__ (pps 59_999_500_000_000L) "1min";
  Test.string ~__POS__ (nps 59_999_500_000_000L) "-1min";
  Test.string ~__POS__ (pps 59_999_499_999_999L) "59.999s";
  Test.string ~__POS__ (nps 59_999_499_999_999L) "-59.999s";
  Test.string ~__POS__ (pps 1_999_500_000_000L) "2s";
  Test.string ~__POS__ (nps 1_999_500_000_000L) "-2s";
  Test.string ~__POS__ (pps 1_999_499_999_999L) "1.999s";
  Test.string ~__POS__ (nps 1_999_499_999_999L) "-1.999s";
  Test.string ~__POS__ (pps 1_534_500_000_000L) "1.535s";
  Test.string ~__POS__ (nps 1_534_500_000_000L) "-1.535s";
  Test.string ~__POS__ (pps 1_534_499_999_999L) "1.534s";
  Test.string ~__POS__ (nps 1_534_499_999_999L) "-1.534s";
  Test.string ~__POS__ (pps 1_000_000_000_000L) "1s";
  Test.string ~__POS__ (nps 1_000_000_000_000L) "-1s";
  Test.string ~__POS__ (pps 1_136_000_000_000L) "1.136s";
  Test.string ~__POS__ (nps 1_136_000_000_000L) "-1.136s";
  Test.string ~__POS__ (pps 1_036_000_000_000L) "1.036s";
  Test.string ~__POS__ (nps 1_036_000_000_000L) "-1.036s";
  (* ms *)
  Test.string ~__POS__ (pps 999_500_000_000L) "1s";
  Test.string ~__POS__ (nps 999_500_000_000L) "-1s";
  Test.string ~__POS__ (pps 999_499_999_999L) "999ms";
  Test.string ~__POS__ (nps 999_499_999_999L) "-999ms";
  Test.string ~__POS__ (pps 1_999_500_000L) "2ms";
  Test.string ~__POS__ (nps 1_999_500_000L) "-2ms";
  Test.string ~__POS__ (pps 1_999_499_999L) "1.999ms";
  Test.string ~__POS__ (nps 1_999_499_999L) "-1.999ms";
  Test.string ~__POS__ (pps 1_332_500_000L) "1.333ms";
  Test.string ~__POS__ (nps 1_332_500_000L) "-1.333ms";
  Test.string ~__POS__ (pps 1_332_499_999L) "1.332ms";
  Test.string ~__POS__ (nps 1_332_499_999L) "-1.332ms";
  Test.string ~__POS__ (pps 1_036_000_000L) "1.036ms";
  Test.string ~__POS__ (nps 1_036_000_000L) "-1.036ms";
  Test.string ~__POS__ (pps 1_000_000_000L) "1ms";
  Test.string ~__POS__ (nps 1_000_000_000L) "-1ms";
  (* us *)
  Test.string ~__POS__ (pps 999_500_000L) "1ms";
  Test.string ~__POS__ (nps 999_500_000L) "-1ms";
  Test.string ~__POS__ (pps 999_499_999L) "999us";
  Test.string ~__POS__ (nps 999_499_999L) "-999us";
  Test.string ~__POS__ (pps 1_999_500L) "2us";
  Test.string ~__POS__ (nps 1_999_500L) "-2us";
  Test.string ~__POS__ (pps 1_999_499L) "1.999us";
  Test.string ~__POS__ (nps 1_999_499L) "-1.999us";
  Test.string ~__POS__ (pps 1_332_500L) "1.333us";
  Test.string ~__POS__ (nps 1_332_500L) "-1.333us";
  Test.string ~__POS__ (pps 1_332_499L) "1.332us";
  Test.string ~__POS__ (nps 1_332_499L) "-1.332us";
  Test.string ~__POS__ (pps 1_036_000L) "1.036us";
  Test.string ~__POS__ (nps 1_036_000L) "-1.036us";
  Test.string ~__POS__ (pps 1_000_000L) "1us";
  Test.string ~__POS__ (nps 1_000_000L) "-1us";
  (* ns *)
  Test.string ~__POS__ (pps 999_500L) "1us";
  Test.string ~__POS__ (nps 999_500L) "-1us";
  Test.string ~__POS__ (pps 999_499L) "999ns";
  Test.string ~__POS__ (nps 999_499L) "-999ns";
  Test.string ~__POS__ (pps 1_995L) "1.995ns";
  Test.string ~__POS__ (nps 1_995L) "-1.995ns";
  Test.string ~__POS__ (pps 1_994L) "1.994ns";
  Test.string ~__POS__ (nps 1_994L) "-1.994ns";
  Test.string ~__POS__ (pps 1_332L) "1.332ns";
  Test.string ~__POS__ (nps 1_332L) "-1.332ns";
  Test.string ~__POS__ (pps 1_036L) "1.036ns";
  Test.string ~__POS__ (nps 1_036L) "-1.036ns";
  Test.string ~__POS__ (pps 1_000L) "1ns";
  Test.string ~__POS__ (nps 1_000L) "-1ns";
  (* ps *)
  Test.string ~__POS__ (pps 999L) "999ps";
  Test.string ~__POS__ (nps 999L) "-999ps";
  Test.string ~__POS__ (pps 50L) "50ps";
  Test.string ~__POS__ (nps 50L) "-50ps";
  Test.string ~__POS__ (pps 1L) "1ps";
  Test.string ~__POS__ (nps 1L) "-1ps";
  Test.string ~__POS__ (pps 0L) "0ps";
  Test.string ~__POS__ (nps 0L) "0ps";
  ()

let tests () =
  test_conversions ();
  test_predicates ();
  test_arithmetic ();
  test_rounding ();
  test_pretty_printing ();
  ()

(*---------------------------------------------------------------------------
   Copyright (c) 2015 The ptime programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_testing
open Testing_ptime

let span_of_d_ps ?__POS__ s = Ptime.Span.of_d_ps s |> Test.get_some ?__POS__

let test_base () =
  Test.test "stamp constants and base constructors" @@ fun () ->
  let of_span s = Ptime.(of_span (span_of_d_ps s)) in
  let get_of_span s = match of_span s with None -> assert false | Some s -> s in
  let to_raw_span t = Ptime.(Span.to_d_ps (to_span t)) in
  T.raw_span ~__POS__
    (to_raw_span Ptime.epoch) (0, 0L);
  T.stamp_option ~__POS__
    (of_span (0, 0L)) (Some Ptime.epoch);
  T.raw_span ~__POS__
    (to_raw_span Ptime.min) (-719528, 0L);
  T.stamp_option ~__POS__
    (of_span (-719528, 0L)) (Some Ptime.min);
  T.stamp_option ~__POS__
    (of_span (-719529, 86_399_999_999_999_999L)) None;
  T.raw_span ~__POS__
    (to_raw_span Ptime.max) (2932896, 86_399_999_999_999_999L);
  T.stamp_option ~__POS__
    (of_span (2932896, 86_399_999_999_999_999L)) (Some Ptime.max);
  T.stamp_option ~__POS__
    (of_span (2932897, 0L)) None;
  Test.float ~__POS__
    (Ptime.to_float_s Ptime.epoch) 0.;
  T.stamp_option ~__POS__
    (Ptime.of_float_s 0.) (Some Ptime.epoch);
  Test.float ~__POS__
    (Ptime.to_float_s Ptime.min) ~-.62167219200.;
  T.stamp_option ~__POS__
    (Ptime.of_float_s ~-.62167219200.) (Some Ptime.min);
  T.stamp_option ~__POS__
    (Ptime.of_float_s ~-.62167219201.) None;
  Test.float ~__POS__
    (Ptime.to_float_s (Ptime.truncate ~frac_s:0 Ptime.max)) 253402300799.;
  T.stamp_option ~__POS__
    (Ptime.of_float_s 253402300799.)
    (Some (Ptime.truncate ~frac_s:0 Ptime.max));
  T.stamp_option ~__POS__
    (Ptime.of_float_s 253402300800.) None;
  T.stamp_option ~__POS__
    (Ptime.of_float_s nan) None;
  T.stamp_option ~__POS__
    (Ptime.of_float_s infinity) None;
  T.stamp_option ~__POS__
    (Ptime.of_float_s ~-.infinity) None;
  T.raw_span ~__POS__
    Ptime.(Span.to_d_ps (frac_s Ptime.max)) (0, 999_999_999_999L);
  T.span ~__POS__
    (Ptime.frac_s @@ get_of_span (0, 100_000_000_000L))
    (span_of_d_ps (0, 100_000_000_000L));
  T.span ~__POS__
    (Ptime.frac_s @@ get_of_span (-1, 100_000_000_000L))
    (span_of_d_ps (0, 100_000_000_000L));
  ()

let test_predicates () =
  Test.test "stamp predicates" @@ fun () ->
  Test.bool ~__POS__ Ptime.(is_earlier min ~than:min) false;
  Test.bool ~__POS__ Ptime.(is_earlier min ~than:epoch) true;
  Test.bool ~__POS__ Ptime.(is_earlier min ~than:max) true;
  Test.bool ~__POS__ Ptime.(is_earlier epoch ~than:min) false;
  Test.bool ~__POS__ Ptime.(is_earlier epoch ~than:epoch) false;
  Test.bool ~__POS__ Ptime.(is_earlier epoch ~than:max) true;
  Test.bool ~__POS__ Ptime.(is_earlier max ~than:min) false;
  Test.bool ~__POS__ Ptime.(is_earlier max ~than:epoch) false;
  Test.bool ~__POS__ Ptime.(is_earlier max ~than:max) false;
  Test.bool ~__POS__ Ptime.(is_later min ~than:min) false;
  Test.bool ~__POS__ Ptime.(is_later min ~than:epoch) false;
  Test.bool ~__POS__ Ptime.(is_later min ~than:max) false;
  Test.bool ~__POS__ Ptime.(is_later epoch ~than:min) true;
  Test.bool ~__POS__ Ptime.(is_later epoch ~than:epoch) false;
  Test.bool ~__POS__ Ptime.(is_later epoch ~than:max) false;
  Test.bool ~__POS__ Ptime.(is_later max ~than:min) true;
  Test.bool ~__POS__ Ptime.(is_later max ~than:epoch) true;
  Test.bool ~__POS__ Ptime.(is_later max ~than:max) false;
  ()

let test_posix_arithmetic () =
  Test.test "stamp POSIX arithmetic" @@ fun () ->
  let span ps = span_of_d_ps (0, ps) in
  let nspan ps = Ptime.Span.(neg (span_of_d_ps (0, ps))) in
  (* Test limits *)
  T.stamp_option ~__POS__ Ptime.(add_span max (span 1L)) None;
  T.stamp_option ~__POS__ Ptime.(add_span min (nspan (1L))) None;
  T.stamp_option ~__POS__ Ptime.(sub_span min (span 1L)) None;
  T.stamp_option ~__POS__ Ptime.(sub_span max (nspan (1L))) None;
  (* Test arithmetic *)
  T.stamp_option ~__POS__
    (Ptime.of_span (span 10L)) Ptime.(add_span epoch (span 10L));
  T.stamp_option ~__POS__
    (Ptime.of_span (nspan 10L)) Ptime.(sub_span epoch (span 10L));
  T.stamp_option ~__POS__
    (Ptime.of_span (nspan (10L))) Ptime.(sub_span epoch (span 10L));
  Test.block @@ fun () ->
  let of_span ps =
    let s = span_of_d_ps (0, Int64.abs ps) in
    Ptime.of_span (if ps < 0L then Ptime.Span.neg s else s)
  in
  let get ?__POS__ s = of_span s |> Test.get_some ?__POS__ in
  let t0 = get ~__POS__ 20L in
  let t1 = get ~__POS__ 10L in
  let t2 = get ~__POS__ (-10L) in
  T.span ~__POS__ (Ptime.diff t0 t1) (span 10L);
  T.span ~__POS__ (Ptime.diff t1 t0) (nspan 10L);
  T.span ~__POS__ (Ptime.diff t2 t0) (nspan 30L);
  T.span ~__POS__ (Ptime.diff t0 t2) (span 30L);
  ()

let test_truncation () =
  Test.test "stamp truncation" @@ fun () ->
  let p ~frac_s t =
    let d1 = Ptime.(diff t min) |> Ptime.Span.truncate ~frac_s
    and d2 = Ptime.diff (Ptime.truncate ~frac_s t) Ptime.min in
    T.span ~__POS__ d1 d2
  in
  let t ~frac_s ps =
    p ~frac_s (Ptime.v (0, ps));
    p ~frac_s (Ptime.v (1, ps));
    p ~frac_s (Ptime.v (-1, ps));
    p ~frac_s (Ptime.v (2932896, ps));
    p ~frac_s (Ptime.v (-719528, ps));
  in
  for i = 0 to 12 do
    t ~frac_s:i 0L;
    t ~frac_s:i 86_399_999_999_999_999L;
    t ~frac_s:i 86_399_000_000_000_000L;
  done;
  ()

let tests () =
  test_base ();
  test_predicates ();
  test_posix_arithmetic ();
  test_truncation ();
  ()

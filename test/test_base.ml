(*---------------------------------------------------------------------------
   Copyright (c) 2015 The ptime programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Testing
open Testing_ptime

let span_of_d_ps s = match Ptime.Span.of_d_ps s with
| None -> invalid_arg ""
| Some s -> s

let base = test "Constants and base constructors" @@ fun () ->
  let of_span s = Ptime.(of_span (span_of_d_ps s)) in
  let get_of_span s = match of_span s with None -> assert false | Some s -> s in
  let to_raw_span t = Ptime.(Span.to_d_ps (to_span t)) in
  eq_raw_span (to_raw_span Ptime.epoch) (0, 0L);
  eq_stamp_opt (of_span (0, 0L)) (Some Ptime.epoch);
  eq_raw_span (to_raw_span Ptime.min) (-719528, 0L);
  eq_stamp_opt (of_span (-719528, 0L)) (Some Ptime.min);
  eq_stamp_opt (of_span (-719529, 86_399_999_999_999_999L)) None;
  eq_raw_span (to_raw_span Ptime.max) (2932896, 86_399_999_999_999_999L);
  eq_stamp_opt (of_span (2932896, 86_399_999_999_999_999L)) (Some Ptime.max);
  eq_stamp_opt (of_span (2932897, 0L)) None;
  eq_float (Ptime.to_float_s Ptime.epoch) 0.;
  eq_stamp_opt (Ptime.of_float_s 0.) (Some Ptime.epoch);
  eq_float (Ptime.to_float_s Ptime.min) ~-.62167219200.;
  eq_stamp_opt (Ptime.of_float_s ~-.62167219200.) (Some Ptime.min);
  eq_stamp_opt (Ptime.of_float_s ~-.62167219201.) None;
  eq_float (Ptime.to_float_s (Ptime.truncate ~frac_s:0 Ptime.max))
    253402300799.;
  eq_stamp_opt (Ptime.of_float_s 253402300799.)
    (Some (Ptime.truncate ~frac_s:0 Ptime.max));
  eq_stamp_opt (Ptime.of_float_s 253402300800.) None;
  eq_stamp_opt (Ptime.of_float_s nan) None;
  eq_stamp_opt (Ptime.of_float_s infinity) None;
  eq_stamp_opt (Ptime.of_float_s ~-.infinity) None;
  eq_raw_span Ptime.(Span.to_d_ps (frac_s Ptime.max)) (0, 999_999_999_999L);
  eq_span (Ptime.frac_s @@ get_of_span (0, 100_000_000_000L))
    (span_of_d_ps (0, 100_000_000_000L));
  eq_span (Ptime.frac_s @@ get_of_span (-1, 100_000_000_000L))
    (span_of_d_ps (0, 100_000_000_000L));
  ()

let predicates = test "Predicates" @@ fun () ->
  eq_bool Ptime.(is_earlier min ~than:min) false;
  eq_bool Ptime.(is_earlier min ~than:epoch) true;
  eq_bool Ptime.(is_earlier min ~than:max) true;
  eq_bool Ptime.(is_earlier epoch ~than:min) false;
  eq_bool Ptime.(is_earlier epoch ~than:epoch) false;
  eq_bool Ptime.(is_earlier epoch ~than:max) true;
  eq_bool Ptime.(is_earlier max ~than:min) false;
  eq_bool Ptime.(is_earlier max ~than:epoch) false;
  eq_bool Ptime.(is_earlier max ~than:max) false;
  eq_bool Ptime.(is_later min ~than:min) false;
  eq_bool Ptime.(is_later min ~than:epoch) false;
  eq_bool Ptime.(is_later min ~than:max) false;
  eq_bool Ptime.(is_later epoch ~than:min) true;
  eq_bool Ptime.(is_later epoch ~than:epoch) false;
  eq_bool Ptime.(is_later epoch ~than:max) false;
  eq_bool Ptime.(is_later max ~than:min) true;
  eq_bool Ptime.(is_later max ~than:epoch) true;
  eq_bool Ptime.(is_later max ~than:max) false;
  ()

let posix_arithmetic = test "POSIX arithmetic" @@ fun () ->
  let span ps = span_of_d_ps (0, ps) in
  let nspan ps = Ptime.Span.(neg (span_of_d_ps (0, ps))) in
  (* Test limits *)
  eq_stamp_opt Ptime.(add_span max (span 1L)) None;
  eq_stamp_opt Ptime.(add_span min (nspan (1L))) None;
  eq_stamp_opt Ptime.(sub_span min (span 1L)) None;
  eq_stamp_opt Ptime.(sub_span max (nspan (1L))) None;
  (* Test arithmetic *)
  eq_stamp_opt (Ptime.of_span (span 10L)) Ptime.(add_span epoch (span 10L));
  eq_stamp_opt (Ptime.of_span (nspan 10L)) Ptime.(sub_span epoch (span 10L));
  eq_stamp_opt (Ptime.of_span (nspan (10L))) Ptime.(sub_span epoch (span 10L));
  block @@ begin fun () ->
    let of_span ps =
      let s = span_of_d_ps (0, Int64.abs ps) in
      Ptime.of_span (if ps < 0L then Ptime.Span.neg s else s)
    in
    let get = of_span $ int64 @-> ret_get_option stamp in
    let t0 = get 20L in
    let t1 = get 10L in
    let t2 = get (-10L) in
    eq_span (Ptime.diff t0 t1) (span 10L);
    eq_span (Ptime.diff t1 t0) (nspan 10L);
    eq_span (Ptime.diff t2 t0) (nspan 30L);
    eq_span (Ptime.diff t0 t2) (span 30L);
  end

let truncation = test "Truncation" @@ fun () ->
  let p ~frac_s t =
    let d1 = Ptime.(diff t min) |> Ptime.Span.truncate ~frac_s
    and d2 = Ptime.diff (Ptime.truncate ~frac_s t) Ptime.min in
    eq_span d1 d2
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

let suite = suite "Ptime base tests"
    [ base;
      posix_arithmetic;
      predicates;
      truncation; ]

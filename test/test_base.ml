(*---------------------------------------------------------------------------
   Copyright (c) 2015 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

open Testing
open Testing_ptime

let base = test "Constants and base constructors" @@ fun () ->
  let of_span s = Ptime.(of_span Span.(unsafe_of_d_ps s)) in
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
  let span ps = Ptime.Span.unsafe_of_d_ps (0, ps) in
  let nspan ps = Ptime.Span.(neg (unsafe_of_d_ps (0, ps))) in
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
      let s = Ptime.Span.unsafe_of_d_ps (0, Int64.abs ps) in
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

let suite = suite "Ptime base tests"
    [ base;
      posix_arithmetic;
      predicates; ]

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

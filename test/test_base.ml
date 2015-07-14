(*---------------------------------------------------------------------------
   Copyright (c) 2015 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

open Testing
open Testing_ptime

let base =
  test "Constants and base constructors" @@ fun () ->
  eq_float Ptime.(Span.to_s (to_span epoch)) 0.;
  eq_float Ptime.(Span.to_s (to_span min)) ~-.62167219200.;
  eq_float Ptime.(Span.to_s (to_span max)) 253402300799.;
  eq_stamp_opt Ptime.(of_span (Span.of_s 0.)) (Some Ptime.epoch);
  eq_stamp_opt Ptime.(of_span (Span.of_s ~-.62167219200.)) (Some Ptime.min);
  eq_stamp_opt Ptime.(of_span (Span.of_s 253402300799.)) (Some Ptime.max);
  eq_stamp_opt Ptime.(of_span (Span.of_s (~-.62167219200. -. 0.1))) None;
  eq_stamp_opt Ptime.(of_span (Span.of_s (253402300799. +. 0.1))) None;
(* TODO  eq_stamp_opt Ptime.(of_span (Span.of_s nan)) None; *)
(* TODO  eq_stamp_opt Ptime.(of_span (Span.of_s infinity)) None; *)
(* TODO  eq_stamp_opt Ptime.(of_span (Span.of_s ~-.infinity)) None; *)
  ()

let predicates =
  test "Predicates" @@ fun () ->
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

let posix_arithmetic =
  test "POSIX arithmetic" @@ fun () ->
  let span = Ptime.Span.of_s in
  (* Test limits *)
  eq_stamp_opt Ptime.(add_span max (span 0.1)) None;
  eq_stamp_opt Ptime.(add_span min (span ~-.0.1)) None;
  eq_stamp_opt Ptime.(sub_span min (span 0.1)) None;
  eq_stamp_opt Ptime.(sub_span max (span ~-.0.1)) None;
  (* Test garbage TODO *)
(*   eq_stamp_opt Ptime.(add_span epoch (span nan)) None;
  eq_stamp_opt Ptime.(add_span epoch (span infinity)) None;
  eq_stamp_opt Ptime.(add_span epoch (span ~-.infinity)) None;
  eq_stamp_opt Ptime.(add_span epoch (span max_float)) None;
  eq_stamp_opt Ptime.(add_span epoch (span ~-.max_float)) None;
*)
  (* Test arithmetic *)
  eq_stamp_opt (Ptime.of_span (span 10.)) Ptime.(add_span epoch (span 10.));
  eq_stamp_opt (Ptime.of_span (span ~-.10.)) Ptime.(sub_span epoch (span 10.));
  eq_stamp_opt (Ptime.of_span (span ~-.10.)) Ptime.(sub_span epoch (span 10.));
  block @@ begin fun () ->
    let of_span s = Ptime.(of_span (Span.of_s s)) in
    let get = of_span $ float @-> ret_get_option raw_stamp in
    let t0 = get 20. in
    let t1 = get 10. in
    let t2 = get ~-.10. in
    eq_float Ptime.(Span.to_s (diff t0 t1)) 10.;
    eq_float Ptime.(Span.to_s (diff t1 t0)) ~-.10.;
    eq_float Ptime.(Span.to_s (diff t2 t0)) ~-.30.;
    eq_float Ptime.(Span.to_s (diff t0 t2)) 30.;
  end

let suite =
  suite "Ptime base tests" @@ fun () ->
  base ();
  posix_arithmetic ();
  predicates ();
  ()

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

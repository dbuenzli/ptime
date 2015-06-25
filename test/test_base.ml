(*---------------------------------------------------------------------------
   Copyright (c) 2015 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

open Testing
open Testing_ptime

let assumptions =
  test "Assumptions" @@ fun () ->
  let max_int_arith = 2. ** 53. in
  let max_internal = Ptime.(to_posix_s max -. to_posix_s min) *. 1e3 in
  eq_bool (max_internal < max_int_arith) true;
  ()

let base =
  test "Constants and base constructors" @@ fun () ->
  eq_float Ptime.(to_posix_s epoch) 0.;
  eq_float Ptime.(to_posix_s min) ~-.62167219200.;
  eq_float Ptime.(to_posix_s max) 253402300799.;
  eq_stamp_opt (Ptime.of_posix_s 0.) (Some Ptime.epoch);
  eq_stamp_opt (Ptime.of_posix_s ~-.62167219200.) (Some Ptime.min);
  eq_stamp_opt (Ptime.of_posix_s 253402300799.) (Some Ptime.max);
  eq_stamp_opt (Ptime.of_posix_s (~-.62167219200. -. 0.1)) None;
  eq_stamp_opt (Ptime.of_posix_s (253402300799. +. 0.1)) None;
  eq_stamp_opt (Ptime.of_posix_s nan) None;
  eq_stamp_opt (Ptime.of_posix_s infinity) None;
  eq_stamp_opt (Ptime.of_posix_s ~-.infinity) None;
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
  (* Test limits *)
  eq_stamp_opt Ptime.(add_posix_s max 0.1) None;
  eq_stamp_opt Ptime.(add_posix_s min ~-.0.1) None;
  eq_stamp_opt Ptime.(sub_posix_s min 0.1) None;
  eq_stamp_opt Ptime.(sub_posix_s max ~-.0.1) None;
  (* Test garbage *)
  eq_stamp_opt Ptime.(add_posix_s epoch nan) None;
  eq_stamp_opt Ptime.(add_posix_s epoch infinity) None;
  eq_stamp_opt Ptime.(add_posix_s epoch ~-.infinity) None;
  eq_stamp_opt Ptime.(add_posix_s epoch max_float) None;
  eq_stamp_opt Ptime.(add_posix_s epoch ~-.max_float) None;
  (* Test arithmetic *)
  eq_stamp_opt (Ptime.of_posix_s 10.) Ptime.(add_posix_s epoch 10.);
  eq_stamp_opt (Ptime.of_posix_s ~-.10.) Ptime.(sub_posix_s epoch 10.);
  eq_stamp_opt (Ptime.of_posix_s ~-.10.) Ptime.(sub_posix_s epoch 10.);
  block @@ begin fun () ->
    let get = Ptime.of_posix_s $ float @-> ret_get_option raw_stamp in
    let t0 = get 20. in
    let t1 = get 10. in
    let t2 = get ~-.10. in
    eq_float Ptime.(diff_posix_s t0 t1) 10.;
    eq_float Ptime.(diff_posix_s t1 t0) ~-.10.;
    eq_float Ptime.(diff_posix_s t2 t0) ~-.30.;
    eq_float Ptime.(diff_posix_s t0 t2) 30.;
  end

let suite =
  suite "Ptime base tests" @@ fun () ->
  assumptions ();
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

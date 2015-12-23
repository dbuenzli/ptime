(*---------------------------------------------------------------------------
   Copyright (c) 2015 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

let str = Printf.sprintf

(* JavaScript values *)

let date = Js.Unsafe.variable "Date"
let date_now = Js.Unsafe.variable "Date.now"
let date_period = None (* Unknown *)

(* POSIX clock *)

let raw t = Ptime.(Span.to_d_ps (to_span t))
let (dmin, _) = raw Ptime.min
let (dmax, _) = raw Ptime.max

let err t =
  let err = str "Ptime_clock: can't represent JavaScript timestamp %f" t in
  raise (Sys_error err)

let now () =
  let (ms : float) = Js.Unsafe.fun_call date_now [||] in
  if ms <> ms (* nan *) then err ms else
  let days = int_of_float (floor (ms /. 86_400_000.)) in
  if days < dmin || days > dmax then err ms else
  let rem_ms = mod_float ms 86_400_000. in
  let rem_ms = if rem_ms < 0. then 86_400_000. +. rem_ms else rem_ms in
  if rem_ms >= 86_400_000. then
    let days = days + 1 in
    if days > dmax then err ms else
    Ptime.unsafe_of_d_ps (days, 0L)
  else
  let frac_ms, rem_ms = modf rem_ms in
  let rem_ps = Int64.(mul (of_float rem_ms) 1_000_000_000L) in
  let frac_ps = Int64.(of_float (frac_ms *. 1e9)) in
  Ptime.unsafe_of_d_ps (days, (Int64.add rem_ps frac_ps))

let period () = Ptime.Span.unsafe_of_d_ps_option date_period

(* System timezone offset *)

let current_tz_offset_s () =
  let d = Js.Unsafe.new_obj date [||] in
  Some ((Js.Unsafe.meth_call d "getTimezoneOffset" [||] : int) * (-60))

(* Raw interface *)

let now_d_ps () = raw (now ())
let period_d_ps () = date_period

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

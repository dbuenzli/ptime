(*---------------------------------------------------------------------------
   Copyright (c) 2015 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

open Testing

let unit ppf () = Format.fprintf ppf "()"

(* Timestamps *)

let float ppf f = Format.fprintf ppf "%.10f" f
let raw_stamp ppf t = float ppf (Ptime.to_posix_s t)

let eq_stamp = eq ~eq:Ptime.equal ~pp:raw_stamp
let eq_stamp_opt = eq_option ~eq:Ptime.equal ~pp:raw_stamp

(* Dates *)

let raw_date ppf (y,m,d) = Format.fprintf ppf "(%d, %d, %d)" y m d
let eq_date = eq ~eq:(=) ~pp:raw_date

(* Times *)

let raw_time ppf (_, ((hh, mm, ss), tz)) =
  Format.fprintf ppf "(%d, %d, %d), %d" hh mm ss tz

(* Date times *)

let raw_date_time ppf ((y, m, d), ((hh, mm, ss), tz)) =
  Format.fprintf ppf "(%d, %d, %d), ((%d, %d, %d), %d)"
    y m d hh mm ss tz

let eq_date_time v v' = eq ~eq:(=) ~pp:raw_date_time v v'
let eq_date_time_opt v v' = eq_option ~eq:(=) ~pp:raw_date_time v v'

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

(*---------------------------------------------------------------------------
   Copyright (c) 2015 The ptime programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Testing

let stdlib_float = float

let unit ppf () = Format.fprintf ppf "()"
let float ppf f = Format.fprintf ppf "%.10f" f
let int64 ppf i = Format.fprintf ppf "%Ld" i

(* Time spans *)

let raw_span ppf (d, ps) = Format.fprintf ppf "@[<1>(%d,@ %Ld)@]" d ps
let eq_raw_span = eq ~eq:(=) ~pp:raw_span

let span = Ptime.Span.dump
let eq_span = eq ~eq:Ptime.Span.equal ~pp:span
let eq_span_opt = eq_option ~eq:Ptime.Span.equal ~pp:span

(* Timestamps *)

let stamp = Ptime.dump
let eq_stamp = eq ~eq:Ptime.equal ~pp:stamp
let eq_stamp_opt = eq_option ~eq:Ptime.equal ~pp:stamp

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

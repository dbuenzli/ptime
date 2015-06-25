(*---------------------------------------------------------------------------
   Copyright (c) 2015 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

open Rresult

(* Errors *)

let strf = Format.asprintf

let err_sub ~pos ~len ~slen =
  strf "invalid substring pos:%d len:%d string length:%d" pos len slen

let err_frac_range min max f =
  strf "frac %d is not in range [%d;%d]" f min max

(* Julian day and proleptic Gregorian calendar date conversion.

   Formulae are from the calendar FAQ:
   http://www.tondering.dk/claus/cal/julperiod.php#formula

   These formulae work for positive Julian days. They represent
   Gegorian calendar BCE year `y` by `-(y-1)`, e.g. 2 BCE is -1, this
   follows the convention of ISO 8601.

   All timestamps in Ptime's [min;max] range are represented by
   positive Julian days and the formulae do not overflow on 32-bit
   platforms in this restricted range. *)

let jd_to_date jd =
  let a = jd + 32044 in
  let b = (4 * a + 3) / 146097 in
  let c = a - ((146097 * b) / 4) in
  let d = (4 * c + 3) / 1461 in
  let e = c - ((1461 * d) / 4) in
  let m = (5 * e + 2) / 153 in
  let day = e - ((153 * m + 2) / 5) + 1 in
  let month = m + 3 - (12 * (m / 10)) in
  let year = 100 * b + d - 4800 + (m / 10) in
  (year, month, day)

let jd_of_date (year, month, day) =
  let a = (14 - month) / 12 in
  let y = year + 4800 - a in
  let m = month + 12 * a - 3 in
  day + ((153 * m) + 2)/ 5 + 365 * y +
  (y / 4) - (y / 100) + (y / 400) - 32045

let jd_posix_epoch = 2_440_588          (* the Julian day of the POSIX epoch *)
let jd_ptime_min = 1_721_060                  (* the Julian day of Ptime.min *)

(* POSIX timestamps

   Internally we store them as milliseconds since the epoch using an
   IEEE double floating point value. All milliseconds on Ptime's
   [min;max] range are at least in [-2^53;2^53] and hence represented
   exactly. *)

type t = float (* milliseconds since the epoch *)

let epoch = 0.
let min = -62_167_219_200_000. (* 0000-01-01 00:00:00 UTC *)
let max = 253_402_300_799_000. (* 9999-12-31 23:59:59 UTC *)

let to_posix_ms t = t
let of_posix_ms t =
  if t < min || t > max || t <> t (* nan *) then None else Some t

let to_posix_s t = (to_posix_ms t) *. 1e-3
let of_posix_s t = of_posix_ms (t *. 1e3)

(* Predicates *)

let equal t0 t1 = t0 = t1
let compare t0 t1 = Pervasives.compare t0 t1
let is_earlier t ~than = compare t than = -1
let is_later t ~than = compare t than = 1

(* POSIX arithmetic *)

let add_posix_s t d = of_posix_ms (t +. d *. 1e3)
let sub_posix_s t d = of_posix_ms (t -. d *. 1e3)
let diff_posix_s t1 t0 = (t1 -. t0) *. 1e-3

(* Time zone offsets between local and UTC timelines *)

type tz_offset_s = int

(* Date-time conversion

   POSIX time counts seconds since 1970-01-01 00:00:00 UTC without
   counting leap seconds -- when a leap second occurs a POSIX second
   can be two SI seconds or zero SI second. Hence 86400 POSIX seconds
   always represent an UTC day and the translations below are accurate
   without having to refer to a leap seconds table. *)

type date = (int * int * int)
type time = (int * int * int) * tz_offset_s

let max_month_day =               (* max day number in a given year's month. *)
  let is_leap_year y = (y mod 4 = 0) && (y mod 100 <> 0 || y mod 400 = 0) in
  let mlen = [|31; 28 (* or not *); 31; 30; 31; 30; 31; 31; 30; 31; 30; 31|] in
  fun y m -> if (m = 2 && is_leap_year y) then 29 else mlen.(m - 1)

let is_date_valid (y, m, d) =
  0 <= y && y <= 9999 &&
  1 <= m && m <= 12 &&
  1 <= d && d <= max_month_day y m

let is_time_valid ((hh, mm, ss), _) =
  0 <= hh && hh <= 23 &&
  0 <= mm && mm <= 59 &&
  0 <= ss && ss <= 60

let of_date_time (date, ((hh, mm, ss), tz_offset_s as t)) =
  (* We first verify that the given date and time are Ptime-valid.
     Once this has been established we find find the number of Julian
     days since the epoch for the given proleptic Georgian calendar
     date. Multiplying this number by 86_400_000 gives us the
     millisecond precision POSIX timestamp for the first instant
     (00:00:00 UTC) of the day corresponding to the date. We then add
     the remaining time fields converted to milliseconds as needed and
     compensate the time zone offset. This final result is checked to
     be in the [min;max] range.

     By definition POSIX timestamps cannot represent leap seconds.
     With the code below any date-time with a seconds value of 60
     (leap second addition) is mapped to the POSIX timestamp that
     happens 1 second later which is what POSIX mktime would to. Any
     formally non-existing UTC date-time with a seconds value of 59
     (leap second subtraction) is mapped on the POSIX timestamp that
     represents this non existing instant. *)
  if not (is_date_valid date && is_time_valid t) then None else
  let days_since_epoch = jd_of_date date - jd_posix_epoch in
  of_posix_ms @@ (float days_since_epoch) *. 86_400_000. +.
                 (float hh) *. 3600_000. +.
                 (float mm) *. 60_000. +.
                 (float ss) *. 1000. -.
                 (float tz_offset_s) *. 1000.

let to_date_time ?(tz_offset_s = 0) t =
  (* We first floor the timestamp. This allows the resulting date-time
     for both positive and negative timestamps to show the second they
     exists in and provide a well behaved time line. Graphically:

        -2   -1    0    1    2
     ... +----+----+----+----+ ...
         [<---[<---[<---[<---[

     To render the timestamp in the given time zone offset we first
     express the timestamp in local time and then compute the date
     fields on that stamp as if it were UTC. If the local timestamp is
     not in [min;max] then its date fields cannot be valid according
     to the constraints guaranteed by Ptime and we fallback to UTC,
     i.e. a time zone offset of 0.

     We then apply the following algorithm whose description makes
     sense on a POSIX timestamp (i.e. UTC) but works equally well to
     render the date-time fields of a local timestamp.

     We first find the timestamp's number of Julian days since
     Ptime.min. In order not to have to deal with negative values on
     which mod_float would not have the desired behaviour we first
     bring the timestamp on a positive timescale by subtracting
     Ptime.min's value. Given the [min;max] range this gives us a
     magnitude that is still smaller than 2^53 (and hence precisely
     represented). Dividing this magnitude by 86_400_000 gives us the
     number of Julian days since Ptime.min which we can use to find
     the actual Julian day and use to convert it to a proleptic
     Gergorian calendar date. The remainder of this division is the
     number of remaining POSIX milliseconds in that day which defines
     the UTC daytime according to its various units.

     By definition no POSIX timestamp can represent a date-time with a
     seconds value of 60 (leap second addition) and thus the function
     will never return a date-time with such a value.  On the other
     hand it will return an inexisting UTC date-time with a seconds
     value of 59 whenever a leap second is subtracted since there is a
     POSIX timestamp that represents this instant. *)
  let t = floor t in
  let t, tz_offset_s = match add_posix_s t (float tz_offset_s) with
  | None -> t, 0
  | Some local -> local, tz_offset_s
  in
  let t = t -. min  (* bring the timestamps on positive numbers *) in
  let day_num = truncate (t /. 86_400_000.) in
  let jd = jd_ptime_min + day_num in
  let date = jd_to_date jd in
  let daytime_s = truncate ((mod_float t 86_400_000.)) / 1000 in
  let hh = (daytime_s / 3600) in
  let mm = (daytime_s mod 3600) / 60 in
  let ss = (daytime_s mod 60) in
  date, ((hh, mm, ss), tz_offset_s)

let of_date date = of_date_time (date, ((00, 00, 00), 0))
let to_date t = fst (to_date_time ~tz_offset_s:0 t)

(* RFC 3339 timestamp conversions *)

(* RFC 3339 timestamp parser *)

type error_range = int * int
type rfc3339_error =
  [ `Invalid_stamp | `Eoi | `Exp_chars of char list | `Trailing_input ]

let pp_rfc3339_error ppf = function
| `Invalid_stamp -> Format.fprintf ppf "@[invalid@ time@ stamp@]"
| `Eoi -> Format.fprintf ppf "@[unexpected@ end@ of@ input@]"
| `Trailing_input -> Format.fprintf ppf "@[trailing@ input@]"
| `Exp_chars cs ->
    let rec pp_chars ppf = function
    | c :: cs -> Format.fprintf ppf "@ %C" c; pp_chars ppf cs
    | [] -> ()
    in
    Format.fprintf ppf "@[expected@ a@ character@ in:%a@]" pp_chars cs

let rfc3339_error_to_msg = function
| Ok _ as v -> v
| Error (`RFC3339 ((s, e), err)) ->
    Error (`Msg (Format.asprintf "%d-%d: %a" s e pp_rfc3339_error err))

exception RFC3339 of (int * int) * rfc3339_error                  (* Internal *)

let error r e = raise (RFC3339 (r, e))
let error_pos p e = error (p, p) e
let error_exp_digit p =
  error_pos p (`Exp_chars ['0'; '1'; '2'; '3'; '4'; '5'; '6'; '7'; '8'; '9'])

let is_digit = function '0' .. '9' -> true | _ -> false

let parse_digits ~count pos max s =
  let stop = pos + count - 1 in
  if stop > max then error_pos max `Eoi else
  let rec loop k acc =
    if k > stop then acc else
    if is_digit s.[k] then loop (k+1) (acc * 10 + Char.code s.[k] - 0x30) else
    error_exp_digit k
  in
  loop pos 0

let parse_char c pos max s =
  if pos > max then error_pos max `Eoi else
  if s.[pos] = c then () else error_pos pos (`Exp_chars [c])

let parse_dt_sep ~strict pos max s =
  let is_dt_sep = function
  | 'T' -> true
  | 't' | ' ' when not strict -> true
  | _ -> false
  in
  if pos > max then error_pos max `Eoi else
  if is_dt_sep s.[pos] then () else
  error_pos pos (`Exp_chars (['T'] @ if strict then [] else ['t'; ' ']))

let decide_frac_or_tz ~strict pos max s =
  if pos > max then error_pos max `Eoi else
  match s.[pos] with
  | '.' -> `Frac
  | '+' | '-' | 'Z' -> `Tz
  | 'z' when not strict  -> `Tz
  | c ->
      let chars = ['.'; '+'; '-'; 'Z'] @ if strict then [] else ['z'] in
      error_pos pos (`Exp_chars chars)

let parse_frac_ms pos max s =
  if pos > max then error_pos max `Eoi else
  if not (is_digit s.[pos]) then error_exp_digit pos else
  let rec loop k acc =
    if k > max then error_pos max `Eoi else
    if not (is_digit s.[k]) then (Some acc), k else
    let count = k - pos in
    loop (k + 1) (acc +. (float (Char.code s.[k] - 0x30)) *.
                         10. ** (float (2 - count)))
  in
  loop pos 0.

let parse_tz_s ~strict pos max s =
  let parse_tz_mag sign pos =
    let hh_pos = pos in
    let mm_pos = hh_pos + 3 in
    let hh = parse_digits ~count:2 hh_pos max s in
    parse_char ':' (mm_pos - 1) max s;
    let mm = parse_digits ~count:2 mm_pos max s in
    if hh > 23 then error (hh_pos, hh_pos + 1) `Invalid_stamp else
    if mm > 59 then error (mm_pos, mm_pos + 1) `Invalid_stamp else
    sign * (hh * 3600 + mm * 60), mm_pos + 1
  in
  if pos > max then error_pos max `Eoi else
  match s.[pos] with
  | 'Z' -> 0, pos
  | 'z' when not strict -> 0, pos
  | '+' -> parse_tz_mag ( 1) (pos + 1)
  | '-' -> parse_tz_mag (-1) (pos + 1)
  | c ->
      let chars = ['+'; '-'; 'Z'] @ if strict then [] else ['z'] in
      error_pos pos (`Exp_chars chars)

let of_rfc3339 ?last ?(strict = false) ?(pos = 0) ?len s =
  try
    let slen = String.length s in
    let max = match len with None -> slen - 1 | Some l -> pos + l - 1 in
    if pos < 0 || max >= slen
    then invalid_arg (err_sub ~pos ~len:(max + 1) ~slen)
    else
    let y_pos = pos in
    let m_pos = y_pos + 5 in
    let d_pos = m_pos + 3 in
    let hh_pos = d_pos + 3 in
    let mm_pos = hh_pos + 3 in
    let ss_pos = mm_pos + 3 in
    let decide_pos = ss_pos + 2 in
    let y = parse_digits ~count:4 y_pos max s in
    parse_char '-' (m_pos - 1) max s;
    let m = parse_digits ~count:2 m_pos max s in
    parse_char '-' (d_pos - 1) max s;
    let d = parse_digits ~count:2 d_pos max s in
    parse_dt_sep ~strict (hh_pos - 1) max s;
    let hh =  parse_digits ~count:2 hh_pos max s in
    parse_char ':' (mm_pos - 1) max s;
    let mm =  parse_digits ~count:2 mm_pos max s in
    parse_char ':' (ss_pos - 1) max s;
    let ss = parse_digits ~count:2 ss_pos max s in
    let frac, tz_pos = match decide_frac_or_tz ~strict decide_pos max s with
    | `Frac -> parse_frac_ms (decide_pos + 1) max s
    | `Tz -> None, decide_pos
    in
    let tz_s, last_pos = parse_tz_s ~strict tz_pos max s in
    match of_date_time ((y, m, d), ((hh, mm, ss), tz_s)) with
    | None -> error (pos, last_pos) `Invalid_stamp
    | Some t ->
        let r = match frac with
        | None | Some 0. -> t, tz_s
        | Some f ->
            match of_posix_ms (t +. f) with
            | None -> error (pos, last_pos) `Invalid_stamp
            | Some t -> t, tz_s
        in
        begin match last with
        | None ->
            if last_pos = max then Ok r else
            error_pos (last_pos + 1) `Trailing_input
        | Some last ->
            last := last_pos; Ok r
        end
  with RFC3339 (r, e) -> Error (`RFC3339 (r, e))

(* RFC 3339 timestamp formatter *)

let rfc3339_adjust_tz_offset tz_offset_s =
  (* The RFC 3339 time zone offset field is limited in expression to
     the bounds below with minute precision. If the requested time
     zone offset exceeds these bounds or is not an *integral* number
     of minutes we simply use UTC. An alternative would be to
     compensate the offset *and* the timestamp but it's more
     complicated to explain and maybe more surprising to the user. *)
  let min = -86340 (* -23h59 in secs *) in
  let max = +86340 (* +23h59 in secs *) in
  if min <= tz_offset_s && tz_offset_s <= max && tz_offset_s mod 60 = 0
  then tz_offset_s
  else 0 (* UTC *)

let s_frac_of_ms frac t =      (* [frac]ths fractional second digits of [t] *)
  (* We take the absolute value of the timestamp modulo 1000 which
     gives us the number milliseconds precision digits represented in
     [t]. For positive numbers this is the number of subsecond
     precision milliseconds in the timestamp. For negative numbers
     this is the remaining number of subsecond precision milliseconds
     until the next timestamp (see the timeline image in
     [to_date_time]'s comment) and hence we subtract that from 1000 to
     get the number milliseconds in the timestamp (e.g. the timestamp
     -200 represents the 800's millisecond of the second before the
     epoch). We then multiply the result by the power of 10 that
     brings the precision on the integral part of a floating point
     value and truncate the result. The [0;9] frac range ensures that
     the resulting integer doesn't overflow on 32-bit platforms. *)
  if frac < 0 || frac > 9 then invalid_arg (err_frac_range 0 9 frac) else
  let fpart = mod_float (abs_float t) 1000. in
  if fpart = 0. then 0 else
  let fpart = if t < 0. then 1000. -. fpart else fpart in
  let mul = 10. ** (float (frac - 3)) in
  truncate (fpart *. mul)

let to_rfc3339 ?(space = false) ?(frac = 0) ?(tz_offset_s = 0) t =
  let buf = Buffer.create 255 in
  let tz_offset_s = rfc3339_adjust_tz_offset tz_offset_s in
  let (y, m, d), ((hh, ss, mm), tz_offset_s) = to_date_time ~tz_offset_s t in
  let dt_sep = if space then ' ' else 'T' in
  Printf.bprintf buf "%04d-%02d-%02d%c%02d:%02d:%02d" y m d dt_sep hh ss mm;
  if frac <> 0 then Printf.bprintf buf ".%0*d" frac (s_frac_of_ms frac t);
  if tz_offset_s = 0 then Printf.bprintf buf "Z" else
  begin
    let tz_sign = if tz_offset_s < 0 then '-' else '+' in
    let tz_min = abs (tz_offset_s / 60) in
    let tz_hh = tz_min / 60 in
    let tz_mm = tz_min mod 60 in
    Printf.bprintf buf "%c%02d:%02d" tz_sign tz_hh tz_mm;
  end;
  Buffer.contents buf

let pp_rfc3339 ?space ?frac ?tz_offset_s () ppf t =
  Format.fprintf ppf "%s" (to_rfc3339 ?space ?frac ?tz_offset_s t)

(* Pretty printing *)

let pp ?(frac = 0) ?(tz_offset_s = 0) () ppf t =
  let tz_offset_s = rfc3339_adjust_tz_offset tz_offset_s in
  let (y, m, d), ((hh, ss, mm), tz_offset_s) = to_date_time ~tz_offset_s t in
  Format.fprintf ppf "%04d-%02d-%02d %02d:%02d:%02d" y m d hh ss mm;
  if frac <> 0 then Format.fprintf ppf ".%0*d" frac (s_frac_of_ms frac t);
  let tz_sign = if tz_offset_s < 0 then '-' else '+' in
  let tz_min = abs (tz_offset_s / 60) in
  let tz_hh = tz_min / 60 in
  let tz_mm = tz_min mod 60 in
  Format.fprintf ppf " %c%02d:%02d" tz_sign tz_hh tz_mm;
  ()

let pp_top = pp ()

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

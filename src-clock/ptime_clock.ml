(*---------------------------------------------------------------------------
   Copyright (c) 2015 The ptime programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Stubs *)

external ptime_clock_now_d_ps : unit -> int * int64 =
  "ocaml_ptime_clock_now_d_ps"

external ptime_clock_period_d_ps : unit -> (int * int64) option =
  "ocaml_ptime_clock_period_d_ps"

external ptime_clock_current_tz_offset_s : unit -> int option =
  "ocaml_ptime_clock_current_tz_offset_s"

(* POSIX clock *)

let now () = Ptime.unsafe_of_d_ps (ptime_clock_now_d_ps ())
let period () = Ptime.Span.unsafe_of_d_ps_option (ptime_clock_period_d_ps ())

(* System time zone offset *)

let current_tz_offset_s = ptime_clock_current_tz_offset_s

(* Raw interface *)

let now_d_ps = ptime_clock_now_d_ps
let period_d_ps = ptime_clock_period_d_ps

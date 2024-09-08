(*---------------------------------------------------------------------------
   Copyright (c) 2015 The ptime programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Ptime test unit comonalities *)

open B0_std
open B0_testing

module T = struct

  (* Time spans *)

  let eq_raw_span =
    let raw_span ppf (d, ps) = Fmt.pf ppf "@[<1>(%d,@ %Ld)@]" d ps in
    Test.Eq.make ~pp:raw_span ()

  let raw_span ?__POS__ = Test.eq ?__POS__ eq_raw_span

  let eq_span = Test.Eq.make ~equal:Ptime.Span.equal ~pp:Ptime.Span.dump ()
  let span ?__POS__ = Test.eq ?__POS__ eq_span
  let span_option ?__POS__ = Test.option ?__POS__ ~some:eq_span

  (* Timestamps *)

  let eq_stamp = Test.Eq.make ~equal:Ptime.equal ~pp:Ptime.dump ()
  let stamp ?__POS__ = Test.eq ?__POS__ eq_stamp
  let stamp_option ?__POS__ = Test.option ?__POS__ ~some:eq_stamp

  (* Dates *)

  module Date = struct
    type t = Ptime.date
    let equal = ( = )
    let pp ppf (y,m,d) = Fmt.pf ppf "(%d, %d, %d)" y m d
  end

  let date ?__POS__ = Test.eq ?__POS__ (module Date)

  (* Date time  *)

  module Date_time = struct
    type t = Ptime.date * Ptime.time
    let equal = ( = )
    let pp ppf ((y, m, d), ((hh, mm, ss), tz)) =
      Fmt.pf ppf "(%d, %d, %d), ((%d, %d, %d), %d)" y m d hh mm ss tz
  end

  let date_time ?__POS__ = Test.eq ?__POS__ (module Date_time)

  let gmtime_to_date_time t =
    let t = Ptime.to_float_s t in
    let t = floor t (* see https://github.com/ocaml/ocaml/issues/6921 *) in
    let tm = Unix.gmtime t in
    let d = (tm.Unix.tm_year + 1900), (tm.Unix.tm_mon + 1), (tm.Unix.tm_mday) in
    let t = tm.Unix.tm_hour, tm.Unix.tm_min, tm.Unix.tm_sec in
    (d, (t, 0)), tm.Unix.tm_wday

  let date_time_gmtime_witness ?__POS__:pos t =
    let fail ?__POS__ n ~checks:_ =
      Test.log_fail ?__POS__ "On stamp %g" (Ptime.to_float_s t)
    in
    Test.block ?__POS__:pos ~fail @@ fun () ->
    let dt, wday = gmtime_to_date_time t in
    let ut = Ptime.to_date_time t in
    Test.eq ~__POS__ (module Date_time) dt ut;
    Test.int ~__POS__ (Ptime.weekday_num t) wday
end

module Rand = struct

  (* Random loop length *)

  let loop_len = ref 100_000
  let loop_len () = !loop_len

  (* Random Ptime-valid stamps from floats *)

  let float_stamp_range min max =
    let bound = max -. min in
    fun () ->
      let r = Random.State.float (Test.Rand.state ()) bound (* inclusive *) in
      let stamp = min +. r in
      match Ptime.(of_float_s stamp) with
      | None -> Fmt.failwith "cannot convert valid random stamp %f" stamp
      | Some t -> t

  let float_stamp_32bits =
    let min_stamp = Int32.(to_float min_int) in
    let max_stamp = Int32.(to_float max_int) in
    float_stamp_range min_stamp max_stamp

  let float_stamp : unit -> Ptime.t =
    let min_stamp = Ptime.(to_float_s min) in
    let max_stamp = Ptime.(to_float_s max) in
    float_stamp_range min_stamp max_stamp

  let stamp =
    if Sys.word_size = 32 then float_stamp_32bits else float_stamp

  (* Random Ptime-valid dates *)

  let date : unit -> (int * int * int) =
    let month_len = [|31; 28; 31; 30; 31; 30; 31; 31; 30; 31; 30; 31 |] in
    let is_leap y = (y mod 4 = 0) && (y mod 100 <> 0 || y mod 400 = 0) in
    fun () ->
      let rstate = Test.Rand.state () in
      let rint bound = Random.State.int rstate bound in
      let y = rint 10_000 in
      let m = 1 + rint 11 in
      let m_len = if (m = 2 && is_leap y) then 29 else month_len.(m - 1) in
      let d = 1 + rint m_len in
      (y, m, d)

  (* Random times *)

  let tz_interval_s = (1 lsl 30 - 1) (* max of Random.int *)
  let tz_offset_s : unit -> int =
    fun () ->
    let rstate = Test.Rand.state () in
    (* N.B. We don't cover the whole spectrum *)
    (Random.State.int rstate tz_interval_s) - (tz_interval_s / 2)

  let min_tz_interval_s = 2000
  let min_tz_offset_s : unit -> int =
    fun () ->
    let rstate = Test.Rand.state () in
    ((Random.State.int rstate min_tz_interval_s) - (min_tz_interval_s / 2)) * 60

  let time : unit -> (int * int * int) =
    fun () ->
    let rstate = Test.Rand.state () in
    let rint bound = Random.State.int rstate bound in
    let hh = rint 24 in
    let mm = rint 60 in
    let ss = rint 61 in
    (hh, mm, ss)
end

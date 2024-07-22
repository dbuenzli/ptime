/*---------------------------------------------------------------------------
   Copyright (c) 2022 The ptime programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*/

//Provides: ocaml_ptime_clock_period_d_ps
function ocaml_ptime_clock_period_d_ps (_unit) {
  return 0;
}

//Provides: ocaml_ptime_clock_current_tz_offset_s
function ocaml_ptime_clock_current_tz_offset_s (_unit) {
  return [0, ((new Date ()).getTimezoneOffset () * -60)]
}

//Provides: ocaml_ptime_clock_now_d_ps
//Requires: caml_int64_of_int32, caml_int64_of_float
//Requires: caml_int64_add, caml_int64_mul
//Requires: caml_modf_float
//Requires: caml_raise_sys_error
function ocaml_ptime_clock_now_d_ps (_unit) {
  function err (ms) {
    caml_raise_sys_error
    ("Ptime_clock: can't represent JavaScript timestamp " + ms);
  }
  var dmin = -719528; /* Day component of Ptime.min */
  var dmax = 2932896; /* Day component of Ptime.max */
  var ms = Date.now ();
  var ps;
  if (ms != ms) err (ms)
  var days = Math.floor (ms / 86400000);
  if (days < dmin || days > dmax) err(ms);
  var rem_ms = ms % 86400000;
  if (rem_ms < 0) rem_ms += 86400000
  if (rem_ms >= 86400000) {
    /* Guard against a potential overflow in the computation of [rem_s] */
    days += 1;
    if (days > dmax) err (ms);
    ps = caml_int64_of_int32 (0);
  }
  else {
    var modf = caml_modf_float (rem_ms);
    var fract_ps = caml_int64_of_float (modf[1] * 1e9);
    var rem_ps = caml_int64_mul (caml_int64_of_float (modf[2]),
                                 caml_int64_of_int32 (1000000000));
    ps = caml_int64_add (rem_ps, fract_ps);
  }
  return [0, days, ps]
}

/*---------------------------------------------------------------------------
   Copyright (c) 2015 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
   --------------------------------------------------------------------------*/

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/fail.h>

#define Val_none Val_int(0)

/* Detect platform */

#if defined(__APPLE__) && defined(__MACH__)
  #define OCAML_PTIME_DARWIN
  #include <time.h>
  #include <sys/time.h>

#elif defined (__unix__) || defined(__unix)
 #include <unistd.h>
 #if defined(_POSIX_VERSION)
   #define OCAML_PTIME_POSIX
   #include <time.h>
 #endif

#else
  #warning OCaml Ptime_clock module: unsupported platform
  #define OCAML_PTIME_UNSUPPORTED
#endif

/* Clock now */

#if defined(OCAML_PTIME_POSIX)

CAMLprim value ocaml_ptime_clock_now_d_ps (value unit)
{
  CAMLparam1 (unit);
  CAMLlocal1 (pair);
  struct timespec now;

  if (clock_gettime (CLOCK_REALTIME, &now))
    caml_raise_sys_error (caml_copy_string
                          ("Ptime_clock: can't determine current time"));

  pair = caml_alloc (2, 0);
  Store_field (pair, 0, Val_int (now.tv_sec / 86400));
  Store_field (pair, 1,
               caml_copy_int64 ((now.tv_sec % 86400) * 1000000000000L +
                                (now.tv_nsec * 1000L)));
  CAMLreturn (pair);
}

#elif defined(OCAML_PTIME_DARWIN)

CAMLprim value ocaml_ptime_clock_now_d_ps (value unit)
{
  CAMLparam1 (unit);
  CAMLlocal1 (pair);
  struct timeval now;

  gettimeofday(&now, NULL);

  pair = caml_alloc (2, 0);
  Store_field (pair, 0, Val_int (now.tv_sec / 86400));
  Store_field (pair, 1,
               caml_copy_int64 ((now.tv_sec % 86400) * 1000000000000L +
                                (now.tv_usec * 1000000L)));
  CAMLreturn (pair);
}

#else

CAMLprim value ocaml_ptime_clock_now_d_ps (value unit)
{
  caml_raise_sys_error (caml_copy_string
                        ("Ptime_clock: unsupported platform"));
}

#endif

/* Clock period */

#if defined(OCAML_PTIME_POSIX)

CAMLprim value ocaml_ptime_clock_period_d_ps (value unit)
{
  CAMLparam1 (unit);
  CAMLlocal2 (some, pair);
  struct timespec res;
  if (clock_getres (CLOCK_REALTIME, &res)) CAMLreturn (Val_none);
  some = caml_alloc (1, 0);
  pair = caml_alloc (2, 0);
  Store_field (some, 0, pair);
  Store_field (pair, 0, Val_int (res.tv_sec / 86400));
  Store_field (pair, 1,
               caml_copy_int64 ((res.tv_sec % 86400) * 1000000000000L +
                                (res.tv_nsec * 1000L)));
  CAMLreturn (some);
}

#else /* OCAML_PTIME_DARWIN && OCAML_PTIME_UNSUPPORTED */

CAMLprim value ocaml_ptime_clock_period_d_ps (value unit)
{ return Val_none; }

#endif


/* Timezone offset (local time - UTC time) */

#if defined(OCAML_PTIME_DARWIN) || defined (OCAML_PTIME_POSIX)

CAMLprim value ocaml_ptime_clock_current_tz_offset_s (value unit)
{
  struct tm *tm;

  time_t now_utc = time (NULL);
  if (now_utc == (time_t)-1) return Val_none;

  tm = localtime (&now_utc);
  if (tm == NULL) return Val_none;
  struct tm local = *tm;

  tm = gmtime (&now_utc);
  if (tm == NULL) return Val_none;
  struct tm utc = *tm;

  int dd = local.tm_yday - utc.tm_yday;
  int dh = local.tm_hour - utc.tm_hour;
  int dm = dh * 60 + (local.tm_min - utc.tm_min);
  dm = (dd ==  1 || dd < -1 /* year wrap */) ? dm + (24 * 60) :
       (dd == -1 || dd >  1 /* year wrap */) ? dm - (24 * 60) :
       dm /* same day */;

  value some = caml_alloc (1, 0);
  Store_field (some, 0, Val_int (dm * 60));
  return some;
}

#else /* OCAML_PTIME_UNSUPPORTED */

CAMLprim value ocaml_ptime_clock_current_tz_offset_s (value unit)
{ return Val_none; }

#endif

/*---------------------------------------------------------------------------
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
   --------------------------------------------------------------------------*/

(*---------------------------------------------------------------------------
   Copyright (c) 2015 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

(** POSIX time clock.

    [Ptime_clock] provides access to a system POSIX time clock and to
    the system's current time zone offset.

    This time does not increase monotically and is subject to system
    calendar time adjustments. Use {!Mtime} if you need monotonic
    wall-clock time to measure time spans.

    Consult important information about {{!err}error handling}
    and {{!platform_support}platform support}.

    {e Release %%VERSION%% - %%MAINTAINER%% } *)

(** {1:clock POSIX clock} *)

val now : unit -> Ptime.t
(** [now ()] is the current POSIX time, by definition always on the
    UTC timeline.

    @raise Sys_error see {{!err}error handling}. *)

val period : unit -> Ptime.span option
(** [period ()] is a positive POSIX time span representing
    the clock's period (if available). *)

(** {1:tz System time zone offset} *)

val current_tz_offset_s : unit -> Ptime.tz_offset_s option
(** [current_tz_offset_s ()] is the system's current local time zone
    offset to UTC in seconds, if known. This is the duration local
    time - UTC time in seconds. *)

(** {1:raw POSIX clock raw interface} *)

val now_d_ps : unit -> int * int64
(** [now_d_ps ()] is [(d, ps)] representing POSIX time occuring at
    [d] * 86'400e12 + [ps] POSIX picoseconds from the epoch
    1970-01-01 00:00:00 UTC. [ps] is in the range
    \[[0];[86_399_999_999_999_999L]\].

    @raise Sys_error see {{!err}error handling} *)

val period_d_ps : unit -> (int * int64) option
(** [period_d_ps ()] is if available [Some (d, ps)] representing the
    clock's picosecond period [d] * 86'400e12 + [ps]. [ps] is in the
    range \[[0];[86_399_999_999_999_999L]\]. *)

(** {1:err Error handling}

    The functions {!now} and {!now_d_ps} raise [Sys_error] whenever
    they can't determine the current time or that it doesn't fit in
    [Ptime]'s well-defined {{!Ptime.t}range}. This exception should
    only be catched at the toplevel of your program to log it and
    abort the program. It indicates a serious error condition in the
    system.

    All the other functions, whose functionality is less essential,
    simply silently return [None] if they can't determine the
    information either because it is unavailable or because an error
    occured.

    {1:platform_support Platform support}

    {ul
    {- Platforms with a POSIX clock (includes Linux) use
       {{:http://pubs.opengroup.org/onlinepubs/9699919799/functions/clock_gettime.html}clock_gettime} with [CLOCK_REALTIME].}
    {- On Darwin {{:http://pubs.opengroup.org/onlinepubs/9699919799/}
                  [gettimeofday]} is used.}
    {- On Windows this is currently unimplemented.}
    {- On JavaScript
       {{:http://www.ecma-international.org/ecma-262/6.0/index.html#sec-date.now}Date.now ()} and
       {{:http://www.ecma-international.org/ecma-262/6.0/index.html#sec-date.prototype.gettimezoneoffset}Date.prototype.getTimezoneOffset} are used.}} *)

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

(*---------------------------------------------------------------------------
   Copyright (c) 2015 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

open Testing
open Testing_ptime

let rand_stamp = Test_rand.(if Sys.word_size = 32 then stamp_32bits else stamp)
let value_of_posix_s =
  Ptime.of_posix_s $ pp_float @-> (ret_get_option raw_stamp)

let unix_to_date_time t =
  let t = Ptime.to_posix_s t in
  let t = floor t (* see http://caml.inria.fr/mantis/view.php?id=6921 *) in
  let tm = Unix.gmtime t in
  let d = (tm.Unix.tm_year + 1900), (tm.Unix.tm_mon + 1), (tm.Unix.tm_mday) in
  let t = tm.Unix.tm_hour, tm.Unix.tm_min, tm.Unix.tm_sec in
  (d, (t, 0))

let compare t =
  let dt = unix_to_date_time t in
  let ut = Ptime.to_date_time t in
  eq_date_time dt ut;
  dt

let compare = compare $ raw_stamp @-> ret raw_date_time

let stamp_to_date_time =
  Testing.test "Random Ptime-valid stamps to date-time" @@ fun () ->
  if Sys.word_size > 32 then begin
    ignore (compare Ptime.min);
    ignore (compare Ptime.max);
  end;
  for i = 1 to Test_rand.loop_len () do ignore (compare (rand_stamp ())) done;
  ()

let exhaustive_min_max =
  if Sys.word_size > 32
  then Ptime.(to_posix_s min), Ptime.(to_posix_s max)
  else Int32.(to_float min_int), Int32.(to_float max_int)

let exhaustive =
  Testing.test "Each second Ptime-valid stamps to date-time" @@ fun () ->
  let min, max = exhaustive_min_max in
  let rec loop t y =
    if t > max then () else
    let stamp = value_of_posix_s t in
    let y', _, _ = fst (compare (stamp)) in
    if y <> y' then log "year: %d" y';
    loop (t +. 1.0) y'
  in
  loop min (-min_int)

let exhaustive_test = ref false

let suite =
  Testing.suite "Ptime tests against Unix.gmtime" @@ fun () ->
  stamp_to_date_time ();
  if !exhaustive_test then exhaustive ();
  ()

let exhaustive =
  "-e", Arg.Set exhaustive_test,
  "Perform exhaustive tests on the whole Ptime range"

let run () =
  Test_rand.cmdline ~opts:[exhaustive] ();
  suite ();
  Testing.log_results ()

let () = if run () then exit 0 else exit 1

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

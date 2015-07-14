(*---------------------------------------------------------------------------
   Copyright (c) 2015 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

open Testing

let strf = Printf.sprintf

(* Random state *)

let rstate = ref None
let set_seed seed =
  log "[RANDOM] Using random seed %d" seed;
  rstate := Some (Random.State.make [| seed |]);
  ()

let rec get_state () = match !rstate with
| Some s -> s
| None -> (* auto-seed *)
    let s = Random.State.make_self_init () in (* auto seed *)
    set_seed (Random.State.bits s);
    get_state ()

(* Random Ptime-valid stamps from floats *)

let stamp_range min max =
  let bound = max -. min in
  fun () ->
    let r = Random.State.float (get_state ()) bound (* inclusive *) in
    let stamp = min +. r in
    match Ptime.(of_span (Span.of_s stamp)) with
    | None -> failwith (strf "cannot convert valid random stamp %f" stamp)
    | Some t -> t

let stamp_32bits =
  let min_stamp = Int32.(to_float min_int) in
  let max_stamp = Int32.(to_float max_int) in
  stamp_range min_stamp max_stamp

let stamp : unit -> Ptime.t =
  let min_stamp = Ptime.(Span.to_s (to_span min)) in
  let max_stamp = Ptime.(Span.to_s (to_span max)) in
  stamp_range min_stamp max_stamp

(* Random Ptime-valid dates *)

let date : unit -> (int * int * int) =
  let month_len = [|31; 28; 31; 30; 31; 30; 31; 31; 30; 31; 30; 31 |] in
  let is_leap y = (y mod 4 = 0) && (y mod 100 <> 0 || y mod 400 = 0) in
  fun () ->
    let rstate = get_state () in
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
    let rstate = get_state () in
    (* N.B. We don't cover the whole spectrum *)
    (Random.State.int rstate tz_interval_s) - (tz_interval_s / 2)

let min_tz_interval_s = 2000
let min_tz_offset_s : unit -> int =
  fun () ->
    let rstate = get_state () in
    ((Random.State.int rstate min_tz_interval_s) - (min_tz_interval_s / 2)) * 60

let time : unit -> (int * int * int) =
  fun () ->
    let rstate = get_state () in
    let rint bound = Random.State.int rstate bound in
    let hh = rint 24 in
    let mm = rint 60 in
    let ss = rint 61 in
    (hh, mm, ss)

(* Random loop length *)

let loop_len = ref 100_000
let set_loop_len len =
  log "[LOOP] Loop iterations: %d" len;
  loop_len := len

let loop_len () = !loop_len

(* Command line helpers *)

let seed_opt =
  "-seed", Arg.Int set_seed,
  "<nat> random seed to use (auto-seeded by default)."

let loop_len_opt =
  "-loop", Arg.Int set_loop_len,
  "<nat> iterations to perform in random run loop tests (defaults to 100_000)."

let cmdline ?(opts = []) () =
  let exec = Filename.basename Sys.executable_name in
  let usage = Printf.sprintf "Usage: %s <options>\nOptions:" exec in
  let anon _ = raise (Arg.Bad "positional arguments unsupported") in
  Arg.parse (opts @ [seed_opt; loop_len_opt]) anon usage;


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

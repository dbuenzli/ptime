(*---------------------------------------------------------------------------
   Copyright (c) 2015 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

(* Compile with:

     ocamlfind ocamlc \
       -package ptime.clock.jsoo -o min_clock_jsoo.byte min_clock_jsoo.ml
     js_of_ocaml min_clock_jsoo.byte

   Use the resulting JavaScript file with min_clock_jsoo.html *)

let setup_log () =
  let log = Dom_html.(createDiv document) in
  let add_entry s =
    let e = Dom_html.(createP document) in
    Js.Unsafe.set e "innerHTML" (Js.string s);
    Dom.appendChild log e;
  in
  Dom.appendChild (Js.Unsafe.get Dom_html.document "body") log;
  Sys_js.set_channel_flusher stdout add_entry;
  Sys_js.set_channel_flusher stderr add_entry;
  ()

let pp_period ppf = function
| None -> Format.fprintf ppf "unknown"
| Some p -> Ptime.Span.pp ppf p

let pp_tz ppf = function
| None -> Format.fprintf ppf "unknown"
| Some tz -> Format.fprintf ppf "%ds" tz

let main _ =
  setup_log ();
  let now = Ptime_clock.now () in
  let tz_offset_s = Ptime_clock.current_tz_offset_s () in
  let period = Ptime_clock.period () in
  Format.printf "Clock period: %a@." pp_period period;
  Format.printf "   TZ offset: %a@." pp_tz tz_offset_s;
  Format.printf "   Now UTC  : %a@." Ptime.pp now;
  Format.printf "   Now local: %a@." Ptime.(pp_human ?tz_offset_s ()) now;
  Js._false

let () = Js.Unsafe.set Dom_html.window "onload" (Dom_html.handler main)

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

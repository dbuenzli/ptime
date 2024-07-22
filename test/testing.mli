(*---------------------------------------------------------------------------
   Copyright (c) 2015 The ptime programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* {1 Value equality and pretty printing} *)

type 'a eq = 'a -> 'a -> bool
type 'a pp = Format.formatter -> 'a -> unit

(* {1 Pretty printers} *)

val pp_int : int pp
val pp_float : float pp
val pp_str : string pp
val pp_opt : pp_v:'a pp -> 'a option pp

(* {1 Logging} *)

val log_part : ('a, Format.formatter, unit) format -> 'a
val log : ?header:string -> ('a, Format.formatter, unit) format -> 'a
val log_results : unit -> bool

(* {1 Testing scopes} *)

type test
type suite

val block : (unit -> unit) -> unit
val test : string -> (unit -> unit) -> test
val suite : string -> test list -> suite

val run : suite list -> unit

(* {1 Passing and failing tests} *)

val pass : unit -> unit
val fail : ('a, Format.formatter, unit, unit) format4 -> 'a

(* {1 Checking values} *)

val eq : eq:'a eq -> pp:'a pp -> 'a -> 'a -> unit
val eq_str : string -> string -> unit
val eq_bool : bool -> bool -> unit
val eq_int : int -> int -> unit
val eq_int32 : int32 -> int32 -> unit
val eq_int64 : int64 -> int64 -> unit
val eq_float : float -> float -> unit
val eq_nan : float -> unit

val eq_option : eq:'a eq -> pp:'a pp -> 'a option -> 'a option -> unit
val eq_some : 'a option -> unit
val eq_none : pp:'a pp -> 'a option -> unit

(* {1 Tracing and checking function applications} *)

type app (* holds information about the application *)

val ( $ ) : 'a -> (app -> 'a -> 'b) -> 'b
val ( @-> ) : 'a pp -> (app -> 'b -> 'c) -> app -> ('a -> 'b) -> 'a -> 'c

val ret : 'a pp -> app -> 'a -> 'a
val ret_eq : eq:'a eq -> 'a pp -> 'a -> app -> 'a -> 'a
val ret_some : 'a pp -> app -> 'a option -> 'a option
val ret_none : 'a pp -> app -> 'a option -> 'a option
val ret_get_option : 'a pp -> app -> 'a option -> 'a

val ret_ok : ('a, 'b) result pp -> app -> ('a, 'b) result -> ('a, 'b) result
val ret_error : ('a, 'b) result pp -> app -> ('a, 'b) result -> ('a, 'b) result
val ret_get_result : ('a, 'b) result pp -> app -> ('a, 'b) result -> 'a


val app_invalid : pp:'b pp -> ('a -> 'b) -> 'a -> unit
val app_exn : pp:'b pp -> exn -> ('a -> 'b) -> 'a -> unit
val app_raises : pp:'b pp -> ('a -> 'b) -> 'a -> unit


(*s Functions to call at exit. *)

val add_at_exit : (unit -> unit) -> unit
val do_at_exit : unit -> unit

(*s Functions to call at reset. *)

val add_at_reset : (unit -> unit) -> unit
val do_at_reset : unit -> unit

(*s Verbose. *)

val set_verbose : int -> unit
val verbose : int -> ('a -> unit) -> 'a -> unit

(*s Timing functions. *)

val utime : ('a -> 'b) -> 'a -> 'b * float

val profile : string -> ('a -> 'b) -> ('a -> 'b)


(*s Functions to call at exit. *)

val add_at_exit : (unit -> unit) -> unit
val do_at_exit : unit -> unit

(*s Functions to call at reset. *)

val add_at_reset : (unit -> unit) -> unit
val do_at_reset : unit -> unit

    
(*s Verbose. *)

val set_verbose : int -> unit
val get_verbose : unit -> int
val verbose : int -> ('a -> unit) -> 'a -> unit

(*s Timing functions. *)

val utime : ('a -> 'b) -> 'a -> 'b * float

val profile : string -> ('a -> 'b) -> ('a -> 'b)

val pp_to_string : (Format.formatter -> 'a -> unit) -> 'a -> string

(*s Type for comparison. *)

type cmp = Less | Equal | Greater

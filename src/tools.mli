
(*s Comparison function similar to Pervasives.compare but not recursive. 
    The function to handle the non-generic cases (i.e. the recursive call
    on constructors' arguments) is provided as the first argument. *)

val gen_compare : ('a * 'a -> int) -> 'a -> 'a -> int

(*s Comparison of various types. *)

val compare_list : ('a -> 'a -> int) -> 'a list -> 'a list -> int
val compare_array : ('a -> 'a -> int) -> 'a array -> 'a array -> int

(*s Lexicographic comparison. *)



val lexico : (('a -> 'a -> int) * 'a * 'a) list -> int

val lexico2 : ('a -> 'a -> int) -> 'a -> 'a -> 
              ('b -> 'b -> int) -> 'b -> 'b -> int

val lexico3 : ('a -> 'a -> int) -> 'a -> 'a -> 
              ('b -> 'b -> int) -> 'b -> 'b ->
	      ('c -> 'c -> int) -> 'c -> 'c -> int

val lexico4 : ('a -> 'a -> int) -> 'a -> 'a -> 
              ('b -> 'b -> int) -> 'b -> 'b ->
	      ('c -> 'c -> int) -> 'c -> 'c ->
	      ('d -> 'd -> int) -> 'd -> 'd -> int	  

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

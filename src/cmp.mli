
(*s Comparison function similar to Pervasives.compare
    but not recursive.  The function to handle the
    non-generic cases (i.e. the recursive call on constructors' arguments)
    is provided as the first argument. *)

val generic : ('a * 'a -> int) -> 'a -> 'a -> int

(*s Comparison of various types. *)

val list : ('a -> 'a -> int) -> 'a list -> 'a list -> int
val array : ('a -> 'a -> int) -> 'a array -> 'a array -> int

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

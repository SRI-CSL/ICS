type 'a seq

val nil : 'a seq
val cons : 'a -> (unit -> 'a seq) -> 'a seq
val singleton : 'a -> 'a seq
val null : 'a seq -> bool
val hd : 'a seq -> 'a
val tl : 'a seq -> 'a seq
val take : int -> 'a seq -> 'a list
val (@) : 'a seq -> 'a seq -> 'a seq
val map : ('a -> 'b) -> 'a seq -> 'b seq
val concat : 'a seq seq -> 'a seq
val mapcan : ('a -> 'b seq) -> 'a seq -> 'b seq
val cycle : ((unit -> 'a seq) -> 'a seq) -> 'a seq

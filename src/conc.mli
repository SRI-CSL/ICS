
(*i*)
open Term
(*i*)

  
(*s Concatenation normal forms are representations for bitvectors
    of the form [b1 ++ ... ++ bn] where [++] denotes
    concatenation and the [bi] are basic bitvectors.
    Basic bitvectors are
       1. constant bitvectors [const(c)],
       2. extractions [sub(x,n,i,j)] of the [i]-th through the [j]-th bit
          of a term [x], or
       3. bitwise operations [ite(b1,b2,b3)], where [b1] is either a
          constant with not all bits zeros or ones, or an extraction;
          [b2] and [b3] are arbitrary basic bitvectors.
  *)

type t

val hom : (Bitv.t -> 'a)
            -> (term -> int -> int -> int -> 'a)
	     -> (int -> 'a -> 'a -> 'a -> 'a) -> t -> 'a list

val length : t -> int

val pp : t -> unit

val eps : t
val zero : int -> t
val one : int -> t
val const : Bitv.t -> t

val inj : int -> term -> t

val sub : t -> int -> int -> t
    
val (++) : t -> t -> t 
val conc : t list -> t

val ite : t -> t -> t -> t

val solve : t -> t -> (term * t) list


(*i*)
open Term
(*i*)

(*s The datatype [t] represents uninterpreted function symbols.
    These include also equality, arithmetic constraints, and
    nonlinear multiplication function symbols.  These function
    symbols are built using the constructors [uninterp],
    [equal], [cnstrnt], and [times], respectively. [eq f g]
    is used to test for equality of function symbols. 
    Given a term [t], [of_term t] computes the corresponding
    uninterpreted function symbol, if [t] is uninterpreted,
    and [None] otherwise. The type ['a map] represents finite
    functions with domain [t].
 *)

type t = tnode Hashcons.hashed
and tnode

val uninterp :  Term.t -> t        
val equal : unit -> t
val mult : unit -> t
val div : unit -> t
   
val of_term: Term.t -> t option
    
val pp : Format.formatter -> t -> unit

type funsym = t

module Map : sig
  type 'a t
  val empty : 'a t
  val add : funsym -> 'a -> 'a t -> 'a t
  val find : funsym -> 'a t -> 'a
  val remove : funsym -> 'a t -> 'a t
  val mem :  funsym -> 'a t -> bool
  val iter : (funsym -> 'a -> unit) -> 'a t -> unit
  val map : ('a -> 'b) -> 'a t -> 'b t
  val fold : (funsym -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
end

module Set : sig
  type t
  val empty : t
  val is_empty : t -> bool
  val mem : funsym -> t -> bool
  val add : funsym -> t -> t
  val singleton: funsym -> t
  val is_empty : t -> bool
  val remove : funsym -> t -> t
  val union : t -> t -> t
  val cardinal : t -> int
  val iter : (funsym -> unit) -> t -> unit
  val fold : (funsym -> 'a -> 'a) -> t -> 'a -> 'a
end



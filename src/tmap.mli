
(*i*)
open Term
(*i*)

type 'a t
  
val empty : 'a t
val add : term -> 'a -> 'a t -> 'a t
val find : term -> 'a t -> 'a
val remove : term -> 'a t -> 'a t
val mem :  term -> 'a t -> bool
val iter : (term -> 'a -> unit) -> 'a t -> unit
val map : ('a -> 'b) -> 'a t -> 'b t
val fold : (term -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
val choose : (term -> term -> bool) -> term t -> term * term

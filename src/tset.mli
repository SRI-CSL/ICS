
(*i*)
open Term
(*i*)

type t
val empty : t
val mem : term -> t -> bool
val add : term -> t -> t
val singleton: term -> t
val sub : t -> t -> bool
val is_empty : t -> bool
val remove : term -> t -> t
val union : t -> t -> t
val inter : t -> t -> t
val iter : (term -> unit) -> t -> unit
val iter2 : (term -> term -> unit) -> t -> unit
val fold : (term -> 'a -> 'a) -> t -> 'a -> 'a
val exists : (term -> bool) -> t -> bool
val filter : (term -> bool) -> t -> t
val to_list : t -> term list
val choose : (term -> bool) -> t -> term
val destructure : t -> term * t


(*s Sets of function symbols *)

type t

val empty : t
val mem : Funsym.t -> t -> bool
val add : Funsym.t -> t -> t
val singleton: Funsym.t -> t
val sub : t -> t -> bool
val is_empty : t -> bool
val remove : Funsym.t -> t -> t
val union : t -> t -> t
val inter : t -> t -> t
val iter : (Funsym.t -> unit) -> t -> unit
val fold : (Funsym.t -> 'a -> 'a) -> t -> 'a -> 'a

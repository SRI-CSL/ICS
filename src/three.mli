
type t = 
  | Yes
  | No
  | X

val is_sub : t -> t -> bool

val inter : t -> t -> t option

val union : t -> t -> t

val is_disjoint : t -> t -> bool


(*s Hash tables from the ocaml standard library, with additional features. *)

type ('a, 'b) t

val create : int -> ('a,'b) t
val clear : ('a, 'b) t -> unit
val add : ('a, 'b) t -> 'a -> 'b -> unit
val find : ('a, 'b) t -> 'a -> 'b
val find_all : ('a, 'b) t -> 'a -> 'b list
val mem :  ('a, 'b) t -> 'a -> bool
val remove : ('a, 'b) t -> 'a -> unit
val iter : ('a -> 'b -> unit) -> ('a, 'b) t -> unit
(***)
val stat : ('a, 'b) t -> unit

(*** Functorial interface *)

module type HashedType =
  sig
    type t
    val equal: t -> t -> bool
    val hash: t -> int
  end

module type S =
  sig
    type key
    type 'a t
    val create: int -> 'a t
    val clear: 'a t -> unit
    val add: 'a t -> key -> 'a -> unit
    val remove: 'a t -> key -> unit
    val find: 'a t -> key -> 'a
    val find_all: 'a t -> key -> 'a list
    val mem: 'a t -> key -> bool
    val iter: (key -> 'a -> unit) -> 'a t -> unit
    (***)
    val stat : 'a t -> unit
  end

module Make(H: HashedType): (S with type key = H.t)

val hash : 'a -> int

external hash_param : int -> int -> 'a -> int = "hash_univ_param" "noalloc"








(*s Hash tables for hash-consing. *)

type 'a hashed = { 
  hkey : int;
  tag : int;
  node : 'a }

(*s Generic part, using ocaml generic equality and hash function. *)

type 'a t

val create : int -> 'a t
val clear : 'a t -> unit
val hashcons : 'a t -> 'a -> 'a hashed
val iter : ('a hashed -> unit) -> 'a t -> unit
val stat : 'a t -> unit

(*s Functorial interface. *) 

module type HashedType =
  sig
    type t
    val equal : t -> t -> bool
    val hash : t -> int
  end

module type S =
  sig
    type key
    type t
    val create : int -> t
    val clear : t -> unit
    val hashcons : t -> key -> key hashed
    val iter : (key hashed -> unit) -> t -> unit
    val stat : t -> unit
  end

module Make(H : HashedType) : (S with type key = H.t)

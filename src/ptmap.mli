
(*s Maps implemented as Patricia trees. *)

open Hashcons

type ('a,'b) t

val empty : ('a,'b) t

val add : 'a hashed -> 'b -> ('a,'b) t -> ('a,'b) t

val find : 'a hashed -> ('a,'b) t -> 'b

val remove : 'a hashed -> ('a,'b) t -> ('a,'b) t

val mem :  'a hashed -> ('a,'b) t -> bool

val iter : ('a hashed -> 'b -> unit) -> ('a,'b) t -> unit

val map : ('b -> 'c) -> ('a,'b) t -> ('a,'c) t

val fold : ('a hashed -> 'b -> 'c -> 'c) -> ('a,'b) t -> 'c -> 'c





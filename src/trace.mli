val init : unit -> unit

type 'a pp = Format.formatter -> 'a -> unit   (* pretty-printer type *)

val call : int -> string -> 'a -> 'a pp -> unit

val exit : int -> string -> 'a -> 'a pp -> unit

val ok: int -> string -> unit

val exc : int -> string -> 'a -> 'a pp -> unit


(*i
 * ICS - Integrated Canonizer and Solver
 * Copyright (C) 2001-2004 SRI International
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the ICS license as published at www.icansolve.com
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * ICS License for more details.
 i*)

(*s Module [Name]: Datatype of names with a constant-time equality. *)

type t

val of_string : string -> t
val to_string : t -> string

val eq : t -> t -> bool

val cmp : t -> t -> int

val pp : Format.formatter -> t -> unit

val hash : t -> int

module Set : (Set.S with type elt = t)
module Map : (Map.S with type key = t)

val pp_map: (Format.formatter -> 'a -> unit)
               -> Format.formatter -> 'a Map.t -> unit



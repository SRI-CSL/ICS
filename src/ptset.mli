
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

(*s Sets implemented as Patricia trees. *)

(*i*)
open Hashcons
(*i*)

type 'a t

val empty : 'a t
val is_empty : 'a t -> bool
val mem : 'a hashed -> 'a t -> bool
val add : 'a hashed -> 'a t -> 'a t
val singleton : 'a hashed -> 'a t
val remove : 'a hashed -> 'a t -> 'a t
val union : 'a t -> 'a t -> 'a t
val cardinal : 'a t -> int
val iter : ('a hashed -> unit) -> 'a t -> unit
val fold : ('a hashed -> 'b -> 'b) -> 'a t -> 'b -> 'b
val map : ('a hashed -> 'b hashed) -> 'a t -> 'b t
val inter : 'a t -> 'a t -> 'a t
val exists : ('a hashed -> bool) -> 'a t -> bool
val for_all : ('a hashed -> bool) -> 'a t -> bool
val filter : ('a hashed -> bool) -> 'a t -> 'a t
val to_list : 'a t -> 'a hashed list
val sub : 'a t -> 'a t -> bool
val equal : 'a t -> 'a t -> bool
val pp : (Format.formatter -> 'a hashed -> unit) -> Format.formatter -> 'a t -> unit

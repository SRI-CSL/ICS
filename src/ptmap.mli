
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


(*s Maps implemented as Patricia trees. *)

(*i*)
open Hashcons
(*i*)

type ('a,'b) t

val empty : ('a,'b) t

val add : 'a hashed -> 'b -> ('a,'b) t -> ('a,'b) t

val find : 'a hashed -> ('a,'b) t -> 'b

val remove : 'a hashed -> ('a,'b) t -> ('a,'b) t

val mem :  'a hashed -> ('a,'b) t -> bool

val iter : ('a hashed -> 'b -> unit) -> ('a,'b) t -> unit

val map : ('b -> 'c) -> ('a,'b) t -> ('a,'c) t

val fold : ('a hashed -> 'b -> 'c -> 'c) -> ('a,'b) t -> 'c -> 'c




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

(*s The purpose of module [Trace] is to provide some rudimentary
  abstractions for a tracer. *)

type level = string

val reset : unit -> unit
val add : level -> unit
val remove : level -> unit
val get : unit -> level list

val call : level -> string -> 'a -> 'a Pretty.printer -> unit

val exit : level -> string -> 'a -> 'a Pretty.printer -> unit

val msg : level -> string -> 'a -> 'a Pretty.printer -> unit

val func : level -> string -> 'a Pretty.printer -> 'b Pretty.printer
             -> ('a -> 'b) -> 'a -> 'b

val proc : level -> string -> 'a Pretty.printer
             -> ('a -> unit) -> 'a -> unit


(*
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
 *)

(*s The purpose of module [Trace] is to provide some rudimentary
  abstractions for a tracer. *)

    (*s Before using this module, the trace must be initialized. *)
val init : unit -> unit

    (*s The type of pretty printers. *)
type 'a pp = Format.formatter -> 'a -> unit

    (*s [call n str a p] is typically placed at the beginning of
      a function with name [str]. If the current verbose level
      (see module Tools) is greater or equal to [n], then the
      name of the function together with the current arguments [a],
      printed by the pretty printer [p] are printed to standard output.
      As a side effect, the current intendation is increased by one. *)
val call : int -> string -> 'a -> 'a pp -> unit

    (*s [exit n str a p] is typically placed at the beginning of
      a function with name [str]. If the current verbose level
      (see module Tools) is greater or equal to [n], then the
      name of the function together with the result [a],
      printed using the procedure [p], are printed to standard output.
      As a side effect, the current intendation is decreased by one. *)
val exit : int -> string -> 'a -> 'a pp -> unit

    (*s [ok] behaves like exit in case there is no return argument. *)
val ok: int -> string -> unit

    (*s [exc n str a p] is used to indicate that an exception [str] has
      been raised, whenever [n] is greater than the current verbose level. *)
val exc : int -> string -> 'a -> 'a pp -> unit


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


(*s Module [Pretty]: Pretty-printing of basic data structures. *)

type 'a  printer = Format.formatter -> 'a -> unit

val list : 'a printer -> 'a list printer
val term : Term.t printer
val eqn : (Term.t * Term.t) printer
val cnstrnt : Interval.t printer
val tset : Term.terms printer
val tmap : 'a printer -> 'a Term.Map.t printer


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

(*s Module [Atom]: Simplification of connectives on atoms. *)


val conj : Term.t -> Term.t -> Term.t option

val disj : Term.t -> Term.t -> Term.t option

val neg : Term.t -> Term.t option

val neg_conj : Term.t -> Term.t -> Term.t option


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

(*s The module [Atom] implements constructors for atomic predicates. *)

val is_atom : Term.t -> bool

(*s Constructors for atoms. *)

val equal : Term.t * Term.t -> Term.t

val diseq : Term.t * Term.t -> Term.t

val cnstrnt : Term.cnstrnt -> Term.t -> Term.t


(*s Check for inconsistency of two atoms. *)

val inconsistent : Term.t -> Term.t -> bool

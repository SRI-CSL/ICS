
(*i
 * The contents of this file are subject to the ICS(TM) Community Research
 * License Version 1.0 (the ``License''); you may not use this file except in
 * compliance with the License. You may obtain a copy of the License at
 * http://www.icansolve.com/license.html.  Software distributed under the
 * License is distributed on an ``AS IS'' basis, WITHOUT WARRANTY OF ANY
 * KIND, either express or implied. See the License for the specific language
 * governing rights and limitations under the License.  The Licensed Software
 * is Copyright (c) SRI International 2001, 2002.  All rights reserved.
 * ``ICS'' is a trademark of SRI International, a California nonprofit public
 * benefit corporation.
 * 
 * Author: Harald Ruess
i*)

(*s Module [Symbolic]: Symbolic constraints are composed 
   of explicit constraints  and a list of pairs [(c,a)] which 
   represents the constraint [c] minus the constraint of term [a]. *)

type t

(*s Accessors. *)

val explicit : t -> Cnstrnt.t
val symbolic : t -> (Cnstrnt.t * Term.t) list

val destruct : t -> Cnstrnt.t * (Cnstrnt.t * Term.t) list

(*s Constructor. *)

val make : Cnstrnt.t * (Cnstrnt.t * Term.t) list -> t

(*s [is_explicit s] holds iff there are no symbolic constraints. *)

val is_explicit : t -> bool

(*s Injecting an explicit constraint. *)

val of_cnstrnt : Cnstrnt.t -> t

(*s [full] is the real-number constraint. *)

val full : t

(*s Pretty-printing symbolic constraints. *)

val pp : t Pretty.printer

(*s Status of symbolic constraint. Either empty, full, singleton or other. *)

val status : t -> Mpa.Q.t Status.t


(*s [occurs x s] holds if [x] occurs uninterpreted in [s]. *)

val occurs : Term.t -> t -> bool


(*s [meaning f s] computes an explicit constraint by instantiating
 all symbolic constraints in [s] using the context information [f]. *)

val meaning : (Term.t -> t) -> t -> Cnstrnt.t


(*s [cnstrnt f a] computes a fully explicit constraint from
 a symbolic constraint using the context information [f]; in
 case [f] raises [Not_found], [Cnstrnt.mk_real] is returned. *)

val cnstrnt : (Term.t -> t) -> Term.t -> Cnstrnt.t

(*s [replace x a s] replaces [x] in the symbolic part of constraints
 with [a] and propagates symbolic constraints which become explicit
 in the process of replacement by conjoining them to the explicit part. *)

val replace : Term.t -> Term.t -> t -> t

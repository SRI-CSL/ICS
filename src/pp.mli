
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

(*i*)
open Term
(*i*)

val is_interp : Term.t -> bool

(*s Module [Arr]: Nonlinear arithmetic. *)

val mk_one : Term.t      (* Different form [Arith.mk_one]! *)

val is_one : Term.t -> bool

val mk_mult : Term.t -> Term.t -> Term.t

val mk_multl : Term.t list -> Term.t

val mk_expt : int -> Term.t -> Term.t

val sigma : Sym.pprod -> Term.t list -> Term.t

val map: (Term.t -> Term.t) -> Term.t -> Term.t

val cmp : Term.t -> Term.t -> int

val min : Term.t -> Term.t -> Term.t
val max : Term.t -> Term.t -> Term.t

(*s Abstract interpretation in the domain of constraints. Given 
 a context [f], which associates uninterpreted subterms of [a]
 with constraints, [cnstrnt f a] recurses over the interpreted
 structure of [a] and accumulates constraints by calling [f] at
 uninterpreted positions and abstractly interpreting the 
 interpreted arithmetic operators in the domain of constraints. *)

val tau : (Term.t -> Cnstrnt.t) -> Sym.pprod -> Term.t list -> Cnstrnt.t

(*s [gcd pp qq] computes the greatest common divisor of the power products
  [pp] and [qq]. It returns a triple of power products [(p, q, g)] 
  such that [g] divides both [pp] and [qq], it is the largest such [g],
  and [mk_mult p pp] and [mk_mult q qq] are equal to [g].  *)

val gcd : Term.t -> Term.t -> Term.t * Term.t * Term.t

(*s Least common muliple [lcm pp qq] *)

val lcm : Term.t -> Term.t -> Term.t

(*s Divisibility test [div pp qq] returns largest [Some(mm)] such that
  [pp * mm = qq] and [None] if no such [mm] exists. *)

val div : Term.t * Term.t -> Term.t option


(*s [split pp] splits a power product [pp] into a pair [(nn,dd)] of
 a numerator [nn] and a denumerator [dd], such that [pp] equals
 [mk_div nn dd]. *)

val split : Term.t -> Term.t * Term.t

val numerator : Term.t -> Term.t
val denumerator : Term.t -> Term.t

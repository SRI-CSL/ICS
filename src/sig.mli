
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


(*s Module [Sig]: builtin simplification *)

val sub : Context.t -> Term.t -> Term.t -> Term.t
val multq : Context.t -> Mpa.Q.t -> Term.t -> Term.t
val div : Context.t -> Term.t * Term.t -> Term.t
val mult : Context.t -> Term.t * Term.t -> Term.t 
val multl : Context.t -> Term.t list -> Term.t
val expt : Context.t -> Term.t -> Term.t -> Term.t
val update : Context.t -> Term.t * Term.t * Term.t -> Term.t
val unsigned : Context.t -> Term.t -> Term.t

(* [i = j => select(update(a,i,x), j) = x]
   [i <> j => select(update(a,i,x),j) = select(a,j)] *)

val update : Context.t -> Term.t * Term.t * Term.t -> Term.t
val select : Context.t -> Term.t * Term.t -> Term.t


(*s [cancel s (a, b)] cancels a power products [a] and [b]. *)

val cancel : Context.t -> Term.t * Term.t -> Term.t * Term.t

(*s Applications. *)

val apply : Context.t -> Sym.range -> Term.t -> Term.t -> Term.t

val lambda : Context.t -> int -> Term.t -> Term.t

(*s Sigma-normal forms. *)

val sigma : Context.t -> Sym.builtin -> Term.t list -> Term.t

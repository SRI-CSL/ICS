
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

(*s Module [D]: Context for handling disequalities. *)

type t


(*s Return disequalities as bindings of the form [x |-> {y1,...,yn}].
 The interpretation of such a binding is the conjunction 
 [x <> y1 & ... & x <> yn] of all known disequalities for [x]. The
 bindings returned by [deq] are closed in that forall [x], [y] 
 such that [x |-> {...,y,...} ] then also [y |-> {....,x,....}] *)

val deq_of : t -> Term.Set.t Term.Map.t

(*s [deq s a] is just returns the binding for [a] in [deq_of s]. *)

val deq : t -> Term.t -> Term.Set.t

(*s check if two terms are known to be disequal. *)

val is_diseq: t -> Term.t -> Term.t -> bool

(*s The empty disequality context. *)

val empty : t


(*s [merge f (a,b) s] merges an equality [a = b] over uninterpreted 
  terms [a], [b] and calls [f] on each derived equality. *)

val merge : Term.t * Term.t -> t -> t

(*s [add (a,b) s] disequality [a <> b] to the disequality context [s]. *)

val add : Term.t * Term.t -> t -> t


(*s Replace all [x |-> {x1,...,xn}] with [y |-> {y1,...,yn}] where [yi] is [find xi]. *)

val inst : find:(Term.t -> Term.t) -> t -> t

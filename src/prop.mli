
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

(*s The module [Prop] implements constructors, recognizers, and destructors
  for propositional connectives. *)


type t

val eq : t -> t -> bool


(*s Destructor. *)

type prop =
  | True
  | False
  | Ite of Atom.t * t * t

val destruct : t -> prop
    
(*s Representations of propositional connectives are built using
  the [ite a b c] constructor. This constructor, together with [tt()]
  and [ff()] builds up ordered binary decision diagrams with nonboolean
  terms (that is, terms not of the form [tt()], [ff()], or [Ite(_)])
  in the conditional position.  Viewed as a directed acyclic graph,
  the resulting [Ite] structures are reduced in the sense that there are
  no subgraphs of the form [Ite(a,b,b)], and every subgraph occurs at
  most once. Conditionals along each part are (strictly) ordered by
  the [Term.cmp] function.  When conditionals are considered to be
  variables or uninterpreted terms, the resulting [Ite] structure is
  canonical in the sense that two such structures are equivalent (in the
  theory of booleans) iff they are syntactically equal. *)
  
val mk_tt : t
val mk_ff : t 
val mk_poslit : Atom.t -> t
val mk_neglit : Atom.t -> t 
val mk_ite : Atom.t -> t -> t -> t

(*s Recognizers. *)

val is_tt : t -> bool
val is_ff : t -> bool

(*s Constructors for negation [neg], conjunction [conj], disjunction [disj],
  exclusive or [xor], implication [imp], and equivalence [iff]
  are derived from [tt], [ff], and [ite].
  \begin{tabular}{lcl}
     [neg a] & = & [ite a ff() tt()] \\
  [conj a b] & = & [ite a b ff()] \\
  [disj a b] & = & [ite a tt() b] \\
   [xor a b] & = & [ite a (neg b) b] \\
   [imp a b] & = & [ite a b tt()] \\
   [iff a b] & = & [ite a b (neg b)] 
  \end{tabular} *)

val mk_neg : t -> t
val mk_conj : t -> t -> t
val mk_disj : t -> t -> t
val mk_xor : t -> t -> t
val mk_imp : t -> t -> t
val mk_iff : t -> t -> t

(*s Some more recognizers. *)

val d_conj : t -> ((Atom.t list) * t) option

(*s Return the set of literals in proposition. *)

val literals_of : t -> Atom.Set.t

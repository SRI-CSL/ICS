
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


(*s Module [Dp]: maintaining the logical context of the decision procedures. *)

(*s A logical context is given by a tuple [(u,c,a,t,bv,fa,d,nla)] where
       \begin{itemize}
       \item [u] consists of a set of equalities [x = y] over terms [x], [y]
             which are either uninterpreted or interpreted constants (see
             module [U]).
       \item [c] consists of constraints of the form [x in d] where [x] is
             an uninterpreted term and [d] is a type constraints.  Moreover, [x] 
             is a canonical representative of an equivalence class in [u];
             that is, [U.can x === x]. 
       \item [a], [t], [bv], [fa] are the contexts for the interpreted 
             equality theories linear arithmetic ([a]), tuples ([t]), bitvectors ([bv]),
             and functional arrays [(fa)]. Each of these contexts consists of
             equalities [x |-> b] where [x] is uninterpreted, and [b] is 
             a con-constant term built entirely from interpreted function symbols
             of the theory under consideration (see module [Th]).
       \item [d] stores the known disequalities over uninterpreted terms (see
             module [D]).
       \item [nla] is basically a symbol table for nonlinear terms of the 
             form [x |-> a] where [x] is uninterpreted and [a] is built from
             exponentiation and nonlinear multiplication.
       \end{itemize}
 *)

type t = {
  ctxt : Atom.Set.t;    (* Current context. *)
  u : Cc.t;             (* Congruence closure data structure. *)
  i : Th.t;             (* Interpreted theories. *)
  d : D.t               (* Disequalities. *)
}

(*s Pretty-printing the context of a state. *)

val pp : t Pretty.printer

(*s For an term [a] which is either uninterpreted or an interpreted
 constant, [type_of s a] returns the most refined type as obtained
 from both static information encoded in the arity of uninterpreted
 function symbols (see module [Sym]) and the constraint context
 [c_of s]. *)

val cnstrnt : t -> Term.t -> Cnstrnt.t option

val is_diseq : t -> Term.t -> Term.t -> bool

val tests : t -> Builtin.tests

val sigma : t -> Sym.t -> Term.t list -> Term.t


(*s Variable partitioning. *)

val partition: t -> Term.t Term.Map.t

(*s Solution sets for equality theories. *)

val solution :Theories.t-> t -> (Term.t * Term.t) list


(*s Parameterized operators *)

val find : Theories.t-> t -> Term.t -> Term.t
val inv : Theories.t-> t -> Term.t -> Term.t
val use : Theories.t-> t -> Term.t -> Term.Set.t

(*s Canonization. *)

val can_t : t -> Term.t -> t * Term.t

val can : t -> Atom.t -> t * Atom.t


(*s The empty logical context. *)

val empty : t 

(*s Processing *)

type 'a status =
  | Valid
  | Inconsistent 
  | Satisfiable of 'a


val process: t -> Atom.t -> t status


(*s Compressing the state. *)

val compress : t -> t


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
  mutable ctxt: Atom.Set.t;     (* current context. *)
  mutable a : Th.A.t;           (* Arithmetic *)
  mutable t : Th.T.t;           (* Tuples. *)
  mutable bv : Th.BV.t;         (* Bitvectors. *)
  mutable u: U.t;               (* congruence closure data structure with constraints *)
  mutable c : C.t;              (* constraints. *)
  mutable d : D.t;              (* Disequalities. *)
  mutable nla : Th.NLA.t;       (* nonlinear arithmetic terms. *)
  mutable p : Prop.t            (* Propositions. *)
}

(*s [ctxt s] returns a set of propositions equivalent to [s]. 
 This set is not necessarily minimal, and propositions in this
 set are not guaranteed to be in  canonical form. *)

val ctxt : t -> Atom.Set.t


(*s Accessors for the respective parts of the logical context. *)

val a_of : t -> Th.A.t     
val t_of : t -> Th.T.t     
val bv_of : t -> Th.BV.t  
val u_of : t -> U.t       
val d_of : t -> D.t
val nla_of : t -> Th.NLA.t
val p_of : t -> Prop.t


(*s Pretty-printing the context of a state. *)

val pp : t Pretty.printer


(*s Constant-time but incomplete equality test; its
 main usage is to test if states have changed or not. *)

val eq : t -> t -> bool


(*s If [sub f s t] returns [Yes], then (the context of) [s] implies
 (the context of) [t]; if [sub s t] yields [No], then this is
 not the case. [sub s t] is incomplete in that from the result [X] 
 nothing can be deduced. *)
 
val sub : f:(t -> Atom.t -> Atom.t) -> t -> t -> Three.t


(*s For an term [a] which is either uninterpreted or an interpreted
 constant, [type_of s a] returns the most refined type as obtained
 from both static information encoded in the arity of uninterpreted
 function symbols (see module [Sym]) and the constraint context
 [c_of s]. *)

val cnstrnt : t -> Term.t -> Type.t

val is_diseq : t -> Term.t -> Term.t -> bool


(*s The empty logical context. *)

val empty : t

(*s Shallow copy of context. *)

val copy : t -> t

(*s New binding. Destructively updates the state. *)

val extend : Sym.classify -> t -> Term.t * Term.t -> unit

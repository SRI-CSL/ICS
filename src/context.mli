
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


(*s Module [Context]: The logical context of the decision procedures. *)

(*s A logical context is given by a tuple [(ctxt,u,i,d)] where
       \begin{itemize}
       \item [ctxt] is the set of atoms used for building up this logical context.
       \item [u] consists of a set of equalities [x = y] over terms [x], [y]
             which are either uninterpreted or interpreted constants (see
             module [U]).
       \item [i] is the context of interpreted solution sets (see module [Th]).
       \item [d] stores the known disequalities over uninterpreted terms (see
             module [D]).
       \end{itemize}
 *)

type t = {
  ctxt : Atom.Set.t;    (* Current context. *)
  u : Cc.t;             (* Congruence closure data structure. *)
  i : Th.t;             (* Interpreted theories. *)
  d : D.t               (* Disequalities. *)
}

val empty : t

(*s Pretty-printing the context of a state. *)

val pp : t Pretty.printer

(*s For an term [a] which is either uninterpreted or an interpreted
 constant, [type_of s a] returns the most refined type as obtained
 from both static information encoded in the arity of uninterpreted
 function symbols (see module [Sym]) and the constraint context
 [c_of s]. *)

val cnstrnt : t -> Term.t -> Cnstrnt.t option

val is_diseq : t -> Term.t -> Term.t -> bool

val is_equal : t -> Term.t -> Term.t -> bool

val is_int : t -> Term.t -> bool

(*s Variable partitioning. *)

val v : t -> Term.t -> Term.t

val partition: t -> Term.Set.t Term.Map.t

(*s Solution sets for equality theories. *)

val solution :Theories.t-> t -> (Term.t * Term.t) list


(*s Parameterized operators *)

val find : Theories.t-> t -> Term.t -> Term.t
val inv : Theories.t-> t -> Term.t -> Term.t
val use : Theories.t-> t -> Term.t -> Term.Set.t

(*s [sigma] normal form. *)

val sigma : t -> Sym.t -> Term.t list -> Term.t
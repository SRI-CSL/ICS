
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

type t

val ctxt_of : t -> Atom.Set.t
val p_of : t -> Partition.t
val eqs_of : t -> Theories.t -> Solution.t
val upper_of : t -> int

val v_of : t -> V.t
val d_of : t -> D.t
val c_of : t -> C.t

val empty : t

(*s Pretty-printing the context of a state. *)

val pp : t Pretty.printer

(*s Identity test on contexts. *)

val eq : t -> t -> bool

(*s For an term [a] which is either uninterpreted or an interpreted
 constant, [type_of s a] returns the most refined type as obtained
 from both static information encoded in the arity of uninterpreted
 function symbols (see module [Sym]) and the constraint context
 [c_of s]. *)

val cnstrnt : t -> Term.t -> Cnstrnt.t

(*
(*s Cnstrnts from terms. Raises [Not_found] when unconstrained. *)

val of_term : V.t * t -> Term.t -> Cnstrnt.t

val of_arith : V.t * t -> Sym.arith -> Term.t list -> Cnstrnt.t

val of_builtin : V.t * t -> Sym.builtin -> Term.t list -> Cnstrnt.t
*)

val is_equal : t -> Term.t -> Term.t -> Three.t


(*s Variable partitioning. *)

val v : t -> Term.t -> Term.t
val d : t -> Term.t -> Term.Set.t
val c : t -> Term.t -> Cnstrnt.t

(*s Fold over an equivalence class. *)

val fold : t -> (Term.t -> 'a -> 'a) -> Term.t -> 'a -> 'a


(*s [lookup s a] returns, if possible, a canonical variable equal to [a]. *)

val lookup : t -> Term.t -> Term.t


(*s [choose s p x] returns [z] if for some [y] in the equivalence class of [x] 
 [p y] yields [Some(z)]. If there is not such [y], [Not_found] is raised. *)

val choose : t -> (Term.t -> 'a option) -> Term.t -> 'a

(*s Parameterized operators *)

val mem : Theories.t -> t -> Term.t -> bool
val apply : Theories.t -> t -> Term.t -> Term.t
val find : Theories.t -> t -> Term.t -> Term.t
val inv : Theories.t -> t -> Term.t -> Term.t
val use : Theories.t -> t -> Term.t -> Term.Set.t
val equality : Theories.t -> t -> Term.t -> Fact.equal

(*s [sigma] normal form. *)

val sigma : t -> Sym.t -> Term.t list -> Term.t

(*s [solve i s (a, b)] applies solver for theory [i] on equality [a = b]. *)

val solve : Theories.t -> t -> Fact.equal -> Fact.equal list

(*s Updates. *)

val extend : Atom.t -> t -> t

val union : Theories.t -> Fact.equal -> t -> t
val restrict : Theories.t -> Term.t -> t -> t
val fuse : Theories.t -> Fact.equal -> t -> t
val compose : Theories.t -> Fact.equal -> t -> t

val update: Partition.t -> t -> t

val name : Theories.t -> t * Term.t -> t * Term.t


(*s List all constraints with finite extension. *)

val split : t -> Atom.Set.t



module Changed : sig

  type t

  val in_v : t -> Term.Set.t
  val in_d : t -> Term.Set.t
  val in_c : t -> Term.Set.t

  val in_eqs : Theories.t -> t -> Term.Set.t

  val reset : unit -> unit

  val save : unit -> t

  val restore : t -> unit

  val stable : unit -> bool

  val pp : t Pretty.printer
 
end


(*s Update rules work on the following global variables together with the index
 for creating new variables. Within a [protect] environment, updates are performed
 destructively. Global variables are not protected! *)

val protect : (t -> t) -> t -> t
  


(*
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
 *)

(** Datatype of terms 

  A term is either a variable [Var(s)] with a name [s] of type {!Name.t} or
  an application [App(f,l)] of a function symbol of type {!Sym.t}
  to a list of argument terms. 

  @author Harald Ruess
*)


type t =
  | Var of Var.t
  | App of Sym.t * t list


(** {6 Variables} *)
 
module Var : sig

  (** {7 Constructors} *)
    
  val mk_var : Name.t -> Dom.t option -> t

  val k : int ref
    (** [k] is a global variable which is incremented by the {!Var.mk_rename} 
      variable constructor below.  In addition, calls to {!Tools.do_at_reset} 
      are resetting this variable to its default value [0]. *)
    
  val mk_rename : Name.t -> int option -> Dom.t option -> t
    (** [mk_rename n None] constructs a fresh variable, where
      'fresh' means that the index part of this fresh variable
      (see Module {!Term.Var}) is larger than {!Term.Var.k}; as a side-effect,
      {!Term.Var.k} is incremented. [mk_fresh_var n Some(k)] simply constructs
      a variable of name [n] and index [k]. *)

  val mk_slack : int option -> Var.slack -> t

  val mk_fresh : Th.t -> int option -> Dom.t option -> t

  val mk_free : int -> t


  (** {7 Recognizers} *)

  val is_rename : t -> bool
    (** [is_rename a] holds if [a] is a variable and if it is
      of category 'fresh' (see module {!Var}). *)

  val is_external : t -> bool

  val is_fresh : Th.t -> t -> bool

  val is_free : t -> bool

  val is_slack : t -> bool
  val is_zero_slack : t -> bool
  val is_nonneg_slack : t -> bool


  val is_internal : t -> bool
  val is_dom : Dom.t -> t -> bool

  val is_int : t -> bool
  val is_real : t -> bool


  (** {7 Accessors} *)

  val name_of : t -> Name.t
    (** [name_of a] returns the name [n] if [a] is a variable of the
      form [Var(n)]; otherwise the result is undefined. *)

  val dom_of : t -> Dom.t


end 

(** {6 Applications} *)

module App : sig

  (** {7 Constructors} *)

  val mk_const : Sym.t -> t
    (** [mk_const c] constructs a function application 
      of symbol [c] to the empty argument list. *)

  val mk_app : Sym.t -> t list -> t
    (* [mk_app f l] constructs a function application of symbol [f] to a list [l]. *)

  
  (** {7 Destructors} *)

  val destruct : t -> Sym.t * t list
    (** [destruct a] destructure an application [a] of the form
      [App(f, l)] into [(f, l)]. *)

  val sym_of : t -> Sym.t
    (** [sym_of a] returns the function symbol [f] of an application
      [a] of the form [App(f,_)]. *)

  val theory_of : t -> Th.t
    (** [theory_of a] returns the theory associated with the top-level
      function symbol of [a], and raises [Not_found] otherwise. *)

  val args_of : t -> t list
    (** [sym_of a] returns the argument list [l] of an application
      [a] of the form [App(_,l)]. *)


end


(** {6 Recognizers} *)

val is_var : t -> bool
  (** [is_var a] holds iff [a] is of the form [Var _]. *)

val is_app : t -> bool
  (** [is_app a] holds iff [a] is of the form [App _]. *)

val is_const : t -> bool
  (** [is_const a] holds iff [a] is of the form [App(_,[])]. *)

val is_pure : Th.t -> t -> bool
  (** [is_pure i a] holds iff all function symbols in [a] are of 
    theory [i]. *)


(** {6 Comparisons} *)

val eq : t -> t -> bool
  (** [eq a b] holds iff [a] and [b] are syntactically equal.
    This comparison is linear in the size of [a] and [b]. *)
  

val eql : t list -> t list -> bool
  (** [eql al bl] holds iff [al] and [bl] are of the
    form [a1;...;an] and [b1;...;bn], respectively, and
    if [eq ai bi] holds for all [i]. *)
  

val cmp : t -> t -> int
  (** [cmp a b] realizes a total ordering on terms. It returns
    [0] if [eq a b] holds. Values less than [0] are interpreted
    as '[a] is less than [b]' and values greater than [0] as
    '[a] is greater than [b]'.  Variables are greater than
    applications, variables [Var(x)] and [Var(y)] are ordered 
    according to {!Var.cmp}[x y]
    (in particular, internal variables are always greater than
    external ones), and function applications [App(f, al)] and
    [App(g, bl)] are ordered using a lexicographic ordering
    on the function symbols [f] and [g] as given by {!Sym.cmp}[f g]
    and the orderings on respective terms in the argument lists [al]
    and [bl]. *)


val (<<<): t -> t -> bool
  (** [a <<< b] iff [cmp a b <= 0]. *)

val orient : t * t -> t * t
  (** [orient (a, b)] is [(b, a)] if [b] is greater than [a],
    and [(a, b)] otherwise. *)


val is_equal : t -> t -> Three.t


(** {6 Iterators} *)

val fold : (t -> 'a -> 'a) -> t -> 'a -> 'a
  (** Fold operator on terms. *)

val iter : (t -> unit) -> t -> unit
  (** Iteration operator on terms. *)

val mapl : (t -> t) -> t list -> t list  
  (** Mapping over list of terms. Avoids unnecessary consing. *)


(** {6 Predicates} *)

val for_all : (t -> bool) -> t -> bool
  (** Predicate holds for all subterms. *)

val subterm : t -> t -> bool
  (** [subterm a b] holds if [a] is a subterm of [b]. *)

val occurs : t -> t -> bool
  (** [occurs x a] holds if term [x] occurs in [a]. *)

val assq : t -> (t * 'a) list -> 'a
  (** Association lists for terms. *)


(** {6 Pretty-Printing} *)

val pretty : bool ref

val pp : Format.formatter -> t -> unit

val to_string : t -> string


(** {6 Sets and maps of terms} *)

type trm = t

module Set2 : (Set.S with type elt = trm * trm)

module Set : (Set.S with type elt = trm)

module Map : (Map.S with type key = trm)

val vars_of : t -> Set.t
  (** Return set of variables. *)



module Equal : sig
  type t = trm * trm
  val lhs : t -> trm
  val rhs : t -> trm
  val make : trm * trm -> t
  val destruct : t -> trm * trm
  val pp : t Pretty.printer
  val compare : t -> t -> int
  val is_var : t -> bool
  val is_pure : Th.t -> t -> bool
end

module Diseq : sig
  type t = trm * trm
  val lhs : t -> trm
  val rhs : t -> trm
  val make : trm * trm -> t
  val destruct : t -> trm * trm
  val pp : t Pretty.printer
  val eq : t -> t -> bool
  val compare : t -> t -> int
end

module Nonneg : sig
  type t = trm
  val make : trm -> t
  val term_of : t -> trm
  val pp : t Pretty.printer
  val compare : t -> t -> int
end 

module Pos : sig
  type t = trm
  val make : trm -> t
  val term_of : t -> trm
  val pp : t Pretty.printer
  val compare : t -> t -> int
end


(** {6 Term Substitution} *)

type apply = t * t -> t -> t
type map = (t -> t) -> (t -> t)

module Subst : sig

  type t = (trm * trm) list

  val pp : t Pretty.printer

  val empty : t

  val fuse : apply -> trm * trm -> t -> t
  val compose : apply -> trm * trm -> t -> t

  val lookup : t -> trm -> trm

  val invlookup : t -> (trm -> bool) -> trm

  val apply : map -> ((trm * trm) * 'a) list -> trm -> trm * 'a list

  val fold : (trm * trm -> 'a -> 'a) -> t -> 'a -> 'a

end



(** {6 Hashing} *)

val hash : t -> int


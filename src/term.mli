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

  @author Harald Ruess

  A {i term} is either a 
  - {i variable} [x] ({!Var.t}) or an
  - {i application} of a function symbol [f] ({!Sym.t}) to a list of 
  argument terms [l].

  Each term [a] has an integer value [hash a] associated with it.
*)


type t =
  | Var of Var.t * int               (* variables with indices *)
  | App of Sym.t * t list * int      (* application with indices *)
      (** Notice: all terms should be build using the constructors below.
	This type definition is only visible for convenience to allow for
	pattern matching on terms. *)
      
type trm = t  (* nickname *)

val hash : t -> int
  (** [hash a] returns a hash value such that {!Term.equal}[a b] implies 
    [hash a = hash b].  Furthermore, [hash] restricted to variables is {i injective}. *)


val pp : t Pretty.printer
  (** Pretty-printing of terms. The exact outcome depends on the 
    value {!Pretty.flag}. *)

val to_string : t -> string
  (** Pretty-printing a term to a string. [to_string a] is synonymous
    with {!Pretty.to_string}[pp a]. *)


(** {6 Variables} *)
 
module Var : sig
 
  val mk_var : Name.t -> Var.Cnstrnt.t -> t
    (** [mk_var n d] constructs an {i external} variable (see {!Var.mk_external})
      of name [n] and optional domain constraint [d]. *)
      
  val k : int ref
    (** [k] is a global variable for generating names of internally generated
      variables.  The value of this counter determines the notion of {i freshness}
      as any internal variable with an index above [k] is assumed to be {i fresh}.
      The counter [k] is incremented by the {!Var.mk_rename} variable constructors below.  
      Calls to {!Tools.do_at_reset} are resetting this variable to its default value [0]. *)
    
  val mk_rename : Name.t -> int option -> Var.Cnstrnt.t -> t
    (** [mk_rename n None] constructs a fresh variable, where
      'fresh' means that the index part of this fresh variable
      (see Module {!Term.Var}) is larger than {!Term.Var.k}; as a side-effect,
      {!Term.Var.k} is incremented. [mk_fresh_var n Some(k)] simply constructs
      a variable of name [n] and index [k]; thus, the resulting variable
      is not necessarily fresh. *)

  val mk_slack : int option -> Var.slack -> t
    (** [mk_slack None sl] constructs a {i fresh} slack variable, 
      and [mk_slack Some(i) sl] construct a slack variable of index [i].
      If [sl] is [Var.Zero], then a {i zero slack} variable is created, and
      if [sl] is of the form [Var.Nonneg(d)], then a {i nonnegative slack}
      variable with domain restriction [d] is generated. See also {!Var.mk_slack}. *)

  val mk_fresh : Th.t -> int option -> Var.Cnstrnt.t -> t
    (** [mk_fresh th None d] creates a theory-specific fresh variable with optional
      domain restriction [d], and  [mk_fresh th Some(i) d] creates the theory-specific
      fresh variable of index [i] with optional domain restriction [d]. See also {!Var.mk_fresh}. *)

  val mk_free : int -> t
    (** [mk_free i] creates a deBruijn index for bound variables. *)

  (** Variables are partitioned into {i renaming}, {i external}, {i fresh}, {i free},
    and {i slack variables}, and the following recognizers can be used to test membership
    of a variable into a partition. Thus, for every variable [x], exactly one of the tests
    below succeed. *)
  val is_rename : t -> bool
  val is_external : t -> bool
  val is_fresh : Th.t -> t -> bool
  val is_free : t -> bool
  val is_slack : t -> bool

  (** Slacks are subpartitioned into {i zero} and {i nonnegative} slacks. *)
  val is_zero_slack : t -> bool
  val is_nonneg_slack : t -> bool
    
  val is_internal : t -> bool
    (** [is_internal x] holds iff [x] is either a renaming, a fresh, or a slack variable. *)
    
  val is_dom : Dom.t -> t -> bool
    (** [is_dom d x] holds iff the domain [e] of [x] as obtained by {!Term.Var.dom_of}[x]
      is a subdomain of [d] (see also {!Dom.sub}). *)
    
  val is_int : t -> bool
    (** [is_int x] holds if [x] is restricted to be interpreted over the integers.
      It is defined as [is_dom Dom.Int]. *)
    
  val is_real : t -> bool
    (** [is_real x] holds if [x] is restricted to be interpreted over the reals.
      It is defined as [is_dom Dom.Real]. *)
    
  val compare : t -> t -> int
    (** Total variable ordering. It is designed to be fast, and does not
      obey {!Var.cmp}. If [x == y] then [cmp x y = 0]. Furthermore, if
      [cmp x y < 0] then [cmp y x > 0].  [Invalid_argument] is raised if one of 
      the arguments is a non-variable. *)
    
  module Set : (Set.S with type elt = trm)
    (** Sets of variables. Exception [Invalid_argument] is raised if a 
      nonvariable term is added to any set. *)
    
  module Map : (Map.S with type key = trm)
    (** Maplets with variables in the domain. [Invalid_argument] is raised
      if a binding with a nonvariable term is added. *)
    
  val name_of : t -> Name.t
    (** [name_of a] returns the name [n] if [a] is a variable of the
      form [Var(n)]; otherwise the result is undefined. *)
    
  val dom_of : t -> Dom.t
    (** [dom_of x] returns the domain restriction associated with [x] (see {!Var.dom_of}).
      If there is no such restriction, [Not_found] is raised. *)

  val width_of : t -> int
    (** [with_of x] returns the length of the bitvector interpretation of [x],
      or raises [Not_found]. *)

 val cnstrnt_of : t -> Var.Cnstrnt.t
    (** [cnstrnt_of x] returns the domain constraint associated with [x].
      If there is no such restriction, [Not_found] is raised. *)
   
end 

(** {6 Applications} *)

module App : sig

  val mk_const : Sym.t -> t
    (** [mk_const c] constructs a function application of symbol [c] to the empty
      argument list. *)

  val mk_app : Sym.t -> t list -> t
    (** [mk_app f l] constructs a function application of symbol [f] to a list [l]. *)
  
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
      [a] of the form [App(_,l, _)]. *)

end


(** {6 Recognizers} *)

val is_var : t -> bool
  (** [is_var a] holds iff [a] is of the form [Var _]. *)

val is_app : t -> bool
  (** [is_app a] holds iff [a] is of the form [App _]. *)

val is_const : t -> bool
  (** [is_const a] holds iff [a] is of the form [App(_,[], _)]. *)

val is_pure : Th.t -> t -> bool
  (** [is_pure i a] holds iff all function symbols in [a] are of 
    theory [i] (see {Theory.t}). *)

val is_equal : t -> t -> Three.t
  (** [is_equal a b] returns
    - [Three.Yes] if {!Term.eq}[a b] holds,
    - [Three.No] is [a] and [b] are two distinct constants in some theory [th], and
    - [Three.X] if none of the above holds. *)


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

val compare : t -> t -> int
  (** Total comparison on term which is designed to be fast.
    Does not necessarily coincide with {!Term.cmp}. *)


val (<<<): t -> t -> bool
  (** [a <<< b] iff [cmp a b <= 0]. *)

val orient : t * t -> t * t
  (** [orient (a, b)] is [(b, a)] if [b] is greater than [a],
    and [(a, b)] otherwise. *)


(** {6 Iterators} *)

val fold : (t -> 'a -> 'a) -> t -> 'a -> 'a
  (** Fold operator [fold f a e] on terms applies 
    argument function [f] to all variables in [a]
    and accumulates the results starting with [e]. 
    Thus, if {!Term.var_of][a] is of the form [{x1,...,xn}]
    with the order of variables unspecified,
    then [fold f a e] reduces to [f x1 (f x2 ... (f xn e))]. *) 

val iter : (t -> unit) -> t -> unit
  (** Iteration operator on terms. *)

val mapl : (t -> t) -> t list -> t list  
  (** Mapping over list of terms. Avoids unnecessary consing. *)


(** {6 Predicates} *)

val subterm : t -> t -> bool
  (** [subterm a b] holds if [a] is a subterm of [b]. *)

val occurs : t -> t -> bool
  (** [occurs a b] holds if term [a] occurs in [b]. *)

val assq : t -> (t * 'a) list -> 'a
  (** Association lists for terms. *)


(** {6 Sets and maps of terms} *)

module Set2 : (Set.S with type elt = trm * trm)

module Set : (Set.S with type elt = trm)

module Map : (Map.S with type key = trm)

val vars_of : t -> Var.Set.t
  (** Return set of variables. *)


(** Following to be deprecated. *)
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

(** Following to be deprecated. *)
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

type 'a transformer = t -> t * 'a


(** {6 Term Substitutions} *)

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

type solve = t * t -> Subst.t

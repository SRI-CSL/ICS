(*
 * The contents of this file are subject to the ICS(TM) Community Research
 * License Version 2.0 (the ``License''); you may not use this file except in
 * compliance with the License. You may obtain a copy of the License at
 * http://www.icansolve.com/license.html.  Software distributed under the
 * License is distributed on an ``AS IS'' basis, WITHOUT WARRANTY OF ANY
 * KIND, either express or implied. See the License for the specific language
 * governing rights and limitations under the License.  The Licensed Software
 * is Copyright (c) SRI International 2003, 2004.  All rights reserved.
 * ``ICS'' is a trademark of SRI International, a California nonprofit public
 * benefit corporation.
 *)

(** Datatype of terms 

  @author Harald Ruess

  A {i term} is either a 
  - an injection of a {i variable} [x] ({!Var.t}) or an
  - {i application} of a function symbol [f] ({!Sym.t}) to arguments [l].
*)

type t =
  | Var of Name.t
  | App of app

and app

type trm = t (* nickname *)

(** {i Term arguments} are finite sequences of terms *)
module Args : sig

  type t
    (** Finite sequence of argument terms. Positions are numbered, from left to right,
     from [0] to [l-1] with [l] the number of argument positions. *)

  val length : t -> int
    (** Number of argument terms. *)

  val for_all : (trm -> bool) -> t -> bool
    (** [for_all f tl] holds if [f a] holds for all argument terms [t] in [tl]. *)

  val for_all2 : (trm -> trm -> bool) -> t -> t -> bool
    (** [for_all2 f tl sl] holds if [tl] and [sl] are of the same length [l] and if
      [f s{i} t{i}] holds for all [i] from [0] to [l-1]. *)

  val exists : (trm -> bool) -> t -> bool 
    (** [exists f tl] holds if [f a] holds for some argument terms [t] in [tl]. *)

  val eq : t -> t -> bool
    (** Syntactic equality on argument sequence. *)

  val compare : t -> t -> int
    (** Lexicographic ordering of two argument sequences. *)

  val get : t -> int -> trm
    (** [get tl i] returns [i]th argument term in [tl]. *)

  val set : t -> int -> trm -> unit
    (** [set tl i s] destructively updates [tl] at position [i] to contain term [s].
      Only defined for [i < length tl]. *)

  val make0 : t

  val make1 : trm -> t
    (** Construct an argument sequence of length [1]. *)

  val make2 : trm -> trm -> t
    (** [make2 t1 t2] construct an argument sequence of length [2] with [t1] at
      position [0] and [t2] at position [1]. *)

  val make3 : trm -> trm -> trm -> t
    (** [make3 t1 t2 t3] construct an argument sequence of length [3] with [t1] at
      position [0], [t2] at position [1], and [t3] at position [2]. *)
	      
  val of_list : trm list -> t
    (** [of_list [t{1};...;t{n}]] constructs an argument sequence corresponding to the
      given list of terms. *)

  val of_stack : int -> trm Stacks.t -> t
    (** Arguments from stack. *)
   
  val to_list : t -> trm list
    (** Inverse operation of [Term.Args.of_list]. *)

  val iter : (trm -> unit) -> t -> unit
    (** [iter f tl] applies [f] to its argument term from left to right. *)

  val map : (trm -> trm) -> t -> t
    (** [map f tl] returns an argument sequence of length [length tl] with
      [f t{i}] at position [i] for [t{i}] at position [i] in [tl]. If 
      [f t{i}] returns a term equal to [t{i}], then the result argument
      sequence is identical to the source arguments. *)

  val eqbut : t -> t -> (trm * trm) option 
    (** [eqbut a b] returns [Some(s, t)] if [s] and [t] are the only nonequal
      matching arguments in [a] and [b]. *)

  val hash : t -> int

end

val hash : t -> int
  (** [hash a] returns a hash value such that {!Term.eq}[a b] 
    implies [hash a = hash b]. *)

val pp : t Pretty.printer
  (** Pretty-printing of terms. The exact outcome depends on the 
    value {!Pretty.flag}. *)

val to_string : t -> string
  (** Pretty-printing a term to a string. [to_string a] is synonymous
    with {!Pretty.to_string}[pp a]. *)


(** {6 Variables} *)
 
val mk_external_var : string -> t
  (** [mk_external_var str] constructs an {i external} variable
    of name [str]. [str] must not be the empty string and the leftmost character of
    [str] must not be a digit. *)

val mk_var : Name.t -> t
      
val k : int ref
  (** [k] is a global variable for generating names of internally generated
    variables.  The value of this counter determines the notion of {i freshness}
    as any internal variable with an index above [k] is assumed to be {i fresh}.
    The counter [k] is incremented by the {!Var.mk_rename} variable constructors below.  
    Calls to {!Tools.do_at_reset} are resetting this variable to its default value [0]. *)

val mk_internal_var : string -> int option -> t
  (** [mk_fresh th None d] creates a theory-specific fresh variable with optional
    domain restriction [d], and  [mk_fresh th Some(i) d] creates the theory-specific
    fresh variable of index [i] with optional domain restriction [d]. See also {!Var.mk_fresh}. *)

val mk_fresh_var : string -> t
  (** [mk_fresh_var str] shorthand for [mk_internal_var str None]. *)

(** Variables are partitioned into {i renaming}, {i external}, {i fresh}, {i free},
  and {i slack variables}, and the following recognizers can be used to test membership
  of a variable into a partition. Thus, for every variable [x], exactly one of the tests
  below succeed. *)
val is_external_var : t -> bool

val is_external_varname : Name.t -> bool
    
val is_internal_var : t -> bool
  (** [is_internal x] holds iff [x] is either a renaming, a fresh, or a slack variable. *)
 
val compare : t -> t -> int
  (** Total variable ordering. It is designed to be fast, and does not
    obey {!Var.cmp}. If [x == y] then [cmp x y = 0]. Furthermore, if
    [cmp x y < 0] then [cmp y x > 0].  [Invalid_argument] is raised if one of 
    the arguments is a non-variable. *)

val name_of : t -> Name.t
  (** [name_of a] returns the name [n] if [a] is a variable of the
    form [Var(n)]; otherwise the result is undefined. *)
 

(** {6 Applications} *)

val mk_const : Funsym.t -> t
  (** [mk_const c] constructs a function application of symbol [c] to the empty
    argument list. *)

val mk_unary : Funsym.t -> t -> t
  (** [mk_unary f t] constructs unary term [f(t)]. *)

val mk_binary : Funsym.t -> t -> t -> t
  (** [mk_binary f t1 t2] constructs unary term [f(t1, t2)]. *)

val mk_ternary : Funsym.t -> t -> t -> t -> t
  (** [mk_ternary f t1 t2 t3] constructs unary term [f(t1, t2, t3)]. *)

val mk_app : Funsym.t -> Args.t -> t
  (** [mk_app f l] constructs a function application of symbol [f] to a list [l]. *)
  
val destruct : t -> Funsym.t * Args.t
  (** [destruct a] destructure an application [a] of the form
    [App(f, l)] into [(f, l)]. *)

val sym_of : t -> Funsym.t
  (** [sym_of a] returns the function symbol [f] of an application
    [a] of the form [App(f,_)]. *)

val theory_of : t -> Theory.t
  (** [theory_of a] returns the theory associated with the top-level
    function symbol of [a], and raises [Not_found] otherwise. *)

val args_of : t -> Args.t
  (** [sym_of a] returns the argument list [l] of an application
    [a] of the form [App(_,l, _)]. *)

val arg1 : t -> t
  (** First argument of an application. *)

val arg2 : t -> t
  (** Second argument of an application. *)


(** {6 Recognizers} *)

val is_var : t -> bool
  (** [is_var a] holds iff [a] is of the form [Var _]. *)

val is_app : t -> bool
  (** [is_app a] holds iff [a] is of the form [App _]. *)

val is_const : t -> bool
  (** [is_const a] holds iff [a] is of the form [App(_,[], _)]. *)

val is_unary : t -> bool
val is_binary : t -> bool
val is_ternary : t -> bool

val is_pure : Theory.t -> t -> bool
  (** [is_pure i a] holds iff all function symbols in [a] are of 
    theory [i] (see {!Theory.tt}). *)

val is_ground : t -> bool
  (** [is_ground a] holds iff [a] does not contain any variables. *)

type status = 
  | Variable
  | Pure of Theory.t
  | Mixed of Theory.t * t

val status : t -> status

val status2: t -> t -> status


(** {6 Comparisons} *)

val eq : t -> t -> bool
  (** [eq a b] holds iff [a] and [b] are syntactically equal.
    This comparison is linear in the size of [a] and [b]. *)
  
val compare : t -> t -> int
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

val fast_compare : t -> t -> int
  (** Total comparison on term which is designed to be fast.
    Does not necessarily coincide with {!Term.cmp}. *)

val (<<<): t -> t -> bool
  (** [a <<< b] iff [cmp a b <= 0]. *)

val orient : t * t -> t * t
  (** [orient (a, b)] is [(b, a)] if [b] is greater than [a],
    and [(a, b)] otherwise. *)


(** {6 Sets and maps of terms} *)

module Set : (Sets.S with type elt = trm)
  (** Set of terms. *)

module Map : (Maps.S with type key = trm)
  (** Maps with terms as keys. *)

module Hash : (Hashtbl.S with type key = trm)
  (** Hash table with terms as keys. *)

val vars_of : t -> Set.t
  (** Return set of variables. *)

val is_var_of : t -> t -> bool
  (** [is_var_of x a] holds iff [x] is in [vars_of a]. *)


(** {6 Iterators} *)

val fold : (t -> 'a -> 'a) -> t -> 'a -> 'a
  (** Fold operator [fold f a e] on terms applies 
    argument function [f] to all variables in [a]
    and accumulates the results starting with [e]. 
    Thus, if [vars_of a] is of the form [{x1,...,xn}]
    with the order of variables unspecified,
    then [fold f a e] reduces to [f x1 (f x2 ... (f xn e))]. *) 

val iter : (t -> unit) -> t -> unit
  (** Iteration operator on terms. *)

val for_all : (t -> bool) -> t -> bool
  (** [forall p a] holds if [p x] for all variables in [a]. *)

val exists : (t -> bool) -> t -> bool
  (** [exists p a] holds if [p x] for some variable in [a]. *)

val choose : (t -> bool) -> t -> t
  (** [choose p a] chooses a variable of [a] which satisfies predicate [p]. *)

(*
val replace: (Funsym.t -> t list -> t) -> t -> Name.t list -> t list -> t
(** [replace norm a [x1;...;xn] [a1;...an]] applies 
  the substitution [a[x1:= a1]...[xn:=an]]; and all new 
  subterms are normalized using [norm]. *)
*)


(** {6 Predicates} *)

val subterm : t -> t -> bool
  (** [subterm a b] holds if [a] is a subterm of [b]. *)

val occurs : t -> t -> bool
  (** [occurs a b] holds if term [a] occurs in [b]. *)

type map = (t -> t) -> (t -> t)

(** {i Term substitutions} consist of a finite set
  of bindings [x |-> t] with [x] not necessarily 
  a variable and [t] does not contain any domain 
  term. *)
module Subst : sig

  type t
    (** Finite set of bindings [x |-> t]. *)

  val pp : Format.formatter -> t -> unit
    (** Pretty-printing a term substitution. *)

  val lookup : t -> trm -> trm
    (** [lookup rho s] returns [t] if there is a binding [s |-> t]
      and raises [Not_found] otherwise. *)

  val empty : unit -> t
    (** Empty term substitution. *)

  val singleton : trm -> trm -> t

  val fuse : t -> trm -> trm -> unit
    (** [fuse rho s t] replaces occurrences of [s] in codomain
      terms of [rho] by [t] and normalizes the resulting terms
      using {!Term.sigma}. *)

  val compose : t -> trm -> trm -> unit
    (** [compose rho s t] first fuses [s] by [t] in [rho] and
      then adds binding [s |-> t]. *)

  val apply : t -> trm -> trm
    (** [apply rho s] replaces subterms [t] in [s] with [t'] if
      [t |-> t'] is in [rho]. *)

  val fold : (trm -> trm -> 'a -> 'a) -> t -> 'a -> 'a

  val iter : (trm -> trm -> unit) -> t -> unit

  val of_list : (trm * trm) list -> t

end

type interp = Funsym.t -> Args.t -> trm

val sigma : interp

val map : (t -> t) -> t -> t

val solve : t -> t -> Subst.t
  (** [solve s t] returns a solved form using a registered solver
    for the theory of th top-level application in [s] or [t]. If no
    solver method is registered, then [Exc.Incomplete] is raised. *)

val replace : t -> t -> t -> t
  (** [replace s1 s2 t] replaces all occurrences of [s1] in [t]
    by [s2]. The resulting applications are normalized using {!Term.sigma}. *)

val vreplace: t -> Name.t list -> t list -> t
  (** [replace a [x1;...;xn] [a1;...an]] applies 
    the substitution [a[x1:= a1]...[xn:=an]]; and all new 
    subterms are normalized using [sigma]. *)

val is_equal : t -> t -> Three.t
  (** [is_equal a b] returns
    - [Three.Yes] if {!Term.eq}[a b] holds,
    - [Three.No] is [a] and [b] are two distinct constants in some theory [th], and
    - [Three.X] if none of the above holds. *)

val is_cnstrnt : t -> Cnstrnt.t -> Three.t

val is_nonneg : t -> Three.t

val is_pos : t -> Three.t


(** {6 Registration} *)

module Methods : sig

  type m = {
    mutable printer : (Format.formatter -> Funsym.t -> Args.t -> unit) option;
    mutable can:  (Funsym.t -> Args.t -> t) option;
    mutable solve : (t -> t -> Subst.t) option;
    mutable is_diseq : (t -> t -> bool) option;
    mutable is_nonneg : (t -> Three.t) option;  
    mutable is_pos : (t -> Three.t) option;
    mutable has_cnstrnt : (t -> Cnstrnt.t -> Three.t) option;
  }
      
  val empty : unit -> m

  val register : Theory.t -> m -> unit

end


(** {6 Variable Assignments, Interpretations, and Models.} *)

(** {i Nondeterministic assignment} with bindings of 
  the form [x |-> {v1, ..., vn}]. The [vi] might contain
  variables. *)
module Assign : sig

  type t
    (** Representation type for nondeterministic assignments. *)

  val pp : Format.formatter -> t -> unit
    (** Pretty-printing. *)
      
  val empty : t
    (** Empty set of bindings. *)

  val add : trm -> trm -> t -> t
    (** [add x a alpha] updates the assignment [alpha] with [x |-> {a} U alpha(x)]. *)

  val apply : t -> trm -> Set.t
    (** For a variable [x], [apply alpha x] returns a set of values [vs] if [alpha]
      contains a binding [x |-> vs] or raises [Not_found]. *)

  val choose : trm -> t -> trm

  val is_ground : t -> bool
    (** [is_ground alpha] holds if all terms in the ranges of the bindings of [alpha]
      are variable-free. *)

  val is_deterministic : t -> bool
    (** [is_deterministic alpha] holds iff all bindings of [alpha] are of the
      form [x |-> {v}]. *)

  val to_list : t -> (trm * Set.t) list

 val value : interp -> t -> trm -> trm
    (** Given an interpretation [i], a nondeterministic variable assignment [alpha], 
      and a term [a], [value i alpha a] returns a possible value [v].
      - [value f alpha x = v] with [v = choose(alpha(x))] if [alpha(x)] is nonempty and [v = x] otherwise,
      - [value f alpha `f(a1,...,an)' = i f [v1,...,vn]] with [vi = value f alpha ai]. *)

  val values : interp -> t -> trm -> Set.t
    (** [values i alpha a] collects the set of all possible values for [a] by
      considering all possible variable assignments [alpha(x)] instead of choosing 
      an arbitrary one as in {!Term.Assign.value} . *)
      
  val combine : interp -> t -> t -> t
    (** Given two variable assignments [rho], [tau], the variable assignment [combine rho tau]
      includes [x |-> vs] if either
      - [x |-> vs] in [rho] but not in [tau],
      - [x |-> vs] in [tau] but not in [rho], or
      - [x |-> vs1] in [tau], [x |-> vs2] in [tau], and [vs] is obtained by eliminating
      all domain variables from [rho], [tau] in the union of [vs1] and [vs2]. *)

end

(** {i Finite interpretations} are maps with bindings [(f, al) |-> v] for
  function symbols [f], argument lists [al], and term values [v]. *)
module Interp : sig
 
  type t
    (** Representation type for finite interpretations. *)

  val pp : Format.formatter -> t -> unit
    (** Pretty-printing finite interpretations. *)

  val empty : t
    (** Empty set of bindings. *)

  val update : Funsym.t -> Args.t -> trm -> t -> t
    (** [update f al v i] returns the updated interpretation [i[(f, al) := v]]. *)

  val apply : t -> interp
    (** [apply i f al] returns a value [v] or raises [Not_found]. *)

  val combine : t -> t -> t
      
end

(** A {i term model} consists of a pair [(i, alpha)] with
  - interpretation [i] obtained from a finite interpretation by extending
  it with the canonical interpretation {!Term.sigma} 
 - and a variable assignment [alpha]. *)
module Model : sig

  type t = Interp.t * Assign.t
      (** Representation of term models. *)

  val pp : Format.formatter -> t -> unit
    (** Pretty-printing term models. *)

  val empty : t
    (** Empty term model [(i, id)] with [i] the canonical term interpretation
      {!Term.sigma} and [id] the identity variable assignment. *)

  val is_deterministic : t -> bool
    (** [is_deterministic (i, alpha)] holds if [alpha] is {i deterministic}. *)

  val is_ground : t -> bool
    (** [is_ground (i, alpha)] holds if [alpha] is {i ground}. *)

  val value : t -> trm -> trm

  val combine : t -> t -> t

end


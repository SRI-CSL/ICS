
(*
 * ICS - Integrated Canonizer and Solver
 * Copyright (C) 2001-2004 SRI International
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the ICS license as published at www.icansolve.com
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * ICS License for more details.
 *
 * ics.mli describes the application programming interface.
 * Currently, we support interfaces for Ocaml, C, and Lisp.
 * The APIs for C and Lisp are automatically generated from this file.
 * When calling ICS from a language other than Ocaml, one first has to
 * start the caml runtime. In the case of C, for example, one simply
 * calls the function ics_caml_startup.
 *
 *      void ics_caml_startup(int full, char** argv)
 *
 * For an explanation of the arguments, see the documentation of
 * the Ocaml function [caml_startup].
 * Datastructures in the Ocaml space are all of type [value*], and
 * can be freed using
 *
 *        void ics_deregister(value* r)
 *
 * ICS datastructures can safely be finalized (in a language with
 * garbage collections), since the API is completely functional.
 *
 * When using this API from C, exceptions are being handled by calling
 * the [ocaml_error] function. Again, details on the behavior of this
 * function can be obtained from the Ocaml documention.
 *)

val init : int -> unit


(*s The type [q] of multi-precision rational numbers. [num_of_int n]
    injects an integer into this type, [num_of_ints n m], for [m <> 0],
    constructs a normalized representation of the rational [n/m] in [q],
    [string_of_num q] constructs a string (usually for printout) of a
    rational number, and [num_of_string s] constructs a rational, whenever
    [s] is of the form ["n/m"] where [n] and [m] are naturals.
  *)
    
type q

val num_of_int : int -> q
val num_of_ints : int -> int -> q
val ints_of_num : q -> string * string
val string_of_num : q -> string
val num_of_string : string -> q


(*s The [interval] type. Interval consists of a domain of interpretation, a 
  low bound, and a high bound. *)


    (*s [low_bound] is the type of lower bounds in intervals and they are
      either negative infinity, or a rational number together with a strictness attribute.
      The recognizers [low_bound_is_open]
      and [low_bound_is_closed] are only defined when [low_bound_is_neginf] does
      not hold. In these cases, one can access the lower bound using [low_bound_value] . *)
  
type low_bound

val low_bound_neginf : unit -> low_bound
val low_bound_open : q -> low_bound
val low_bound_closed : q -> low_bound
   
val low_bound_is_neginf : low_bound -> bool
val low_bound_is_open : low_bound -> bool
val low_bound_is_closed : low_bound -> bool
val low_bound_value : low_bound -> q

    (*s [high_bound] is the type of upper bounds in intervals and they are
      either positive infinity, or a rational number together with a strictness attribute.
      The recognizers [high_bound_is_open]
      and [high_bound_is_closed] are only defined when [high_bound_is_posinf] does
      not hold. In these cases, one can access the lower bound using [high_bound_value] . *)
  
type high_bound

val high_bound_posinf : unit -> high_bound
val high_bound_open : q -> high_bound
val high_bound_closed : q -> high_bound
   
val high_bound_is_posinf : high_bound -> bool
val high_bound_is_open : high_bound -> bool
val high_bound_is_closed : high_bound -> bool
val high_bound_value : high_bound -> q

    (*s Intervals are interpreted either over the reals, the integers, or the reals without
      the integers. *)
   
type interval_domain

val interval_domain_int: unit -> interval_domain
val interval_domain_real: unit -> interval_domain
val interval_domain_nonint: unit -> interval_domain


val interval_domain_is_real : interval_domain -> bool
val interval_domain_is_int : interval_domain -> bool
val interval_domain_is_nonint : interval_domain -> bool


type interval = interval_domain * low_bound * high_bound

val interval_pp : interval -> unit

(*s Constraints. *)

type cnstrnt

val cnstrnt_top : cnstrnt
val cnstrnt_bot : cnstrnt
val cnstrnt_boolean : cnstrnt
val cnstrnt_tuple : cnstrnt
val cnstrnt_arith : interval list -> cnstrnt
val cnstrnt_oo : interval_domain -> q -> q -> cnstrnt
val cnstrnt_oc : interval_domain -> q -> q -> cnstrnt
val cnstrnt_co : interval_domain -> q -> q -> cnstrnt
val cnstrnt_cc : interval_domain -> q -> q -> cnstrnt
val cnstrnt_lt : interval_domain -> q -> cnstrnt
val cnstrnt_le : interval_domain -> q -> cnstrnt
val cnstrnt_gt : interval_domain -> q -> cnstrnt
val cnstrnt_ge : interval_domain -> q -> cnstrnt
val cnstrnt_real : cnstrnt
val cnstrnt_int : cnstrnt
val cnstrnt_nonint : cnstrnt
val cnstrnt_neg : cnstrnt
val cnstrnt_nonpos : cnstrnt
val cnstrnt_singleton : q -> cnstrnt
val cnstrnt_diseq : q -> cnstrnt

val cnstrnt_is_bot : cnstrnt -> bool
val cnstrnt_is_top : cnstrnt -> bool
val cnstrnt_is_tuple : cnstrnt -> bool
val cnstrnt_is_boolean : cnstrnt -> bool

val cnstrnt_is_arith : cnstrnt -> bool
val cnstrnt_d_arith : cnstrnt -> interval list

val cnstrnt_is_singleton : cnstrnt -> bool
val cnstrnt_d_singleton : cnstrnt -> q

val cnstrnt_is_nonzero: cnstrnt -> bool  

val cnstrnt_pp : cnstrnt -> unit

val cnstrnt_of_interp: Term.t -> cnstrnt       
val cnstrnt_union : cnstrnt -> cnstrnt -> cnstrnt
val cnstrnt_inter : cnstrnt -> cnstrnt -> cnstrnt
val cnstrnt_compl : cnstrnt -> cnstrnt

    
(*s Terms. This is the main syntactic category of ICS. Terms are either
    variables, uninterpreted function application, or interpreted
    constants and operators drawn from a combination of theories.
    Currently, ICS supports linear arithmetic, both rational and integer,
    tuples, function (array update) update, sets, and bitvectors.

    Terms are build using constructors, whose names are all of the form [mk_xxx].
    For each constructor [mk_xxx] there is a corresponding recognizer [is_xxx]
    which reduces to true if its argument term has been built
    with the constructor [mk_xxx]. Moreover, for each constructor
    [mk_xxx}] above there is a corresponding desctructor [d_xxx$]
    for analyzing the components of such a constructor term.
*)


type term

type domain = Term.domain

val is_intdom : domain -> bool
val is_booldom : domain -> bool
val is_ratdom : domain -> bool

val is_external : term -> bool


  (*s  Variables of name [x] are build with [mk_var(x)].
    The constructor [mk_fresh(x)] explicitly creates a
    fresh variable, by adding an new integer to the string
    argument in order to create a fresh name. [is_var a] tests if
    its argument is a variable.  For all terms for which [is_var a]
    is true, the name of [a] can be obtained using the destructor [d_var];
    the behavior of applying [d_var] to all other terms is undefined.
  *)

val mk_var : string -> term
val mk_intvar : string -> term
val mk_ratvar : string -> term
val mk_boolvar : string -> term
val is_var : term -> bool
val d_var : term -> string
val is_intvar : term -> bool
val is_ratvar : term -> bool
val is_boolvar : term -> bool

val mk_rename_var : string -> term -> term
val is_rename_var : term -> bool
val d_rename_var : term -> string * term

val mk_fresh : domain -> term
val is_fresh : term -> bool
val d_fresh : term -> string * domain

val is_internal_var : term -> bool


    (*s Function applications are created using the [mk_app~f~l]
      constructor.  In general, the name of the function [f] is
      specified by an arbitrary term and not only by a function
      symbol. Function update [update~f~x~a] specifices the
      update of function [f] at position [x] with value [a].
    *)

val mk_uninterp : term -> term list -> term
val mk_uninterp_a: term -> term list -> term
val mk_uninterp_ac : term -> term list -> term
val mk_uninterp_c : term -> term list -> term
val is_uninterp : term -> bool
val d_uninterp : term -> term * term list


    (*s Update. *)

val mk_update : term -> term -> term -> term
val is_update : term -> bool
val d_update : term -> term * term * term



    (*s Arithmetic terms include rational constants built from
      [mk_num q];  in addition, linear arithmetic terms are built
      using binary and $n$-ary versions for addition and
      multiplication, unary negation [mk_unary_minus], and a
      binary subtraction constructor [mk_minus]. Internally, [mk_times]
      and [mk_times2] build up different data structures for linear
      multiplication, i.e.  multiplication by a constant number, and
      nonlinear multiplication. [is_multq] is successful only if its
      argument is indeed a linear multiplication, and [is_mult] yields
      true only if the actual argument is indeed nonlinear.
    *)
 
val mk_num : q -> term
val mk_div : term -> term -> term
val mk_add : term list -> term
val mk_add2 : term -> term -> term    
val mk_sub : term -> term -> term
val mk_unary_minus : term -> term
val mk_mult : term list -> term
val mk_mult2 : term -> term -> term
val mk_expt : term -> int -> term

val is_arith : term -> bool
val is_num : term -> bool
val is_multq : term -> bool
val is_add : term -> bool
val is_mult : term -> bool
val is_div : term -> bool
val is_expt : term -> bool

val d_num : term -> q
val d_add : term -> term list
val d_mult : term -> term list
val d_multq : term -> q * term    
val d_div : term -> term * term
val d_expt : term -> term * int

    (*s Tuples are built using the [mk_tuple] constructor, and
      projection of the [i]-th component of a tuple [t] of
      length [n] is realized using [mk_proj i n t];
      [is_tuple a] and [is_proj a] are the respective recognizers,
      while [d_tuple] returns the tuple entries as a list, and
      [d_proj] returns the triple [(i,n,a)] for a projection
      constructed with [mk_proj i n a].
    *)
    
val mk_tuple : term list -> term
val mk_proj : int -> int -> term -> term
    
val is_tuple : term -> bool
val is_proj : term -> bool

val d_tuple : term -> term list
val d_proj : term -> int * int * term

    (*s Terms representing the usual propositional constants and
      connectives are built from the following constructors.
      Boolean terms are either built using [mk_true], [mk_false],
      or [mk_ite]. These constructors build ordered binary decision
      diagrams (OBDDs), where the conditional is neither a
      propositional constant nor a conditional. The ordering
      on these conditionals is given by [term_cmp]. In addition,
      [mk_ite] carries out a number of simplifications.

      Other constructors such as [mk_conj] can be thought of
      as being derived. For example, [mk_conj a b] reduces to
      [mk_ite a b (mk_false)]. Consequently, exactly one of the
      recognizers [is_true], [is_false], or [is_ite] holds of
      any boolean term. Recognizers such as [is_conj] yield
      true if the argument is a conditional with its third
      argument being the false constant. Destructors such as
      [d_conj] are only defined on arguments satisfying the
      corresponding recognizer [is_conj].
    *)
    
val mk_true  : unit -> term
val mk_false : unit -> term
val mk_ite : term -> term -> term -> term

val mk_neg : term -> term
val mk_conj : term -> term -> term
val mk_conjl : term list -> term
val mk_disj : term -> term -> term
val mk_disjl : term list -> term
val mk_xor : term -> term -> term
val mk_imp : term -> term -> term
val mk_iff : term -> term -> term

val is_bool : term -> bool
    
val is_true : term -> bool
val is_false : term -> bool
val is_ite : term -> bool

val is_neg : term -> bool
val is_conj : term -> bool
val is_disj : term -> bool
val is_xor : term -> bool
val is_imp : term -> bool
val is_iff : term -> bool

val d_ite : term -> term * term * term

val d_neg : term -> term
val d_conj : term -> term * term
val d_disj : term -> term * term
val d_xor : term -> term * term
val d_imp : term -> term * term
val d_iff : term -> term * term
    

    (*s Equality and disequality. [mk_equal a b] constructs equalites,
        whereas [mk_diseq a b] yields a disequality of the form
        [mk_not(mk_equal a b)]. *)
      
val mk_equal  : term -> term -> term
val is_equal : term -> bool
val d_equal : term -> term * term

val mk_diseq  : term -> term -> term
val is_diseq : term -> bool
val d_diseq : term -> term * term

val mk_cnstrnt  : cnstrnt -> term -> term
val is_cnstrnt : term -> bool
val d_cnstrnt : term -> cnstrnt * term


    (*s  Constructors for unary arithmetic predicates.
      [mk_int t] restricts the domain of interpretations of term
      [t] to the integers. Similarly, [mk_real] restricts its
      argument to the real numbers, [mk_pos] restricts its
      argument to the positive real numbers, [mk_neg] restricts
      its argument to the negative real numbers, [mk_nonneg]
      restricts its argument to the nonnegative real numbers, and
      [mk_nonpos] restricts its argument to the nonpositive real
      numbers.  Membership test [mk_in a b] is equivalent
      to [mk_app b a], [mk_notin a b] is equivalent to [mk_not(mk_in a b)],
      and the remaining constructors such as [mk_int a] can be viewed as
      abbreviations for [mk_in (cnstrnt_int) a], where [cnstrnt_int] represents
      the set of integers (see below). 
  *)

val mk_int : term -> term
val mk_real : term -> term
val mk_nonint : term -> term
    
val mk_lt : term -> term -> term
val mk_le : term -> term -> term
val mk_gt : term -> term -> term
val mk_ge : term -> term -> term

(*s Set of terms. *)

type terms

val terms_empty : unit -> terms
val terms_add : term -> terms -> terms
val terms_pp : terms -> unit
val terms_mem : term -> terms -> bool
val terms_sub : terms -> terms -> bool
val terms_is_empty : terms -> bool
val terms_to_list : terms -> term list
val terms_of_list : term list -> terms
val terms_choose : terms -> term * terms

(*s Set of fresh variables. *)

val freshvars_of : term -> terms
   
    
(*s Maps with terms as domain. *)

type 'a map

val map_empty : unit -> 'a map
val map_add : term -> 'a -> 'a map -> 'a map
val map_is_empty : 'a map -> bool
val map_find : term -> 'a map -> 'a
val map_remove : term -> 'a map -> 'a map
val map_mem :  term -> 'a map -> bool
val map_to_list : 'a map -> (term * 'a) list
val map_pp : ('a -> unit) -> 'a map -> unit

    
(*s Substititon. *)
    
type subst

val subst_empty : unit -> subst
val subst_add   : term -> term -> subst -> subst
val subst_mem   : subst -> term -> bool
val subst_find  : subst -> term -> term
val subst_apply : subst -> term -> term
val subst_of_list : (term * term) list -> subst
val subst_to_list : subst -> (term * term) list
val subst_pp    : subst -> unit

(*s Integer tag for each term, which is unique for each session. *)

val term_tag : term -> int  


(*s Equality and Comparison.
  Both the equality test [eq] and comparison [cmp] on terms
  works in constant time. Comparison [cmp a b] returns either
  [-1], [0], or [1] depending on whether [a] is less than [b],
  the arguments are equal, or [a] is greater than [b].
  
  Notice that the comparison function [cmp] is session-dependent,
  since it uses the order of memory addresses as the underlying order. 
  *)

val term_eq : term -> term -> bool
val term_cmp : term -> term -> int
val term_fast_cmp : term -> term -> int
val term_pp : term -> unit       
val eqn_pp : term * term -> unit 


(*s A [state] or context can be thought of a function with
  terms as both domain and codomain. Such a context determines
  canonical representatives for terms. Contexts are represented
  by the abstract data type [state].  An empty context
  is built using the [init] operator. The canonical
  representative of a term in a given context is determined by
  [find]. [pp_find] prints the finite, non-identical part of the context
  on standard output. [state_eq] tests if two states are identical.
*)

type state = Dp.t

val state_eq : state -> state -> bool
 
val state_init : unit -> state

type theories = Term.theories

val theory_arith : unit -> theories
val theory_tuple : unit -> theories
val theory_boolean : unit -> theories
val theory_eq : unit -> theories


val state_find : theories -> state -> term -> term

val state_ctxt_of : state -> term list
val state_find_of : theories -> state -> subst
val state_use_of : theories -> state -> terms map
val state_diseqs_of : state -> terms map
val state_cnstrnts_of : state -> cnstrnt map

val state_inconsistent : state -> bool

val state_solutions : state -> terms -> (term * terms) list

val state_pp : state -> unit

val state_use : theories -> state -> term -> terms (*s Use lists. For debugging purposes. *)
val state_diseqs : state -> term -> terms
val state_ext : state -> term -> terms
  
(*s The operation [process] adds a new proposition to a state.
  The codomain of this function is of type [status], elements of
  which represent the three possible outcomes of processing a
  proposition: 1. the argument is inconsistent in the
  current context, 2. it is valid, or, 3., it is satisfiable but
  not valid. In the third case, a modified state is obtained using
  the destructor [d_consistent].
*)

type status =
  | Valid
  | Inconsistent
  | Consistent of Dp.t	
    
val is_consistent   : status -> bool
val is_redundant    : status -> bool
val is_inconsistent : status -> bool

val d_consistent : status -> state
    
val process : state -> term -> status

(*s In addition, a given state may be used to check for the validity
  or the unsatisfiability of a given proposition. *)

type tstatus = Dp.status

val check : state -> term -> tstatus

val is_check_valid : tstatus -> bool
val is_check_inconsistent : tstatus -> bool
val is_check_satisfiable : tstatus -> bool
val d_check_satisfiable : tstatus -> state list


(*s [can] normalizes terms inside-out. The resulting term may contain *)
(*s fresh variables as introduced by solvers but no rename variables. *)

val can : state -> term -> term

(*s Solving an equation in one of the theories. *) 

val solve : theories -> state -> term * term -> (term * term) list option

(*s Computing a best constraint in a given state. *)

val cnstrnt : state -> term -> cnstrnt

(*s Groebner basis completion. *)

val groebner : state -> state option

(*s Controls.
    [reset] clears all the global tables. This
    does not only include the current context, but also
    internal tables used for hash-consing and memoization purposes.
    [gc] triggers a full major collection of ocaml's garbage collector.
    Finally, [set_verbose] controls the amount of trace messages.
    The default is [0], which means that nothing is printed at all.
    The higher the value, the more numerous the messages are.
  *)

val reset : unit -> unit
    
val gc : unit -> unit
    
val set_verbose : int -> unit
    
val flush : unit -> unit

    
(*s Lists. *)

val is_nil : 'a list -> bool
val cons : 'a -> 'a list -> 'a list
val head : 'a list -> 'a
val tail : 'a list -> 'a list
val list_pp : ('a -> unit) -> ('a list -> unit)

    
(*s Pairs. *)

val pair : 'a -> 'b -> 'a * 'b
val fst : 'a * 'b -> 'a
val snd : 'a * 'b -> 'b

(*s Triples. *)

val triple : 'a -> 'b -> 'c -> 'a * 'b * 'c
val fst_of_triple : 'a * 'b *'c -> 'a
val snd_of_triple : 'a * 'b *'c -> 'b
val third_of_triple : 'a * 'b *'c -> 'c

(*s Quadruples. *)
  
val fst_of_quadruple : 'a * 'b * 'c *'d -> 'a
val snd_of_quadruple : 'a * 'b * 'c *'d -> 'b
val third_of_quadruple : 'a * 'b * 'c *'d -> 'c
val fourth_of_quadruple : 'a * 'b * 'c *'d -> 'd
    
      
(*s Options. *)

val is_some : 'a option -> bool
val is_none : 'a option -> bool

val value_of : 'a option -> 'a

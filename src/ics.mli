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
val string_of_num : q -> string
val num_of_string : string -> q

    
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

  (*s  Variables of name [x] are build with [mk_var(x)].
    The constructor [mk_fresh(x)] explicitly creates a
    fresh variable, by adding an new integer to the string
    argument in order to create a fresh name. [is_var a] tests if
    its argument is a variable.  For all terms for which [is_var a]
    is true, the name of [a] can be obtained using the destructor [d_var];
    the behavior of applying [d_var] to all other terms is undefined.
  *)

val mk_var : string -> term
val is_var : term -> bool
val d_var : term -> string   
    
val mk_fresh : string -> term
val is_fresh : term -> bool
val fresh_equiv : term -> term option


    (*s Function applications are created using the [mk_app~f~l]
      constructor.  In general, the name of the function [f] is
      specified by an arbitrary term and not only by a function
      symbol. Function update [update~f~x~a] specifices the
      update of function [f] at position [x] with value [a].
    *)
    
val mk_app : term -> term list -> term
val mk_update : term -> term -> term -> term

val is_app : term -> bool
val is_update : term -> bool

val d_app : term -> term * term list
val d_update : term -> term * term * term

    (*s Conditionals. *)

val mk_cond : term -> term -> term -> term
val is_cond : term -> bool
val d_cond : term -> term * term * term

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
val mk_plus : term list -> term
val mk_plus2 : term -> term -> term    
val mk_minus : term -> term -> term
val mk_unary_minus : term -> term
val mk_times : term list -> term
val mk_times2 : term -> term -> term

val is_arith : term -> bool
val is_num : term -> bool
val is_multq : term -> bool
val is_add : term -> bool
val is_mult : term -> bool

val d_num : term -> q
val d_add : term -> term list
val d_mult : term -> term list
val d_multq : term -> q * term    

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

      Other constructors such as [mk_and] can be thought of
      as being derived. For example, [mk_and a b] reduces to
      [mk_ite a b (mk_false)]. Consequently, exactly one of the
      recognizers [is_true], [is_false], or [is_ite] holds of
      any boolean term. Recognizers such as [is_and] yield
      true if the argument is a conditional with its third
      argument being the false constant. Destructors such as
      [d_and] are only defined on arguments satisfying the
      corresponding recognizer [is_and].
    *)
    
val mk_true  : unit -> term
val mk_false : unit -> term
val mk_ite : term -> term -> term -> term
val mk_not : term -> term
val mk_and : term -> term -> term
val mk_or : term -> term -> term
val mk_xor : term -> term -> term
val mk_imp : term -> term -> term
val mk_iff : term -> term -> term

val is_bool : term -> bool
    
val is_true : term -> bool
val is_false : term -> bool
val is_ite : term -> bool
val is_not : term -> bool
val is_and : term -> bool
val is_or : term -> bool
val is_xor : term -> bool
val is_imp : term -> bool
val is_iff : term -> bool

val d_ite : term -> term * term * term
val d_not : term -> term
val d_and : term -> term * term
val d_or : term -> term * term
val d_xor : term -> term * term
val d_imp : term -> term * term
val d_iff : term -> term * term
    

    (*s Equality and disequality. [mk_equal a b] constructs equalites,
        whereas [mk_diseq a b] yields a disequality of the form
        [mk_not(mk_equal a b)].
      *)
      
val mk_equal  : term -> term -> term
val is_equal : term -> bool
val d_equal : term -> term * term

val mk_diseq  : term -> term -> term

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
    
val mk_in : term -> term -> term
val mk_notin: term -> term -> term
val mk_int : term -> term
val mk_real : term -> term
val mk_lt : term -> term -> term
val mk_le : term -> term -> term
val mk_gt : term -> term -> term
val mk_ge : term -> term -> term


    (*s A fixed-sized bitvector constant of width [5] such as [0b01001] is
      constructed by [mk_bv_const "01001"]. These constructor
      can also be used to emulate the [mk_bv_eps],
      [mk_bv_one], and [mk_bv_zero] functions for
      constructing the empty bitvector, bitvectors with only ones, and
      bitvectors with only zeros, respectively. A bitvector [b1] of length [n] is
      concatenated with a bitvector [b2] of length [m] by [mk_bv_conc (n,b1) (m,b2)].
      Furthermore, given a bitvector [b] of [n], the extraction of the bits [i]
      through [j] is denoted by [mk_bv_extr (n,b) i j].  Hereby, the left-most
      position of a bitvector of width [n] is addressed by [0] and the right-most bit
      by [n-1]\@. The constructors [mk_bv_and n], [mk_bv_or n], and [mk_bv_xor n]
      are the bitwise logical operations of bitvectors of width [n].
    *)

val mk_bv_eps : unit -> term
val mk_bv_zero : int -> term
val mk_bv_one : int -> term
val mk_bv_const: string -> term
val mk_bv_conc : (int * term) -> (int * term) -> term
val mk_bv_extr : int * term -> int -> int -> term
val mk_bv_neg : int -> term -> term
val mk_bv_and : int -> term -> term -> term
val mk_bv_or : int -> term -> term -> term
val mk_bv_xor : int -> term -> term -> term

val width_of : term -> int option

val is_bv : term -> bool

val is_bv_const : term -> bool
val is_bv_zero : term -> bool
val is_bv_one : term -> bool
val is_bv_conc : term -> bool
val is_bv_extr : term -> bool
val is_bv_ite : term -> bool

val d_bv_const : term -> string
val d_bv_conc : term -> (int * term) list
val d_bv_extr : term -> (int * term) * int * int
val d_bv_ite : term -> int * term * term * term


    (*s Set of terms. *)

type terms

val terms_empty : unit -> terms
val terms_add : term -> terms -> terms
val terms_pp : terms -> unit
val terms_mem : term -> terms -> bool
val terms_sub : terms -> terms -> bool
val terms_is_empty : terms -> bool
val terms_to_list : terms -> term list
val terms_choose : terms -> term * terms

    
    (*s Constructors for the $<$, $\le$, $>$, $\ge$ constructors,
      respectively.
    *)

type cnstrnt

val cnstrnt_app : cnstrnt -> term -> term

val cnstrnt_pp : cnstrnt -> unit

    (*s [low_bound] is the type of lower bounds in intervals and they are
      either negative infinity, or a rational number together with a strictness attribute.
      The recognizers [low_bound_is_strict]
      and [low_bound_is_nonstrict] are only defined when [low_bound_is_neginf] does
      not hold. In these cases, one can access the lower bound using [low_bound_value] . *)
  
type low_bound

val low_bound_neginf : unit -> low_bound
val low_bound_strict : q -> low_bound
val low_bound_nonstrict : q -> low_bound
   

val low_bound_is_neginf : low_bound -> bool
val low_bound_is_strict : low_bound -> bool
val low_bound_is_nonstrict : low_bound -> bool
val low_bound_value : low_bound -> q

    (*s [high_bound] is the type of upper bounds in intervals and they are
      either positive infinity, or a rational number together with a strictness attribute.
      The recognizers [high_bound_is_strict]
      and [high_bound_is_nonstrict] are only defined when [high_bound_is_posinf] does
      not hold. In these cases, one can access the lower bound using [high_bound_value] . *)
  

type high_bound

val high_bound_posinf : unit -> high_bound
val high_bound_strict : q -> high_bound
val high_bound_nonstrict : q -> high_bound
 
  
val high_bound_is_posinf : high_bound -> bool
val high_bound_is_strict : high_bound -> bool
val high_bound_is_nonstrict : high_bound -> bool
val high_bound_value : high_bound -> q

    (*s Intervals are interpreted either over the reals, the integers, or the reals without
      the integers. *)
   
type interval_domain

val interval_domain_int: unit -> interval_domain
val interval_domain_real: unit -> interval_domain
val interval_domain_nonintreal: unit -> interval_domain

val interval_domain_is_real : interval_domain -> bool
val interval_domain_is_int : interval_domain -> bool
val interval_domain_is_nonintreal : interval_domain -> bool

      (*s Listify constraints as the disjunction of singleton constraints. *)

type interval = interval_domain * low_bound * high_bound

val cnstrnt_to_list : cnstrnt -> interval list
val cnstrnt_of_list : interval list -> cnstrnt

    (*s Constraint constructors. *)
        
val cnstrnt_lt : interval_domain -> q -> cnstrnt
val cnstrnt_le : interval_domain -> q -> cnstrnt
val cnstrnt_ge : interval_domain -> q -> cnstrnt
val cnstrnt_gt : interval_domain -> q -> cnstrnt

val cnstrnt_domain : interval_domain -> cnstrnt
val cnstrnt_int : cnstrnt
val cnstrnt_real : cnstrnt

val cnstrnt_openopen : interval_domain -> q -> q -> cnstrnt
val cnstrnt_openclosed : interval_domain -> q -> q -> cnstrnt
val cnstrnt_closedopen : interval_domain -> q -> q -> cnstrnt
val cnstrnt_closedclosed : interval_domain -> q -> q -> cnstrnt

     
   
    (*s Sets are build from the empty set [mk_empty] and the
      full set [mk_full], and the usual set operators. Actually,
      each set constructor name determines a family of
      constructors with a tag index of type [int] for determining
      the ``type'' of its element.
    *)

val mk_empty : int -> term
val mk_full : int -> term
   
val mk_finite : terms -> term
val mk_cnstrnt : cnstrnt -> term
   
val mk_setite : term -> term -> term -> term
val mk_compl : int -> term -> term
val mk_inter : int -> term -> term -> term
val mk_union : int -> term -> term -> term
val mk_diff : int -> term -> term -> term
val mk_sym_diff : int -> term -> term -> term
val mk_sub : int -> term -> term -> term
val mk_seteq : int -> term -> term -> term

val is_set : term -> bool

val is_empty : term -> bool
val is_full : term -> bool
val is_setite : term -> bool
val is_compl : term -> bool
val is_union : term -> bool
val is_inter : term -> bool
val is_finite : term -> bool
val is_cnstrnt : term -> bool

val d_empty : term -> int
val d_full : term -> int
val d_setite : term -> int * term
val d_compl : term -> int * term
val d_inter : term -> int * term * term
val d_union : term -> int * term * term
val d_finite : term -> terms
val d_cnstrnt : term -> cnstrnt

    
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
val subst_norm  : subst -> term -> term 


(*s Integer tag for each term, which is unique for each session. *)

val tag : term -> int  


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
val term_pp : term -> unit       
val eqn_pp : term * term -> unit 


(*s {\bf States.} A state or context can be thought of a function with
  terms as both domain and codomain. Such a context determines
  canonical representatives for terms. Contexts are represented
  by the abstract data type [state].  An empty context
  is built using the [init] operator. The canonical
  representative of a term in a given context is determined by
  [find]. [pp_find] prints the finite, non-identical part of the context
  on standard output. [state_eq] tests if two states are identical.
*)

type state = State.t

val state_eq : state -> state -> bool
 
val init : unit -> state
val find : state -> term -> term
val ext  : state -> term -> term list
val use  : state -> term -> term list
val uninterp : state -> term -> term list

val ctxt_of : state -> (term * term) list
val find_of : state -> subst
val ext_of : state -> terms map
val cnstrnt_of : state -> cnstrnt map
val use_of : state -> terms map

val state_pp : state -> unit
 
  
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
  | Consistent of State.t	
    
val is_consistent   : status -> bool
val is_redundant    : status -> bool
val is_inconsistent : status -> bool

val d_consistent : status -> state
    
val process : state -> term -> status

    
(*s In addition, a given state may be used to check for the validity
  or the unsatisfiability of a given proposition. *)

val is_valid : state -> term -> bool
val is_unsat : state -> term -> bool

    
(*s Simplifications.
     [norm] realizes encodes simplification by normalizing
     interpreted function applications according to their interpretation,
     and by replacing toplevel uninterpreted function symbols by their
     representatives with respect to the argument state.

     [canon] normalizes terms inside-out. A predicate [t] reduces
     to [true] if and only if it is valid in the argument state.

     [simplify] is weaker than [canon] in that it does not
     compute a canonical form, since, in contrast to  [canon], it
     does not process conditionals in a complete way.  Consequently, it
     is both more efficient than \texttt{canon} and the simplified term
     is guaranteed not to contain any new constant symbols.
  *)

val norm : state -> term -> term
val can : state -> term -> term
val simplify : term -> term

val is_solvable : term -> bool

val solve : term option -> state -> term * term -> subst

val solution : state -> term -> term option

val cnstrnt : state -> term -> cnstrnt
   

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

    
(*s Imperative API.  All the functions presented up to that point were
  purely functional and all the data-types were persistent. In the 
  following, we also provide an imperative API,
  for use in an imperative context of type [istate].
  There is a new data-type for states, [iinit], which is updated in-place.
  The function [current] returns the value of type [state] contained
  in a given imperative state, so that all the previous functions 
  ([canon], [is_valid], etc.) can be reused on an imperative state.
  The function call [iprocess s a] processes an atom [a] in a given 
  imperative state [s], returning a value [v] of type [v].
  If the result is a new consistent state, then [s] is updated 
  in-place, and the state contained in [v] can be discarded (it is now
  equal to [current s].  The imperative states are equipped with an
  internal notion of stack, handled using the [push] and
  [pop] functions.
  *)

type istate

val iinit : unit -> istate
val current : istate -> state
val push : istate -> unit
val pop : istate -> unit
val iprocess : istate -> term -> status

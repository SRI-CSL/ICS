
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

(*s Module [Ics]: description of the application programming interface.
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

(*s Three-valued logic. *)

type three

val three_yes : three
val three_no : three
val three_x : three

(*s Channels. *)

type inchannel
type outchannel

val stdin : unit -> inchannel
val stdout : unit -> outchannel
val stderr : unit -> outchannel
val in_of_string : string -> inchannel   (*s raises [Sys_error] *)
val out_of_string : string -> outchannel


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

(* Interval. *)

type dom

val dom_int : dom
val dom_real : dom

type endpoint

val endpoint_posinf : endpoint
val endpoint_neginf : endpoint
val endpoint_open : q -> endpoint
val endpoint_closed : q -> endpoint

type interval
type intervals

val interval_make : endpoint -> endpoint -> interval

(*s Names. *)

type name

val name_of_string : string -> name
val name_to_string : name -> string

type names

val names_singleton : string -> names
val names_add : string -> names -> names

(*s Map for names. *)

type 'a namemap


(*s Constraints. *)

type cnstrnt

(*s Attributes. *)

type attribute

val attribute_mk_a : attribute
val attribute_mk_c : attribute
val attribute_mk_ac : attribute 
val attribute_mk_top : attribute


(*s Arities. *)

type arity

val arity_of_string : string -> arity
val arity_input : inchannel -> arity
val arity_output : outchannel -> arity -> unit

val arity_mk_constant : cnstrnt -> arity
val arity_mk_functorial : cnstrnt list -> cnstrnt -> arity

   
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

val term_of_string : string -> term
val term_input : inchannel -> term
val term_output : outchannel -> term -> unit

    (*s Function applications are created using the [mk_app~f~l]
      constructor.  In general, the name of the function [f] is
      specified by an arbitrary term and not only by a function
      symbol. Function update [update~f~x~a] specifices the
      update of function [f] at position [x] with value [a].
    *)

val mk_uninterp : string -> arity -> term list -> term


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
val mk_multq : q -> term -> term
val mk_add : term -> term -> term    
val mk_sub : term -> term -> term
val mk_unary_minus : term -> term

val is_arith : term -> bool

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

    (*s Terms representing the usual propositional constants *)
    
val mk_true  : unit -> term
val mk_false : unit -> term

val is_true : term -> bool
val is_false : term -> bool

     (*s Builtin "uninterpreted" terms. *)

val mk_unsigned : term -> term

    (*s Bitvectors *)

val mk_bvconst : string -> term
val mk_bvsub : int -> int -> int -> term -> term
val mk_bvconc : int -> int -> term -> term -> term
val mk_bwite : int -> term -> term -> term -> term
val mk_bwand : int -> term -> term -> term
val mk_bwor : int -> term -> term -> term
val mk_bwxor : int -> term -> term -> term
val mk_bwnot : int -> term -> term

    (*s Enumerations. *)

val mk_enum : names -> string -> term

    (*s Equality and disequality. [mk_equal a b] constructs equalites,
        whereas [mk_diseq a b] yields a disequality of the form
        [mk_not(mk_equal a b)]. *)

type atom
type atoms
      
val mk_equal  : term -> term -> atom
val mk_diseq  : term -> term -> atom
val mk_in  : cnstrnt -> term -> atom

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

val mk_real : term -> atom
    
val mk_lt : term -> term -> atom
val mk_le : term -> term -> atom
val mk_gt : term -> term -> atom
val mk_ge : term -> term -> atom

(*s Set of terms. *)

type terms

val terms_empty : unit -> terms
val terms_add : term -> terms -> terms
val terms_pp : outchannel -> terms -> unit
val terms_mem : term -> terms -> bool
val terms_sub : terms -> terms -> bool
val terms_is_empty : terms -> bool
val terms_to_list : terms -> term list
val terms_of_list : term list -> terms
val terms_choose : terms -> term * terms
    
(*s Maps with terms as domain. *)

type 'a map

val map_empty : unit -> 'a map
val map_add : term -> 'a -> 'a map -> 'a map
val map_is_empty : 'a map -> bool
val map_find : term -> 'a map -> 'a
val map_remove : term -> 'a map -> 'a map
val map_mem :  term -> 'a map -> bool
val map_to_list : 'a map -> (term * 'a) list
val map_pp : (outchannel -> 'a -> unit) -> outchannel -> 'a map -> unit

(*s Propositions. *)

type prop 
    
(*s Substititon. *)
    
type subst

val subst_empty : unit -> subst
val subst_add   : term -> term -> subst -> subst
val subst_mem   : subst -> term -> bool
val subst_find  : subst -> term -> term
val subst_apply : subst -> term -> term
val subst_of_list : (term * term) list -> subst
val subst_to_list : subst -> (term * term) list
val subst_pp    : outchannel -> subst -> unit

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

val state_ctxt_of : state -> atoms
val state_diseqs_of : state -> terms map
val state_cnstrnts_of : state -> cnstrnt map
val state_diseqs : state -> term -> terms
  
(*s The operation [process] adds a new proposition to a state.
  The codomain of this function is of type [status], elements of
  which represent the three possible outcomes of processing a
  proposition: 1. the argument is inconsistent in the
  current context, 2. it is valid, or, 3., it is satisfiable but
  not valid. In the third case, a modified state is obtained using
  the destructor [d_consistent].
*)

type status
    
val is_consistent   : status -> bool
val is_redundant    : status -> bool
val is_inconsistent : status -> bool

val d_consistent : status -> state
    
val process : state -> atom -> status


(*s [can] normalizes terms inside-out. The resulting term may contain *)
(*s fresh variables as introduced by solvers but no rename variables. *)

val can : state -> term -> term

(*s Computing a best constraint in a given state. *)

val cnstrnt : state -> term -> cnstrnt

(*s Theories. *)

type theory

val theory_a : theory
val theory_t : theory
val theory_b : theory
val theory_e : theory
val theory_bv : theory
val theory_nla : theory
val theory_of_term : term -> theory


(*s Command interface for manipulating global state. *)

type istate

val istate_current : unit -> state

val istate_def : name -> term -> unit
val istate_sig : name -> arity -> unit
val istate_type : name -> cnstrnt -> unit
val istate_set_in_channel : inchannel -> unit
val istate_set_out_channel : outchannel -> unit
val istate_flush : unit -> unit
val istate_nl : unit -> unit
val istate_can : term -> term
val istate_process : prop -> status
(* val istate_check : names -> term -> (term * cnstrnt) map status *)
val istate_ext : term -> terms
val istate_reset : unit -> unit
val istate_save : name -> unit
val istate_restore : name -> unit
val istate_remove : name -> unit
val istate_forget : unit -> unit
val istate_sub : name -> name -> three
val istate_use_of : theory -> terms map
val istate_use : theory -> term -> terms
val istate_find_of : theory -> term map
val istate_find : theory -> term -> term


(*s Execute a command. *)

val istate_eval : unit -> unit

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

val do_at_exit : unit -> unit
    
val flush : unit -> unit

    
(*s Lists. *)

val is_nil : 'a list -> bool
val cons : 'a -> 'a list -> 'a list
val head : 'a list -> 'a
val tail : 'a list -> 'a list

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


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


(*s Module [Ics]: the application programming interface to ICS for 
 asserting formulas to a logical context, switching between different
 logical contexts, and functions for manipulating and normalizing terms.

 There are two sets of interface functions.  The functional interface
 provides functions for building up the main syntactic categories of
 ICS such as terms, atoms, and propositions and for extending
 logical contexts using [process], which is side-effect free.

 In contrast to this functional interface, the command interface
 manipulates a global state consisting, among others, of symbol tables 
 and the current logical context.  The [istate_eval] procedure, which 
 reads commands from the current input channel and manipulates the global
 structures accordingly, is used to implement the ICS interactor.

 Besides functions for manipulating ICS datatypes, this interface also
 contains a number of standard datatypes such as channels, multiprecision 
 arithmetic, tuples, and lists.
*)
 
 
(*s Initialization. [init n] sets the verbose level to [n]. The higher
 the verbose level, the more trace information is printed to [stderr]
 (see below). There are no trace messages for [n = 0]. In addition, 
 initialization makes the system to raise the [Sys.Break] exception upon
 user interrupt [^C^C].  The [init] function should be called before
 using any other function in this API. *)

val init : int -> unit


(*s Controls. [reset] clears all the global tables. This
 does not only include the current context but also internal tables used 
 for hash-consing and memoization purposes.
 [gc] triggers a full major collection of ocaml's garbage collector.
 Finally, [set_verbose] controls the amount of trace messages.
 The default is [0], which means that nothing is printed at all.
 The higher the value, the more numerous the messages are. *)

val reset : unit -> unit
    
val gc : unit -> unit
    
val set_verbose : int -> unit

val do_at_exit : unit -> unit
    
val flush : unit -> unit


(*s Channels. [inchannel] is the type of input channels. A channel
 of name [str] is opened with [in_of_string str]. This function 
 raises [Sys_error] in case such a channel can not be opened.
 [outchannel] is the type of formatting output channels, and channels
 of this type are opened with [out_of_string].  [stdin], [stdout],
 and [stderr] are predefined channels for standard input, standard
 output, and standard error. *)

type inchannel
type outchannel

val stdin : unit -> inchannel
val stdout : unit -> outchannel
val stderr : unit -> outchannel
val in_of_string : string -> inchannel 
val out_of_string : string -> outchannel


(*s Multi-precision rational numbers. [num_of_int n]
  injects an integer into this type, [num_of_ints n m], for [m <> 0],
  constructs a normalized representation of the rational [n/m] in [q],
  [string_of_num q] constructs a string (usually for printout) of a
  rational number, and [num_of_string s] constructs a rational, whenever
  [s] is of the form ["n/m"] where [n] and [m] are naturals. *)
    
type q

val num_of_int : int -> q
val num_of_ints : int -> int -> q
val ints_of_num : q -> string * string
val string_of_num : q -> string
val num_of_string : string -> q


(*s Names. [name_of_string] and [name_to_string] coerce between the
 datatypes of strings and names.  These coercions are inverse to each
 other.  [name_eq] tests for equality of names in constant time. *)

type name

val name_of_string : string -> name
val name_to_string : name -> string
val name_eq : name -> name -> bool


(*s Constraints on reals are defined by the grammar
      \begin{verbatim}
      <cnstrnt> ::= <dom> [<intervals>] 
      \end{verbatim}
    where [<intervals>] and [<dom>] are defined above. A real
    number is in such a constraint if it satisfies both the domain
    and the interval constraint.  [cnstrnt_of_string str] parses
    the string [str] according to this grammar and produces the
    corresponding constraint representation. Likewise, [cnstrnt_input in]
    parses the concrete syntax of constraints from the input channel [in]. 
    Constraints [c] are printed to output channel [out] using [cnstrnt_output out c]
    and to the standard output using [cnstrnt_pp c].
 *)

type cnstrnt

val cnstrnt_of_string : string -> cnstrnt
val cnstrnt_input : inchannel -> cnstrnt
val cnstrnt_output : outchannel -> cnstrnt -> unit
val cnstrnt_pp : cnstrnt -> unit

val cnstrnt_mk_int : unit -> cnstrnt
val cnstrnt_mk_nat : unit -> cnstrnt
val cnstrnt_mk_singleton : q -> cnstrnt
val cnstrnt_mk_diseq : q -> cnstrnt
val cnstrnt_mk_oo : q -> q -> cnstrnt
val cnstrnt_mk_oc : q -> q -> cnstrnt
val cnstrnt_mk_co : q -> q -> cnstrnt
val cnstrnt_mk_cc : q -> q -> cnstrnt
val cnstrnt_mk_lt : q -> cnstrnt
val cnstrnt_mk_le : q -> cnstrnt

val cnstrnt_union : cnstrnt -> cnstrnt -> cnstrnt
val cnstrnt_inter : cnstrnt -> cnstrnt -> cnstrnt
val cnstrnt_compl : cnstrnt -> cnstrnt

val cnstrnt_add : cnstrnt -> cnstrnt -> cnstrnt
val cnstrnt_multq : q -> cnstrnt -> cnstrnt
val cnstrnt_mult : cnstrnt -> cnstrnt -> cnstrnt
val cnstrnt_div : cnstrnt -> cnstrnt -> cnstrnt

   
(*s Terms. Terms are either variables, uninterpreted function application, 
 or interpreted constants and operators drawn from a combination of theories.
 Currently, ICS supports linear arithmetic, both rational and integer,
 tuples, function (array update) update, sets, and bitvectors.

 Terms are build using constructors, whose names are all of the form [mk_xxx].
 For each constructor [mk_xxx] there is a corresponding recognizer [is_xxx]
 which reduces to true if its argument term has been built
 with the constructor [mk_xxx]. Moreover, for each constructor
 [mk_xxx}] above there is a corresponding desctructor [d_xxx$]
 for analyzing the components of such a constructor term. *)


type term

val term_of_string : string -> term
val term_input : inchannel -> term
val term_output : outchannel -> term -> unit
val term_pp : term -> unit

(*s Equality and Comparison.
  Both the equality test [eq] and comparison [cmp] on terms
  works in constant time. Comparison [cmp a b] returns either
  [-1], [0], or [1] depending on whether [a] is less than [b],
  the arguments are equal, or [a] is greater than [b]. *)

val term_eq : term -> term -> bool
val term_cmp : term -> term -> int

(*s Function applications are created using the [mk_app~f~l]
  constructor.  In general, the name of the function [f] is
  specified by an arbitrary term and not only by a function
  symbol. Function update [update~f~x~a] specifices the
  update of function [f] at position [x] with value [a]. *)

val term_mk_var : string -> term

val term_mk_uninterp : string -> term list -> term


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
 
val term_mk_num : q -> term
val term_mk_multq : q -> term -> term
val term_mk_add : term -> term -> term 
val term_mk_addl : term list -> term   
val term_mk_sub : term -> term -> term
val term_mk_unary_minus : term -> term

val term_is_arith : term -> bool

    (*s Tuples are built using the [mk_tuple] constructor, and
      projection of the [i]-th component of a tuple [t] of
      length [n] is realized using [mk_proj i n t];
      [is_tuple a] and [is_proj a] are the respective recognizers,
      while [d_tuple] returns the tuple entries as a list, and
      [d_proj] returns the triple [(i,n,a)] for a projection
      constructed with [mk_proj i n a].
    *)
    
val term_mk_tuple : term list -> term
val term_mk_proj : int -> int -> term -> term

    (*s Terms representing the usual propositional constants *)
    
val term_mk_true  : unit -> term
val term_mk_false : unit -> term

val term_is_true : term -> bool
val term_is_false : term -> bool
 
     (*s Addtional nonlinear terms. *)

val term_mk_mult : term -> term -> term
val term_mk_multl : term list -> term
val term_mk_expt : term -> term -> term


     (*s Builtin "uninterpreted" terms. *)

val term_mk_unsigned : term -> term

val term_mk_update : term -> term -> term -> term
val term_mk_select : term -> term -> term


    (*s Bitvectors *)

val term_mk_bvconst : string -> term
val term_mk_bvsub : (int * int * int) -> term -> term
val term_mk_bvconc : int * int -> term -> term -> term
val term_mk_bwite : int -> term * term * term -> term
val term_mk_bwand : int -> term -> term -> term
val term_mk_bwor : int -> term -> term -> term
val term_mk_bwxor : int -> term -> term -> term
val term_mk_bwnot : int -> term -> term

    (*s Equality and disequality. [mk_equal a b] constructs equalites,
        whereas [mk_diseq a b] yields a disequality of the form
        [mk_not(mk_equal a b)]. *)

type atom
type atoms

val atom_pp : atom -> unit
      
val atom_mk_equal  : term -> term -> atom
val atom_mk_diseq  : term -> term -> atom
val atom_mk_in  : cnstrnt -> term -> atom
val atom_mk_true : unit -> atom
val atom_mk_false : unit -> atom

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

val atom_mk_real : term -> atom
val atom_mk_int : term -> atom
    
val atom_mk_lt : term -> term -> atom
val atom_mk_le : term -> term -> atom
val atom_mk_gt : term -> term -> atom
val atom_mk_ge : term -> term -> atom
val atom_neg : atom -> atom


(*s Sets of terms. *)

type terms

(*s Term maps. *)

type 'a map

(*s Propositions. *)

type prop 

val prop_mk_true : prop
val prop_mk_false : prop
val prop_mk_poslit : atom -> prop
val prop_mk_neglit : atom -> prop
val prop_mk_ite : atom -> prop -> prop -> prop
val prop_mk_neg : prop -> prop
val prop_mk_conj : prop -> prop -> prop
val prop_mk_disj : prop -> prop -> prop
val prop_mk_xor : prop -> prop -> prop
val prop_mk_imp : prop -> prop -> prop
val prop_mk_iff : prop -> prop -> prop




(*s A [state] or context can be thought of a function with
  terms as both domain and codomain. Such a context determines
  canonical representatives for terms. Contexts are represented
  by the abstract data type [state].  An empty context
  is built using the [init] operator. The canonical
  representative of a term in a given context is determined by
  [find]. [pp_find] prints the finite, non-identical part of the context
  on standard output. [state_eq] tests if two states are identical.
*)

type state

val state_eq : state -> state -> bool
 
val state_empty : unit -> state

val state_ctxt_of : state -> atoms
val state_diseqs_of : state -> terms map
val state_cnstrnts_of : state -> cnstrnt map
val state_pp : state -> unit
  

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

val can : state -> atom -> state * atom

(*s Computing a best constraint in a given state. *)

val cnstrnt : state -> term -> Number.t option


(*s Command interface for manipulating global state. *)

type istate

val istate_current : unit -> state

val istate_def : name -> term -> unit
val istate_sig : name -> int -> unit
val istate_type : name -> Number.t -> unit
val istate_set_in_channel : inchannel -> unit
val istate_set_out_channel : outchannel -> unit
val istate_flush : unit -> unit
val istate_nl : unit -> unit
val istate_can : term -> term
val istate_process : prop -> status
(* val istate_check : names -> term -> (term * cnstrnt) map status *)
val istate_reset : unit -> unit
val istate_save : name -> unit
val istate_restore : name -> unit
val istate_remove : name -> unit
val istate_forget : unit -> unit

(*s Eval. [istate_eval] reads a command from the current
 input channel, modifies the current internal [istate] accordingly,
 and outputs the result to the current output channel. *)

val istate_eval : unit -> unit

    
(*s Lists. *)

val is_nil : 'a list -> bool
val cons : 'a -> 'a list -> 'a list
val head : 'a list -> 'a
val tail : 'a list -> 'a list

(*s Pairs. [pair a b] builds a pair [(a,b)] and
 [fst (pair a b)] returns [a] and [snd (pair b a)]
 returns [b]. *)

val pair : 'a -> 'b -> 'a * 'b
val fst : 'a * 'b -> 'a
val snd : 'a * 'b -> 'b


(*s Triples. Accessors for triples [(a,b,c)]. *)

val triple : 'a -> 'b -> 'c -> 'a * 'b * 'c
val fst_of_triple : 'a * 'b *'c -> 'a
val snd_of_triple : 'a * 'b *'c -> 'b
val third_of_triple : 'a * 'b *'c -> 'c


(*s Quadruples. Accessors for quadruples [(a,b,c,d)]. *)
  
val fst_of_quadruple : 'a * 'b * 'c *'d -> 'a
val snd_of_quadruple : 'a * 'b * 'c *'d -> 'b
val third_of_quadruple : 'a * 'b * 'c *'d -> 'c
val fourth_of_quadruple : 'a * 'b * 'c *'d -> 'd
    
      
(*s Options. An element of type ['a option] either satisfies
 the recognizer [is_some] or [is_none].  In case, [is_some]
 holds, a value of type ['a] can be obtained by [value_of].  *)

val is_some : 'a option -> bool
val is_none : 'a option -> bool

val value_of : 'a option -> 'a

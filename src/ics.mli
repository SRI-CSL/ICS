
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
 ICS such as terms and atoms, and for extending logical contexts 
 using [process],  which is side-effect free.

 In contrast to this functional interface, the command interface
 manipulates a global state consisting, among others, of symbol tables 
 and the current logical context.  The [istate_eval] procedure, which 
 reads commands from the current input channel and manipulates the global
 structures accordingly, is used to implement the ICS interactor.

 Besides functions for manipulating ICS datatypes, this interface also
 contains a number of standard datatypes such as channels, multiprecision 
 arithmetic, tuples, and lists.
*)
 


(*s Controls. [reset] clears all the global tables. This does not only 
 include the current context but also internal tables used  for hash-consing 
 and memoization purposes. [gc] triggers a full major collection of ocaml's garbage 
 collector. Finally, [set_verbose] controls the amount of trace messages.
 The default is [0], which means that nothing is printed at all.
 The higher the value, the more numerous the messages are.  
 [do_at_exit] clears out internal data structures. *)

val reset : unit -> unit
    
val gc : unit -> unit

val do_at_exit : unit -> unit


(*s Rudimentary control on tracing. *)
     
type trace_level = string

val trace_reset : unit -> unit
val trace_add : trace_level -> unit
val trace_remove : trace_level -> unit
val trace_get : unit -> trace_level list


(*s Channels. [inchannel] is the type of input channels. A channel
 of name [str] is opened with [in_of_string str]. This function 
 raises [Sys_error] in case such a channel can not be opened.
 [outchannel] is the type of formatting output channels, and channels
 of this type are opened with [out_of_string].  [stdin], [stdout],
 and [stderr] are predefined channels for standard input, standard
 output, and standard error. [flush] flushes the [stdout] channel. *)

type inchannel = in_channel
type outchannel = Format.formatter

val channel_stdin : unit -> inchannel
val channel_stdout : unit -> outchannel
val channel_stderr : unit -> outchannel
val inchannel_of_string : string -> inchannel 
val outchannel_of_string : string -> outchannel
  
val flush : unit -> unit


(*s Initialization. [init n] sets the verbose level to [n]. The higher
 the verbose level, the more trace information is printed to [stderr]
 (see below). There are no trace messages for [n = 0]. In addition, 
 initialization makes the system to raise the [Sys.Break] exception upon
 user interrupt [^C^C].  The [init] function should be called before
 using any other function in this API. *)

val init : int * bool * string * inchannel * outchannel -> unit


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


(*s Arithmetic constraints. A constraint consists of a domain
 restriction [Int] or [Real] , a real interval, and a set of 
 disequality numbers. A real number satisfies such a constraint
 if, first, it satisfies the domain restriction, second, it 
 is a member of the interval, and, third, it is none of the 
 numbers in the disequality set.

 [cnstrnt_of_string str] parses the string [str] according to
 the nonterminal [cnstrnteof] in module [Parser] (see its specification
 in file [parser.mly]) and produces the corresponding constraint representation. 
 In contrast, [cnstrnt_input in] parses the concrete syntax of constraints from 
 the input channel [in]. Constraints [c] are printed to the output channel [out] 
 using [cnstrnt_output out c] and to the standard output using [cnstrnt_pp c].

 For the definition of constraint constructors see Module [Cnstrnt]. 
 [cnstrnt_mk_int()] constructs an integer constraint, [cnstrnt_mk_nat()]
 a constraint for the natural numbers, [cnstrnt_mk_singleton q] is the
 constraint which holds only of [q], [cnstrnt_mk_diseq q] holds for all
 reals except for [q], [cnstrnt_mk_oo l h] constructs an open interval
 with lower bound [l] and upper bound [h], [cnstrnt_mk_oc l h] is the
 left-open, right-closed interval with lower bound [l] and upper bound [h], 
 [cnstrnt_mk_co l h] is the left-closed, right-open interval with lower 
 bound [l] and upper bound [h], [cnstrnt_mk_cc l h] is the closed interval 
 with lower bound [l] and upper bound [h]. 

 The intersection of two constraints [c], [d] is computed by [cnstrnt_inter c d],
 that is, a real [q] is in both [c] and [d] iff it is in  [cnstrnt_inter c d].
 
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
val cnstrnt_mk_gt : q -> cnstrnt
val cnstrnt_mk_ge : q -> cnstrnt

val cnstrnt_inter : cnstrnt -> cnstrnt -> cnstrnt

(*s Abstract interval interpretation. A real number [x] is in
 [cnstrnt_add c d] iff there are real numbers [y] in [c] and 
 [z] in [d] such that [x = y + x]. Likewise, [x] is in 
 [cnstrnt_multq q c] iff there exists [y] in [c] such that
 [x = q * y].  In constrast to these exact abstract operators,
 [cnstrnt_mult] and [cnstrnt_div] compute overaprroximations,
 that is, if [x] in [c] and [y] in [d] then there exists
 a [z] in [cnstrnt_mult c d] ([cnstrnt_div c d]) such that
 [z = x * y] ([z = x/y]). *)

val cnstrnt_add : cnstrnt -> cnstrnt -> cnstrnt
val cnstrnt_multq : q -> cnstrnt -> cnstrnt
val cnstrnt_mult : cnstrnt -> cnstrnt -> cnstrnt
val cnstrnt_div : cnstrnt -> cnstrnt -> cnstrnt

   
(*s Terms. Terms are either variables, application of uninterpreted functions, 
 or interpreted constants and operators drawn from a combination of theories.
 The interpreted operators are drawn from the theory of arithmetic, tuples,
 propostional constants, and bitvectors.

 [term_of_string] parses a string according to the grammar for the nonterminal
 [termeof] in module [Parser] (see its specification in file [parser.mly]) and
 builds a corresponding term. Similary, [term_input] builds a term by reading
 from an input channel. 

 [term_output out a] prints term [a] on the output channel [out], and [term_pp a]
 is equivalent to [term_output stdout a]. 

 Terms are build using constructors, whose names are all of the form [mk_xxx].
 For each constructor [mk_xxx] there is a corresponding recognizer [is_xxx]
 which reduces to true if its argument term has been built
 with the constructor [mk_xxx]. Moreover, for each constructor
 [mk_xxx}] above there is a corresponding desctructor [d_xxx]
 for analyzing the components of such a constructor term.  *)


type term

val term_of_string : string -> term
val term_to_string : term -> string
val term_input : inchannel -> term
val term_output : outchannel -> term -> unit
val term_pp : term -> unit

(*s Equality and Comparison. Comparison [cmp a b] returns either
  [-1], [0], or [1] depending on whether [a] is less than [b],
  the arguments are equal, or [a] is greater than [b] according
  to the builtin term ordering (see [Term.(<<<)]). [term_eq a b] 
  is true iff if [term_cmp a b] returns [0]. *)

val term_eq : term -> term -> bool
val term_cmp : term -> term -> int


(*s Given a string [s], [term_mk_var s] constructs a 
 variable with name [s] and [term_mk_uninterp s al]
 constructs an application of an uninterpreted function
 symbol [s] to a list of argument terms.  If [s] is any
 of the builtin function symbols specified in module 
 [Builtin], the builtin simplifications are applied to
 this application. *)

val term_mk_var : string -> term

val term_mk_uninterp : string -> term list -> term


(*s Arithmetic terms include rational constants built from
 [term_mk_num q], linear multiplication [term_mk_multq q a],
 addition [term_mk_add a b] of two terms, n-ary 
 addition [term_mk_addl al] of a list of terms [al],
 subtraction [term_mk_sub a b] of term [b] from term [a],
 negation [term_mk_unary_minus a], multiplication
 [term_mk_mult a b], and exponentiation [term_mk_expt n a].
 These constructors build up arithmetic terms in a canonical
 form as defined in module [Arith]. [term_is_arith a] holds
 iff the toplevel function symbol of [a] is any of the 
 function symbols interpreted in the theory of arithmetic. *)
 

val term_mk_num : q -> term
val term_mk_multq : q -> term -> term
val term_mk_add : term -> term -> term 
val term_mk_addl : term list -> term   
val term_mk_sub : term -> term -> term
val term_mk_unary_minus : term -> term
val term_mk_mult : term -> term -> term
val term_mk_multl : term list -> term
val term_mk_expt : int -> term -> term

val term_is_arith : term -> bool


(*s Tuples are built using the [mk_tuple] constructor, and
  projection of the [i]-th component of a tuple [t] of
  length [n] is realized using [mk_proj i n t]. *)
    
val term_mk_tuple : term list -> term
val term_mk_proj : int -> int -> term -> term

(*s Boolean constants. *)
    
val term_mk_true  : unit -> term
val term_mk_false : unit -> term

val term_is_true : term -> bool
val term_is_false : term -> bool
 

    (*s Bitvectors *)

val term_mk_bvconst : string -> term
val term_mk_bvsub : (int * int * int) -> term -> term
val term_mk_bvconc : int * int -> term -> term -> term
val term_mk_bwite : int -> term * term * term -> term
val term_mk_bwand : int -> term -> term -> term
val term_mk_bwor : int -> term -> term -> term
val term_mk_bwnot : int -> term -> term

(*s Set of terms. *)

type terms


(*s Atoms. [atom_mk_true()] is the trivially true atom,
 [atom_mk_false()] is the trivially false atom, and 
 given terms [a], [b], the constructor [mk_equal a b]
 constructs an equality constraint, [mk_diseq a b] a 
 disequality constraint, and [atom_mk_in a c] constructs
 a membership constraint for [a] and a arithmetic constraint [c]. 
 Atoms are printed to [stdout] using [atom_pp]. *)

type atom

val atom_pp : atom -> unit

val atom_of_string : string -> atom
val atom_to_string : atom -> string
      
val atom_mk_equal  : term -> term -> atom
val atom_mk_diseq  : term -> term -> atom
val atom_mk_in  : cnstrnt -> term -> atom
val atom_mk_true : unit -> atom
val atom_mk_false : unit -> atom


(*s Derived atomic constraints. [atom_mk_int t] restricts the domain of 
 interpretations of term [t] to the integers. Similarly, [atom_mk_real] 
 restricts its argument to the real numbers. [atom_mk_lt a b] generates
 the constraint ['a' < 'b'],  [atom_mk_le a b] yields ['a' <= 'b'], 
 [atom_mk_gt a b] yields ['a' > 'b'], and [atom_mk_ge a b] yields ['a' >= 'b'].
 *)

val atom_mk_real : term -> atom
val atom_mk_int : term -> atom
    
val atom_mk_lt : term -> term -> atom
val atom_mk_le : term -> term -> atom
val atom_mk_gt : term -> term -> atom
val atom_mk_ge : term -> term -> atom


(*s Solution sets. *)

type solution

val solution_apply : solution -> term -> term

val solution_find : solution -> term -> term

val solution_inv : solution -> term -> term

val solution_mem : solution -> term -> bool

val solution_occurs : solution -> term -> bool

val solution_use : solution -> term -> terms

val solution_is_empty : solution -> bool



(*s Logical context.  An element of type [state] is a logical
 context with [state_empty] the empty context. [state_eq s1 s2]
 is a constant-time predicate for testing for identity of two
 states. Thus, whenever this predicate holds its arguments states
 are equivalent, but not necessarily the other way round. Logical
 contexts are printed using [state_pp]. The set of atoms in a
 context [s] are obtained with [state_ctxt_of s]. *)
 

type context

val context_eq : context -> context -> bool
 
val context_empty : unit -> context

val context_ctxt_of : context -> atom list
val context_u_of : context -> solution
val context_a_of : context -> solution
val context_t_of : context -> solution
val context_bv_of : context -> solution
val context_pp : context -> unit


(*s Builtin simplifying constructors. *)

val term_mk_unsigned : context -> term -> term

val term_mk_update : context -> term * term * term -> term
val term_mk_select :  context -> term * term -> term

val term_mk_div :  context -> term -> term -> term
val term_mk_floor : context -> term -> term
val term_mk_ceiling : context -> term -> term

val term_mk_sin : context -> term -> term
val term_mk_cos : context -> term -> term


(*s The operation [process s a] adds a new atom [a] to a logical context [s].
  The codomain of this function is of type [status], elements of
  which represent the three possible outcomes of processing a
  proposition: 1. the atom [a] is inconsistent in [s], 2. it is valid, or, 
  3., it is satisfiable but not valid. In the third case, a modified state 
  is obtained using the destructor [d_consistent]. *)

type status
    
val is_consistent   : status -> bool
val is_redundant    : status -> bool
val is_inconsistent : status -> bool

val d_consistent : status -> context
    
val process : context -> atom -> status

(*s Suggesting finite case split. *)

val split : context -> atom list

(*s Canonization. Given a logical context [s] and an atom [a],
 [can s a] computes a semicanonical form of [a] in [s], that is,
 if [a] holds in [s] it returns [Atom.True], if the negation of [a]
 holds in [s] then it returns [Atom.False], and, otherwise, an
 equivalent normalized atom built up only from variables is 
 returned. The returned logical state might contain fresh variables. *)

val can : context -> atom -> context * atom

(*s Given a logical context [s] and a term [a], [cnstrnt s a]
 computes the best possible arithmetic constraint for [a] in [s]
 using constraint informatin in [s] and abstraction interval interpretation.
 If no such constraint can be deduced, [None] is returned. *)

val cnstrnt : context -> term -> cnstrnt option


(*s An imperative state [istate] does not only include a logical 
 context of type [state] but also a symbol table and input and output 
 channels. A global [istate] variable is manipulated and
 destructively updated by commands. *)


(*s [cmd_eval] reads a command from the current input channel according
 to the grammar for the nonterminal [commandeof] in module [Parser] (see
 its specification in file [parser.mly], the current internal [istate] 
 accordingly, and outputs the result to the current output channel. *)

val cmd_rep : unit -> unit


(*s Sleeping for a number of seconds. *)

val sleep : int -> unit

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

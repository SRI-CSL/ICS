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


(** Application programming interface.

  The ICS API includes function for
  - asserting formulas to a logical context, 
  - switching between different logical contexts, and 
  - manipulating and normalizing terms.

  There are two sets of interface functions.  The functional interface
  provides functions for building up the main syntactic categories of
  ICS such as terms and atoms, and for extending logical contexts 
  using {!Ics.process},  which is side-effect free.

  In contrast to this functional interface, the command interface
  manipulates a global state consisting, among others, of symbol tables 
  and the current logical context.  The {!Ics.cmd_rep} procedure, which 
  reads commands from the current input channel and manipulates the global
  structures accordingly, is used to implement the ICS interactor.

  Besides functions for manipulating ICS datatypes, this interface also
  contains a number of standard datatypes such as channels, multiprecision 
  arithmetic, tuples, and lists.
*)
 


(** {6 Control flags} *)

val set_maxloops : int -> unit
  (** [set_maxloops n] determines an upper number of loops in
    the main ICS loop. [n < 0] determines that there is no such
    bound; this is also the default. *)


(** {6 Channels} *)

type inchannel = in_channel
    (** [inchannel] is the type of input channels. *)

type outchannel = Format.formatter
    (** Formattable output channel. *)

val channel_stdin : unit -> inchannel
  (** [channel_stdin] is the predefined standard input channel. *)

val channel_stdout : unit -> outchannel
  (** [channel_stdout] is the predefined standard output channel. *)

val channel_stderr : unit -> outchannel
  (** [channel_stdout] is the predefined standard error channel.
    All ICS trace messages are put onto this channel. *)

val inchannel_of_string : string -> inchannel 
  (** [inchannel_of_string str] opens an input
    channel for reading from a string (file name).  This function 
    raises [Sys_error] in case such a channel can not be opened. *)

val outchannel_of_string : string -> outchannel
  (** [outchannel_of_string str] opens an output
    channel for writing from a string (file name).  This function 
    raises [Sys_error] in case such a channel can not be opened. *)


(** {6 Multi-precision arithmetic} *)

type q
  (** Type for representing the rational numbers. *)

val num_of_int : int -> q
  (** [num_of_int n] constructs a rational from the integer [n]. *)

val num_of_ints : int -> int -> q
  (** [num_of_ints n m], for [m <> 0], constructs a normalized 
    representation of the rational [n/m] in [q]. *)

val ints_of_num : q -> string * string
  (** [ints_of_num q] decomposes a rational with numerator [n] and
    denumerator [m] into [("n", "m")]. *)

val string_of_num : q -> string
  (** [string_of_num q] constructs a string (usually for printout) of a
    rational number *)
  
val num_of_string : string -> q
  (** [num_of_string s] constructs a rational, whenever
    [s] is of the form [n/m] where [n] and [m] are integers. *)
    

(** {6 Names} *)

(** Names. [name_of_string] and [name_to_string] coerce between the
 datatypes of strings and names.  These coercions are inverse to each
 other.  [name_eq] tests for equality of names in constant time. *)

type name
  (** Representation of strings. *)

val name_of_string : string -> name
  (** [name_of_string str] constructs a name [n] from a string
    such that {!Ics.name_to_string}[(n)] yields [str]. *)

val name_to_string : name -> string
  (** [name_to_string n] is the inverse operation of {!Ics.name_to_string}. *)

val name_eq : name -> name -> bool
  (** [name_eq n m] holds iff the corresponding 
    strings {!Ics.name_to_string}[(n)] and  {!Ics.name_to_string}[(m)]
    are equal.  This equality test is constant in the length of strings. *)


(** {6 Arithmetic Constraints} *)

(** An {b arithmetic constraint} consists of 
  - a domain restriction,
  - an interval with rational or unbounded endpoints, 
  - and a set of rational disequality numbers. 
  A real number satisfies such a constraint if
  - it satisfies the domain restriction,
  - it is a member of the interval, 
  - it is none of the numbers in the disequality set.
  To each constraint we associate the set of all reals satisfying
  these requirements. For more details see description of 
  module {!Cnstrnt}.
*)


type cnstrnt

val cnstrnt_of_string : string -> cnstrnt
  (** [cnstrnt_of_string str] parses the string [str] according to
    the parsing function {!Parser.cnstrnteof} (see specification of
    the nonterminal [cnstrnteof] in file [parser.mly]) and produces 
    the corresponding constraint representation. *)

val cnstrnt_input : inchannel -> cnstrnt
  (** In contrast, [cnstrnt_input in] parses the concrete 
    syntax of constraints from the input channel [in].  *)

val cnstrnt_output : outchannel -> cnstrnt -> unit
  (** Constraints [c] are printed to the output channel [out] 
    using [cnstrnt_output out c] *)

val cnstrnt_pp : cnstrnt -> unit
  (** [cnstrnt_pp c] prints [c] on {!Ics.channel_stdout}. *)

val cnstrnt_mk_int : unit -> cnstrnt
  (** [cnstrnt_mk_int()] is the constraint for representing
    all integers. *)

val cnstrnt_mk_nonint : unit -> cnstrnt
  (** [cnstrnt_mk_int()] is the constraint for representing
    all non-integer reals. *)

val cnstrnt_mk_nat : unit -> cnstrnt
  (** [cnstrnt_mk_nat()] represents the set of all natural numbers. *)

val cnstrnt_mk_singleton : q -> cnstrnt
  (** [cnstrnt_mk_singleton q] represents the singleton set with member [q]. *)
  
val cnstrnt_mk_diseq : q -> cnstrnt
  (** [cnstrnt_mk_diseq q] represents the set of reals exluding [q]. *)

val cnstrnt_mk_oo : q -> q -> cnstrnt
  (** [cnstrnt_mk_oo p q] represents the subset [{x | p < x < q}] of the reals. *)

val cnstrnt_mk_oc : q -> q -> cnstrnt
 (** [cnstrnt_mk_oc p q] represents the subset [{x | p < x <= q}] of the reals. *)

val cnstrnt_mk_co : q -> q -> cnstrnt
  (** [cnstrnt_mk_co p q] represents the subset [{x | p <= x < q}] of the reals. *)

val cnstrnt_mk_cc : q -> q -> cnstrnt
  (** [cnstrnt_mk_cc p q] represents the subset [{x | p <= x <= q}] of the reals. *)

val cnstrnt_mk_lt : q -> cnstrnt
  (** [cnstrnt_mk_lt q] represents the subset [{x | x < q}] of the reals. *)

val cnstrnt_mk_le : q -> cnstrnt
  (** [cnstrnt_mk_le q] represents the subset [{x | x <= q}] of the reals. *)

val cnstrnt_mk_gt : q -> cnstrnt
  (** [cnstrnt_mk_gt q] represents the subset [{x | x > q}] of the reals. *)

val cnstrnt_mk_ge : q -> cnstrnt
  (** [cnstrnt_mk_ge q] represents the subset [{x | x >= q}] of the reals. *)

val cnstrnt_inter : cnstrnt -> cnstrnt -> cnstrnt
  (** The intersection of two constraints [c], [d] is computed 
    by [cnstrnt_inter c d], that is, a real [q] is in both [c] 
    and [d] iff it is in  [cnstrnt_inter c d]. *)

val cnstrnt_add : cnstrnt -> cnstrnt -> cnstrnt
  (** Abstract interval interpretation of addition. A real number 
    [x] is in [cnstrnt_add c d] iff there are real numbers [y] in 
    [c] and [z] in [d] such that [x = y + x]. *)

val cnstrnt_multq : q -> cnstrnt -> cnstrnt
  (** Abstract interval interpretation of linear arithmetic.
    A real number [x] is in [cnstrnt_multq q c] iff there 
    exists [y] in [c] such that [x = q * y]. *) 

val cnstrnt_mult : cnstrnt -> cnstrnt -> cnstrnt
  (** Abstract interval interpretation of multiplication.
    [cnstrnt_mult] computes an overapproximation of the
    this interpretation, that is, if [x] in [c] and [y] in [d] then there exists
    a [z] in [cnstrnt_mult c d] such that [z = x * y]. *)

val cnstrnt_div : cnstrnt -> cnstrnt -> cnstrnt
  (** If [x] in [c] and [y] in [d] then there exists
    a [z] in [cnstrnt_div c d] such that [z = x/y]. *)


(** {6 Eqeuality theories} *)

(** An {b equality theory} is associated with each function symbol.
 These theories are indexed by naturals between [0] and [maxtheories = 8] according
 to the following table
 - [0] Theory of uninterpreted function symbols.
 - [1] Linear arithmetic theory.
 - [2] Product theory.
 - [3] Bitvector theory.
 - [4] Coproducts.
 - [5] Power products. 
 - [6] Theory of function abstraction and application. 
 - [7] Array theory. 
 - [8] Theory of bitvector interpretation(s). 
*)

val th_to_string : int -> string
  (** [th_to_string th] returns the unique name associated to theory [th]. *)

val th_of_string : string -> int
 (** [th_of_string s] returns theory [th] if [to_string th] is [s]; 
   otherwise the result is unspecified. *)


(** {6 Function Symbols} *)

type sym

val sym_theory_of : sym -> int
  (** [sym_theory_of f] returns the theory [th] associated with
    the function symbol [f]. *)

val sym_eq : sym -> sym -> bool
  (** [sym_eq] tests for equality of two function symbols. *)

val sym_cmp : sym -> sym -> int
  (** [sym_cmpf g] provides a total ordering on function symbols.
    If it returns
    - a negative integer, then [f] is said to be smaller than [g],
    - [0], then [f] is equal to [g] and {!Ics.sym_eq}[(f, g)], and
    - a positive numbe, then [f] is said to be larger than [g]. *)

(** {b Uninterpreted} function symbols *)

val sym_is_uninterp : sym -> bool
  (** [sym_is_uninterp f] holds iff [f] is an uninterpreted
    function symbol. *)

val sym_d_uninterp : sym -> name
  (** [sym_d_uninterp f] returns the name associated with
    an uninterpreted function symbol [f]. This accessor is
    undefined if {!Ics.sym_is_uninterp}[(f)] does not hold. *)


(** {b Linear arithmetic} function symbols are either 
  - {i numerals} for representing all rational numbers, 
  - the {i addition} symbols, 
  - symbols for representing {i linear multiplication} by a 
  rational of type {!Ics.q}. 
*)

val sym_is_num : sym -> bool
  (** [sym_is_num f] holds iff [f] represents a numeral. *)

val sym_d_num : sym -> q
  (** [sym_d_num f] returns the rational [q] if [f] represents [q].
    This accessor is undefined if {!Ics.sym_is_num} does not hold. *)

val sym_is_add : sym -> bool
  (** [sym_is_add f] holds iff [f] represents the addition symbol. *)

val sym_is_multq : sym -> bool
  (** [sym_is_multq f] holds iff [f] represents a linear 
    multiplication symbol. *)

val sym_d_multq : sym -> q
  (** [sym_d_multq f] returns [q] if [f] represents linear
    multiplication by [q]. This accessor is undefined if 
    {!Ics.sym_d_multq} does not hold. *)


(** Symbols of the {b product theory} consist of
  - tupling
  - projections of the [i]-th component in a tuple of length [n]. 
*)

val sym_is_tuple : sym -> bool
  (** [sym_is_tuple f] holds iff [f] represents tupling. *)

val sym_is_proj : sym -> bool
  (** [sym_is_proj f] holds iff [f] represents a projection. *)

val sym_d_proj : sym -> int * int
  (** If {!Ics.sym_is_proj}[(f)] holds, [sym_d_proj f]
    returns the parameters [(i, n)] of [f], denoting the
    [i]-th projection of a tuple of length [n]. *)


(** Symbols of the theory of {b coproducts} are eith
  - left and right injections,
  - left and right coinjections
*)

val sym_is_inl : sym -> bool
  (** [sym_is_inl f] holds iff [f] represents left injection. *)

val sym_is_inr : sym -> bool
  (** [sym_is_inr f] holds iff [f] represents right injection. *)

val sym_is_outl : sym -> bool
  (** [sym_is_outl f] holds iff [f] represents left coinjection. *)

val sym_is_outr : sym -> bool
  (** [sym_is_outr f] holds iff [f] represents right coinjection. *)


(** Symbols in the fixed-sized {b bitvector} theory include
  - constant bitvectors of length [n >= 0],
  - concatenation of a bitvector of width [n >= 0] with a bitvector of width [m >= 0],
  - extraction of bits [i] through [j] of a bitvector of length [n >= 0],
    ([0 <= i <= j < n]), and
  - bitwise conditionals for bitvectors of length [n]. 
*)

val sym_is_bv_const : sym -> bool
  (** [sym_is_bv_const f] holds iff [f] represents a bitvector constant symbol. *)

val sym_is_bv_conc : sym -> bool
  (** [sym_is_bv_conc f] holds iff [f] represents a concatenation symbol. *)

val sym_d_bv_conc : sym -> int * int
  (** [sym_d_bv_conc f] returns [(n, m)] iff [f] 
    represents a concatenation symbol for bitvectors of width [n] with a
    bitvector of width [m]. *)

val sym_is_bv_sub : sym -> bool
  (** [sym_is_bv_sub f] holds iff [f] represents a bitvector extraction symbol. *)

val sym_d_bv_sub : sym -> int * int * int
  (** [sym_d_bv_sub f] returns [(i, j, n)] iff [f] represents a bitvector
    extraction of bits [i] through [j] of a bitvector of width [n]. *)

val sym_is_bv_bitwise : sym -> bool
  (** [sym_is_bv_bitwise f] holds iff [f] represents a logical bitwise operation
    symbol. *)

val sym_d_bv_bitwise : sym -> int
 (** [sym_d_bitwise f] returns [n] iff [f] represents a logical bitwise
   operation of width [n]. *)


(** Symbols from the theory of {b power products}. *)

val sym_is_mult : sym -> bool

val sym_is_expt : sym -> bool


(** Symbols from the theory of {b function abstraction and application}. *)

val sym_is_apply : sym -> bool

val sym_d_apply : sym -> cnstrnt option

val sym_is_abs : sym -> bool


(** Symbols from the theory of {b arrays}. *)

val sym_is_select : sym -> bool

val sym_is_update : sym -> bool


(** Symbols from the theory of {b arithmetic interpretations of bitvectors}. *)

val sym_is_unsigned : sym -> bool


(** {6 Terms} *)
   
(** Terms are either 
  - variables or
  - applications of function symbols of type {!Ics.sym} to a list of terms.
*)

type term

val term_of_string : string -> term
  (** [term_of_string] parses a string according to the grammar for the nonterminal
    [termeof] in module [Parser] (see its specification in file [parser.mly]) and
    builds a corresponding term. *)

val term_input : inchannel -> term
  (** Similary, [term_input] builds a term by reading from an input channel. *)

val term_to_string : term -> string

val term_output : outchannel -> term -> unit
  (** [term_output out a] prints term [a] on the output channel [out], 
    and [term_pp a] is equivalent to [term_output stdout a]. *)

val term_pp : term -> unit

val term_eq : term -> term -> bool
  (** [term_eq a b] holds iff [a] and [b] are syntactically
    equal, that is, either
    - both [a] and [b] are variables of the same kind and their
      associated names are equal
    - both [a] and [b] are application terms with equal
      function symbols (see {!Ics.sym_eq}), the number of
      arguments in [a] and [b] is equal, and the respective
      arguments at every position are term equal. *)

val term_cmp : term -> term -> int
  (** Comparison [term_cmp a b] returns either
    [-1], [0], or [1] depending on whether [a] is less than [b],
    the arguments are equal, or [a] is greater than [b]. *)

val term_mk_var : string -> term
  (** Given a string [s], [term_mk_var s] constructs a 
    variable with name [s] and [term_mk_uninterp s al]
    constructs an application of an uninterpreted function
    symbol [s] to a list of argument terms. *)

val term_mk_uninterp : string -> term list -> term


(** {b Arithmetic terms} include rational constants built from
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


val term_is_arith : term -> bool


(** Tuples are built using the [mk_tuple] constructor, and
  projection of the [i]-th component of a tuple [t] of
  length [n] is realized using [mk_proj i n t]. *)
    
val term_mk_tuple : term list -> term
val term_mk_proj : int -> int -> term -> term

(** Boolean constants. *)
    
val term_mk_true  : unit -> term
val term_mk_false : unit -> term

val term_is_true : term -> bool
val term_is_false : term -> bool
 

(** {b Bitvector terms} *)

val term_mk_bvconst : string -> term
val term_mk_bvsub : (int * int * int) -> term -> term
val term_mk_bvconc : int * int -> term -> term -> term
val term_mk_bwite : int -> term * term * term -> term
val term_mk_bwand : int -> term -> term -> term
val term_mk_bwor : int -> term -> term -> term
val term_mk_bwnot : int -> term -> term

(** {b Coproducts} *)

val term_mk_inj : int -> term -> term
val term_mk_out : int -> term -> term



(** Builtin simplifying constructors. *)

val term_mk_unsigned : term -> term

val term_mk_update : term -> term -> term -> term
val term_mk_select :  term -> term -> term

val term_mk_div :  term -> term -> term

val term_mk_mult : term -> term -> term
val term_mk_multl : term list -> term
val term_mk_expt : int -> term -> term   
  (* [term_mk_expt n x] represent [x^n]. *)

val term_mk_apply : term -> term list -> term

val term_mk_arith_apply : cnstrnt -> term -> term list -> term


(** Set of terms. *)

type terms


(** {6 Atoms} *)

(** Atoms. [atom_mk_true()] is the trivially true atom,
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


(** Derived atomic constraints. [atom_mk_int t] restricts the domain of 
 interpretations of term [t] to the integers. Similarly, [atom_mk_real] 
 restricts its argument to the real numbers. [atom_mk_lt a b] generates
 the constraint ['a' < 'b'],  [atom_mk_le a b] yields ['a' <= 'b'], 
 [atom_mk_gt a b] yields ['a' > 'b'], and [atom_mk_ge a b] yields ['a' >= 'b'].
 *)

val atom_mk_real : term -> atom
val atom_mk_int : term -> atom
val atom_mk_nonint : term -> atom
    
val atom_mk_lt : term -> term -> atom
val atom_mk_le : term -> term -> atom
val atom_mk_gt : term -> term -> atom
val atom_mk_ge : term -> term -> atom


(** {6 Solutions sets} *)

(** Solution sets. *)

type solution

val solution_apply : solution -> term -> term

val solution_find : solution -> term -> term

val solution_inv : solution -> term -> term

val solution_mem : solution -> term -> bool

val solution_occurs : solution -> term -> bool

val solution_use : solution -> term -> terms

val solution_is_empty : solution -> bool



(** {6 Logical Context} *)
 

type context
  (** A logical context represents a conjunction of atoms. *)

val context_eq : context -> context -> bool
  (** [context_eq s1 s2] is a constant-time predicate for 
    testing for identity of two states. Thus, whenever this 
    predicate holds, its corresponding contexts are logically
    equivalent. *)
 
val context_empty : unit -> context
  (** [context_empty()] represents the empty logical context. *)

val context_ctxt_of : context -> atom list
  (** [context_ctxt_of c] returns a set of atoms whose
    conjunction is represented by [c].  This set is not
    necessarily minimal. *)

val context_u_of : context -> solution

val context_a_of : context -> solution

val context_t_of : context -> solution

val context_bv_of : context -> solution

val context_pp : context -> unit

val context_ctxt_pp : context -> unit

type status
    
val is_consistent   : status -> bool
val is_redundant    : status -> bool
val is_inconsistent : status -> bool

val d_consistent : status -> context
    
val process : context -> atom -> status
  (** The operation [process s a] adds a new atom [a] to a logical context [s].
    The codomain of this function is of type [status], elements of
    which represent the three possible outcomes of processing an atom
    - the atom [a] could be demonstrated to be inconsistent in [s]. 
      In this case, {!Ics.is_inconsistent} holds of the result.
    - the atom [a] could be demonstrated to be derivable in the context [s].
      In this case, {!Ics.is_redundant} holds.
    - Neither of the above holds. In this case, a modified context
      for representing the context of [s] conjoined with [a]
      is obtained using the destructor {!Ics.d_consistent}. 

    Notice that a result [res] with {!Ics.is_consistent}[(res)] does not necessarily
    imply that atom [a] is indeed satisfiable, since the theory of ICS is indeed
    undecidable.  Moreover, ICS includes a number of nonconvex theories, which
    requires case-splitting for completeness.  [process] does not perform these
    case-splits in order to keep worst-case runtimes polynomial (with the notable
    exception of canonization of logical bitwise operators).  Instead, it is in 
    the responsibility of the application programmer to perform these splits;
    see also {!Ics.split}. *)

val split : context -> atom list
  (** Suggesting case splits. *)


val can : context -> atom -> atom
  (** Given a logical context [s] and an atom [a],
    [can s a] computes a semicanonical form of [a] in [s], that is,
    - if [a] holds in [s] it returns [Atom.True], 
    - if the negation of [a] holds in [s] then it returns [Atom.False], and, otherwise,
    - an equivalent normalized atom built up only from variables is returned. *)


val cnstrnt : context -> term -> cnstrnt option
  (** Given a logical context [s] and a term [a], [cnstrnt s a]
    computes an arithmetic constraint for [a] in [s] using constraint 
    information in [s] and abstraction interval interpretation.
    If no such constraint can be deduced, [None] is returned. *)


(** {6 Commands} *)

(** An imperative state [istate] does not only include a logical 
 context of type [state] but also a symbol table and input and output 
 channels. A global [istate] variable is manipulated and
 destructively updated by commands. *)

val init : int * bool * string * inchannel * outchannel -> unit
  (** Initialization. [init n] sets the verbose level to [n]. The higher
    the verbose level, the more trace information is printed to [stderr]
    (see below). There are no trace messages for [n = 0]. In addition, 
    initialization makes the system to raise the [Sys.Break] exception upon
    user interrupt [^C^C].  The [init] function should be called before
    using any other function in this API. *)

val cmd_rep : unit -> unit
  (** [cmd_rep] reads a command from the current input channel according
    to the grammar for the nonterminal [commandeof] in module [Parser] (see
    its specification in file [parser.mly], the current internal [istate] 
    accordingly, and outputs the result to the current output channel. *)

val cmd_batch : unit -> unit


val flush : unit -> unit



(** {6 Controls}. *)

val reset : unit -> unit
  (** [reset()] clears all the global tables. This does not only 
    include the current context but also internal tables used  for hash-consing 
    and memoization purposes. *)
    
val gc : unit -> unit
  (** [gc()] triggers a full major collection of ocaml's garbage collector. *)

val do_at_exit : unit -> unit
  (** [do_at_exit] clears out internal data structures. *)

val sleep : int -> unit
  (** Sleeping for a number of seconds. *)


(** {6 Tracing} *)

(**
  Rudimentary control on trace messages, which are 
 sent to [stderr]. These functions are mainly included
 for debugging purposes, and are usually not being used
 by the application programmer. *)

val trace_reset : unit -> unit
  (** [trace_reset()] disables all tracing. *)

val trace_add : string -> unit
  (** [trace_add str] enables tracing of functions associated with trace level [str]. *)

val trace_remove : string -> unit
  (** [trace_remove str] removes [str] from the set of active trace levels *)

val trace_get : unit -> string list
    (** [trace_get()] returns the set of active trace levels. *) 


(** {6 Lists} *)

val is_nil : 'a list -> bool
val cons : 'a -> 'a list -> 'a list
val head : 'a list -> 'a
val tail : 'a list -> 'a list


(** {6 Pairs} 

  Pairs. [pair a b] builds a pair [(a,b)] and
 [fst (pair a b)] returns [a] and [snd (pair b a)]
 returns [b]. *)

val pair : 'a -> 'b -> 'a * 'b
val fst : 'a * 'b -> 'a
val snd : 'a * 'b -> 'b

(** {6 Triples}  *)

val triple : 'a -> 'b -> 'c -> 'a * 'b * 'c
val fst_of_triple : 'a * 'b *'c -> 'a
val snd_of_triple : 'a * 'b *'c -> 'b
val third_of_triple : 'a * 'b *'c -> 'c


(** Quadruples. Accessors for quadruples [(a,b,c,d)]. *)
  
val fst_of_quadruple : 'a * 'b * 'c *'d -> 'a
val snd_of_quadruple : 'a * 'b * 'c *'d -> 'b
val third_of_quadruple : 'a * 'b * 'c *'d -> 'c
val fourth_of_quadruple : 'a * 'b * 'c *'d -> 'd

(** {6 Options} *)
      
(** Options. An element of type ['a option] either satisfies
 the recognizer [is_some] or [is_none].  In case, [is_some]
 holds, a value of type ['a] can be obtained by [value_of].  *)

val is_some : 'a option -> bool
val is_none : 'a option -> bool

val value_of : 'a option -> 'a

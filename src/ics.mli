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

  There are two sets of interface functions.  The {b functional interface}
  provides functions for building up the main syntactic categories of
  ICS such as terms and atoms, and for extending logical contexts 
  using {!Ics.process},  which is side-effect free.

  In contrast to this functional interface, the {b command interface}
  manipulates a global state consisting, among others, of symbol tables 
  and the current logical context.  The {!Ics.cmd_rep} procedure, which 
  reads commands from the current input channel and manipulates the global
  structures accordingly, is used to implement the ICS interactor.

  Besides functions for manipulating ICS datatypes, this interface also
  contains a number of standard datatypes such as channels, multiprecision 
  arithmetic, tuples, and lists.
*)

(** {6 Configuration} *)

val set_profile : bool -> unit
val set_pretty : bool -> unit
val set_compactify : bool -> unit
val set_verbose : bool -> unit
val set_remove_subsumed_clauses : bool -> unit
val set_validate_counter_example : bool -> unit
val set_polarity_optimization: bool -> unit
val set_clause_relevance : int -> unit
val set_cleanup_period : int -> unit
val set_num_refinements : int -> unit
val set_statistic : bool -> unit
val set_footprint : bool -> unit
val set_justifications : bool -> unit
val set_integer_solve : bool -> unit
 

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
  - a domain restriction, and 
  - a sign restriction.
  A real number satisfies such a constraint if it satisfies both
  - the domain restriction and
  - the sign restriction.
  To each constraint [s] we associate the set [D(s)] of reals satisfying
  these requirements. *)


type dom


(** {6 Equality theories} *)

(** An {b equality theory} is associated with each function symbol of terms.
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

type th

val th_to_string : th -> string
  (** [th_to_string th] returns the unique name associated to theory [th]. *)

val th_of_string : string -> th
 (** [th_of_string s] returns theory [th] if [to_string th] is [s]; 
   otherwise the result is unspecified. *)


(** {6 Function Symbols} *)

type sym

val sym_theory_of : sym -> th
  (** [sym_theory_of f] returns the theory [th] associated with
    the function symbol [f]. *)

val sym_eq : sym -> sym -> bool
  (** [sym_eq] tests for equality of two function symbols. *)

val sym_cmp : sym -> sym -> int
  (** [sym_cmp f g] provides a total ordering on function symbols.
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
  rational of type {!Ics.q}. *)

val sym_mk_num : q -> sym
  (** [sym_mk_num q] constructs a numeral symbol for representing [q]. *)

val sym_is_num : sym -> bool
  (** [sym_is_num f] holds iff [f] represents a numeral. *)

val sym_d_num : sym -> q
  (** [sym_d_num f] returns the rational [q] if [f] represents [q].
    This accessor is undefined if {!Ics.sym_is_num} does not hold. *)

val sym_mk_add : unit -> sym
  (** [sym_mk_add()] constructs the addition symbol. *)

val sym_is_add : sym -> bool
  (** [sym_is_add f] holds iff [f] represents the addition symbol. *)

val sym_mk_multq : q -> sym
  (** [sym_mk_multq q] constructs the symbol for linear multiplication
    by a rational [q]. *)

val sym_is_multq : sym -> bool
  (** [sym_is_multq f] holds iff [f] represents a linear 
    multiplication symbol. *)

val sym_d_multq : sym -> q
  (** [sym_d_multq f] returns [q] if [f] represents linear
    multiplication by [q]. This accessor is undefined if 
    {!Ics.sym_d_multq} does not hold. *)


(** Symbols of the {b product theory} consist of
  - tupling
  - projections of the [i]-th component in a tuple of length [n]. *)

val sym_mk_cons : unit -> sym
  (** [sym_mk_cons()] constructs the symbol for tupling. *)

val sym_is_cons : sym -> bool
  (** [sym_is_cons f] holds iff [f] represents tupling. *)

val sym_is_car : sym -> bool
  (** [sym_is_car f] holds iff [f] represents a projection. *)

val sym_mk_car : unit -> sym
  (** [sym_mk_car()] constructs the symbol for the first projection *)

val sym_is_cdr : sym -> bool
  (** [sym_is_cdr f] holds iff [f] represents a projection. *)

val sym_mk_cdr : unit -> sym
  (** [sym_mk_cdr()] constructs the symbol for the first projection *)



(** Symbols of the theory of {b coproducts} are eith
  - left and right injections,
  - left and right coinjections *)

val sym_mk_inl : unit -> sym
  (** [sym_mk_inl ()] constructs symbol for left injection. *)

val sym_is_inl : sym -> bool
  (** [sym_is_inl f] holds iff [f] represents left injection. *)

val sym_mk_inr : unit -> sym
  (** [sym_mk_inr ()] constructs symbol for right injection. *)

val sym_is_inr : sym -> bool
  (** [sym_is_inr f] holds iff [f] represents right injection. *)

val sym_mk_outl : unit -> sym
  (** [sym_mk_outl ()] constructs symbol for left injection. *)

val sym_is_outl : sym -> bool
  (** [sym_is_outl f] holds iff [f] represents left coinjection. *)

val sym_mk_outr : unit -> sym
  (** [sym_mk_outr ()] constructs symbol for right coinjection. *)

val sym_is_outr : sym -> bool
  (** [sym_is_outr f] holds iff [f] represents right coinjection. *)


(** Symbols in the fixed-sized {b bitvector} theory include
  - constant bitvectors of length [n >= 0],
  - concatenation of a bitvector of width [n >= 0] with a bitvector of width [m >= 0],
  - extraction of bits [i] through [j] of a bitvector of length [n >= 0],
    ([0 <= i <= j < n]), and
  - bitwise conditionals for bitvectors of length [n]. *)

val sym_mk_bv_const : string -> sym
  (** [sym_mk_bv_const str] constructs, say, a bitvector constant [01001]
    from a string of the form ["01001"]. The result is undefined if characters
    other than ['0'] or ['1'] appear in the string. *)

val sym_is_bv_const : sym -> bool
  (** [sym_is_bv_const f] holds iff [f] represents a bitvector constant symbol. *)

val sym_mk_bv_conc : int -> int -> sym
  (** [sym_mk_bv_conc n m] constructs a concatenation symbol with indices [n]
    and [m], for [n, m >= 0], for concatenating a bitvector of width [n] with a 
    bitvector of length [m]. *)

val sym_is_bv_conc : sym -> bool
  (** [sym_is_bv_conc f] holds iff [f] represents a concatenation symbol. *)

val sym_d_bv_conc : sym -> int * int
  (** [sym_d_bv_conc f] returns [(n, m)] iff [f] 
    represents a concatenation symbol for bitvectors of width [n] with a
    bitvector of width [m]. *)

val sym_mk_bv_sub : int -> int -> int -> sym
  (** [sym_mk_bv_sub i j n] constructs a bitvector extraction symbol for the
    indices [0 <= i <= j < n]. *)

val sym_is_bv_sub : sym -> bool
  (** [sym_is_bv_sub f] holds iff [f] represents a bitvector extraction symbol. *)

val sym_d_bv_sub : sym -> int * int * int
  (** [sym_d_bv_sub f] returns [(i, j, n)] iff [f] represents a bitvector
    extraction of bits [i] through [j] of a bitvector of width [n]. *)


(** Symbols from the theory of {b power products} include
  - Multi-ary nonlinear multiplication symbol
  - Exponentiation with an integer.
*)

val sym_mk_mult : unit -> sym
  (** [sym_mk_mult()] constructs the nonlinear multiplication symbol. *)

val sym_is_mult : sym -> bool
  (** [sym_is_mult f] holds iff [f] represents the nonlinear multiplication symbol. *)

val sym_mk_expt : int -> sym
  (** [sym_mk_expt n] constructs the 'exponentiation by [n]' symbol. *)

val sym_is_expt : sym -> bool
  (** [sym_is_expt f] holds iff [f] represents an exponentiation symbol. *)

val sym_d_expt : sym -> int
  (** [sym_d_expt f] returns [n] if [f] represents the 'exponentiation by [n]' symbol. *)


(** Symbols from the theory of {b function abstraction and application} include
  - function abstraction
  - function application

  A function application symbol may have a constraint of type {!Ics.cnstrnt} associated
  with it. *)

val sym_mk_apply : dom option -> sym
 (** [sym_mk_apply co] constructs a symbol for function application with associated
   constraint [co]. *)

val sym_is_apply : sym -> bool
  (** [sym_is_apply f] holds iff [f] represents the function application symbol. *)

val sym_d_apply : sym -> dom option
 (** [sym_d_apply f] returns the constraint associated with a function application
   symbol. *)

val sym_mk_abs : unit -> sym
  (** [sym_mk_abs()] constructs the symbol for function abstraction. *)

val sym_is_abs : sym -> bool
  (** [sym_is_abs f] holds iff [f] represents the function abstraction symbol. *)


(** Symbols from the theory of {b arrays} include
  - array updates (write)
  - array selection (read) *)

val sym_mk_select : unit -> sym
  (** The array select symbol. *)

val sym_is_select : sym -> bool
  (** [sym_is_select f] holds iff [f] represents the array selection symbol. *)

val sym_mk_update : unit -> sym
  (** The array update symbol. *)

val sym_is_update : sym -> bool
 (** [sym_is_update f] holds iff [f] represents the array update symbol. *)



(** {6 Variables} *)

(** The set of all variables is partitioned into different {b kinds} of variables, namely
  - {i external},
  - {i fresh}, and 
  - {b bound} variables.  
  
  There is a name of type {!Name.t} associated with each variable. Names for 
  fresh variables are always of the form ["x!i"], where [x] is an arbitrary string 
  and [i] is an integer string. The name associated with a bound variable is of the 
  form ["!i"] for an integer [i].
*)

type var

(*
val var_name_of : var -> name
  (** [name_of x] returns the name associated with a variable [x]. *)

val var_eq : var -> var -> bool
  (** [eq x y] holds iff [x] and [y] are in the same category (that is,
    external, fresh, and bound) of variables and if their names are identical. *)

val var_cmp : var -> var -> int
  (** [cmp x y] realizes a total ordering on variables. The result is [0]
    if [eq x y] holds, it is less than [0] we say, '[x] is less than [y]',
    and, otherwise, '[x] is greater than [y]'. An external variable [x] is always
    less than a nonexternal (that is, a fresh or a free) variable [y]. Otherwise, 
    the outcome of [cmp x y] is unspecified. *)

val var_mk_external : name -> term
  (** [var_mk_external x] creates an external variable with associated name [x]. *)

val var_mk_bound : int -> term
  (** [var_mk_bound i] constructs a bound variable with associated name [!i]. *)

val var_is_external : var -> bool
  (** [is_var x] holds iff [x] is an external variable. *)

val var_is_bound : var -> bool
  (** [is_bound x] holds iff [x] is a bound variable. *)
*)


(** {6 Terms} *)
   
(** Terms are either 
  - variables or
  - applications of function symbols of type {!Ics.sym} to a list of terms. *)

type term

val term_of_string : string -> term
  (** [term_of_string] parses a string according to the grammar 
    for the nonterminal {!Parser.termeof} (see its specification in 
    file [parser.mly]) and builds a corresponding term. *)

val term_input : inchannel -> term
  (** [term_input inch] is similar to {!Ics.term_of_string} but builds a 
    term by reading from input channel [inch]. *)

val term_output : outchannel -> term -> unit
  (** [term_output outch a] prints term [a] on the output channel [out]. *)

val term_to_string : term -> string
  (** [term_to_string a] prints a term to a string. This string
    is parsable by {!Ics.term_of_string}. *)

val term_pp : term -> unit
  (** [term_pp a] is equivalent to [term_output (Ics.stdout()) a]. *)
  
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
    the arguments are equal, or [a] is greater than [b]. 
    - Variables are always greater than applications, 
    - variables are ordered according to {!Ics.var_cmp}, and 
    - applications are ordered lexicographically using {!Ics.sym_cmp} 
      on the function symbols and comparing respective term arguments. *)

val term_mk_var : string -> term
  (** Given a string [s], [term_mk_var s] constructs an 
    {i external} variable with name [s]. *)

val term_mk_uninterp : string -> term list -> term
  (** [term_mk_uninterp s al] constructs an application of an 
    uninterpreted function symbol [s] to a list [al] of argument 
    terms. *)


(** {b Linear arithmetic terms} are built-up from rational constants,
  linear multiplication of a rational with a variable, and n-ary
  addition. 

  Linear arithmetic terms are always normalized as a {b sum-of-product}
  [q0 + q1*x1+...+qn*xn] where the [qi] are rational constants and the
  [xi] are variables (or any other term not interpreted in this
  theory), which are ordered such that {!Ics.term_cmp}[xi xj] is
  greater than zero for [i < j]. This implies that any such variable
  occurs at most once. In addition, [qi], for [i > 0], is never zero.
  If [qi] is one, we just write [xi] instead of [qi * xi], and if [q0]
  is zero, it is simply omitted in the sum-of-product above.

include rational constants built from
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


val term_is_arith : term -> bool
  (** [term_is_arith a] holds if the toplevel symbol of [a]
    is interpreted in linear arithmetic. *)

val term_mk_num : q -> term
  (** [term_mk_num q] constructs a numeral term for representing
    the rational [q]. *)

val term_mk_multq : q -> term -> term
  (** [term_mk_multq q a] constructs a term for representing
    the term [a] multiplied by [q]. If [a] is in sum-of-product
    form, then so is [term_mk_multq q a]. *)
  
val term_mk_add : term -> term -> term 
  (** [term_mk_add a b] constructs a term for representing the
    sum of [a] and [b]. If both [a] and [b] are in sum-of-product
    form, then so is  [term_mk_add a b]. *)

val term_mk_addl : term list -> term  
   (** Iteration of binary addition
     - [term_mk_addl []] is {!Ics.term_mk_num()},
     - [term_mk_addl [a]] is [a], and
     - [term_mk_addl (a :: al)] is [term_mk_add a (term_mk_addl al)]. *)

val term_mk_sub : term -> term -> term
  (** [term_mk_sub a b] represents the difference [a - b]. If
    both [a] and [b] are in sum-of-product form, then so is the result. *)

val term_mk_unary_minus : term -> term
  (** [term_mk_unary_minus a] represents the negation of [a].
    If [a] is in sum-of-product form, then so is the result. *)


(** Tuple terms. Tuple terms in normal form do not contain 
  (applicable) projections on tuples. *)
   
val term_mk_tuple : term list -> term
  (** [term_mk_tuple [a1;...;an]] constructs tuple
    term for respresenting the tuple [(a1,...,an)]. The
    result is in tuple normal form, when all [ai] are in tuple normal
    form  *)

val term_mk_proj : int -> term -> term
  (** [term_mk_proj i a] constructs, for [0 <= i < n], a term 
    for representing the [i]-th projection of an [n]-tuple. If [a]
    is in tuple normal form, then so is the result. *)


(** {b Bitvector terms} are built up from bitvector constants, concatenation
  of two bitvectors, extraction of a contiguous subrange from a bitvector,
  and logical bitwise operations.  Each bitvector term has a nonnegative
  {i width} associated with it, and bits in a bitvector of width [n] are
  addressed from [0] to [n-1] in increasing order from left-to-right. All bitvector 
  terms are in {i concatenation normal form}, that is, a left-associative 
  concatenation of 
  - terms uninterpreted in the bitvector theory
  - bitvector constants (with adjacent constants merged)
  - single extractions from uninterpreted terms in this theory
  - bitvector BDDs, which are BDDs with nodes consisting of one
    of the above classes of terms. 

  The constructors below all construct concatenation normal forms,
  whenever their arguments are in this form.
*)

val term_mk_bvconst : string -> term
  (** [term_mk_bvconst str] constructs a bitvector constant. *)

val term_mk_bvsub : (int * int * int) -> term -> term
  (** [term_mk_bvsub i j n a] constructs, for [0 <= i <= j < n] a term 
    for representing the extraction of the [j-i+1] bits from 
    position [i] through [j] in a term of width [n]. *)

val term_mk_bvconc : int * int -> term -> term -> term
  (** [term_mk_bvconc n m a b] constructs the concatenation [a ++ b]
    of bitvector terms [a] of width [n] with [b] of width [m]. *)



(** {b Boolean Term Constants.} *)
    
val term_mk_true  : unit -> term
  (** The propositional constant [term_mk_true()] is encoded
    as the bitvector constant of width [1] with a [1] at position [0]. *)

val term_mk_false : unit -> term
  (** The propositional constant [term_mk_false()] is encoded
    as the bitvector constant of width [1] with a [0] at position [0]. *)

val term_is_true : term -> bool
  (** [term_is_true a] holds iff [a] is term equal to [term_mk_true()]. *)

val term_is_false : term -> bool
  (** [term_is_false a] holds iff [a] is term equal to [term_mk_false()]. *)


(** {b Coproducts} *)

val term_mk_inj : int -> term -> term
  (** [term_mk_inj n a] constructs a term for [n]-ary injection. *)

val term_mk_out : int -> term -> term
  (** [term_mk_out n a] constructs a term for [n]-ary outjection. *)


(** Builtin simplifying constructors. *)

val term_mk_update : term -> term -> term -> term
val term_mk_select :  term -> term -> term

val term_mk_div :  term -> term -> term

val term_mk_mult : term -> term -> term
val term_mk_multl : term list -> term
val term_mk_expt : int -> term -> term   
  (* [term_mk_expt n x] represent [x^n]. *)

val term_mk_apply : term -> term -> term

(** Set of terms. *)

type terms

val terms_of_list : term list -> terms

val terms_to_list : terms -> term list


(** {6 Atoms} *)

(** Atoms. [atom_mk_true()] is the trivially true atom,
 [atom_mk_false()] is the trivially false atom, and 
 given terms [a], [b], the constructor [mk_equal a b]
 constructs an equality constraint, [mk_diseq a b] a 
 disequality constraint, and [atom_mk_in a c] constructs
 a membership constraint for [a] and a arithmetic constraint [c]. 
 Atoms are printed to [stdout] using [atom_pp]. *)

type atom
  (** An {b atom} is either
    - the trivially true atom [atom_mk_true],
    - the unsatisfiable [atom_mk_false],
    - an equality atom [atom_mk_equal a b],
    - a disequality atom [atom_mk_diseq a b], or
    - a constraint atom [atom_mk_in a c], which
      constrains [a] to be interpreted over the domain [D(c)] associated
      with the constraint [c] of type {!Ics.cnstrnt}. *)

val atom_pp : atom -> unit

val atom_of_string : string -> atom
val atom_to_string : atom -> string
     
val atom_mk_true : unit -> atom
val atom_mk_false : unit -> atom
val atom_mk_equal  : term -> term -> atom
val atom_mk_diseq  : term -> term -> atom

val atom_mk_le : term -> term -> atom
val atom_mk_lt : term -> term -> atom
val atom_mk_ge : term -> term -> atom
val atom_mk_gt : term -> term -> atom

val atom_is_negatable : atom -> bool
val atom_negate : atom -> atom

type atoms

val atoms_empty : unit -> atoms
val atoms_singleton : atom -> atoms
val atoms_add : atom -> atoms -> atoms
val atoms_to_list : atoms -> atom list



(** {6 Justifications} *)

type justification

val justification_pp : justification -> unit


(** {6 Logical Context} *)
 

type context
  (** A logical context represents a conjunction of atoms. *)

val context_pp : context -> unit
  (** Pretty-printing a context to standard output. *)

val context_ctxt_pp : context -> unit
  (** Pretty-printing the logical context in a way that can be read in again by the parser. *)

val context_eq : context -> context -> bool
  (** [context_eq s1 s2] is a constant-time predicate for 
    testing for identity of two states. Thus, whenever this 
    predicate holds, its corresponding contexts are logically
    equivalent. *)

val context_ctxt_of : context -> atoms
  (** [context_ctxt_of s] returns the logical context of [s] as a set of atoms. *)
  
val context_mem : th -> context -> Term.t -> bool
  (** [context_mem th s x] iff [x = _] is in the solution set for theory [th] in [s]. *)
  
val context_apply : th -> context -> Term.t -> Term.t * justification
  (** [apply th s x] is [a] when [x = a] is in the solution set for theory [th]
    in [s]; otherwise [Not_found] is raised. *)
  
val context_find : th -> context -> Term.t -> Term.t * justification
  (** [find th s x] is [a] if [x = a] is in the solution set for theory [th]
    in [s]; otherwise, the result is just [x]. *)
  
val context_inv : th -> context -> Term.t -> Term.t
  (** [inv th s a] is [x] if there is [x = a] in the solution set for
    theory [th]; otherwise [Not_found] is raised. *)
  
val context_use : th -> context -> Term.t -> Term.Set.t
  (** [use th s x] consists of the set of all term variables [y] such
    that [y = a] in [s], and [x] is a variable [a]. *)

val context_empty : unit -> context
  (** [context_empty()] represents the empty logical context. *)

type status
  (** Inhabitants of type status are used as return values for {!Ics.process}. *)
    
val is_consistent   : status -> bool
val is_redundant    : status -> bool
val is_inconsistent : status -> bool

val d_consistent : status -> context
  (** In case [is_consistent st] holds, [d_consistent st] returns a new context. *)
    
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
  (** Suggested case splits. *)

val can : context -> term -> term * justification
  (** Given a logical context [s] and an atom [a],
    [can s a] computes a semicanonical form of [a] in [s], that is,
    - if [a] holds in [s] it returns [Atom.True], 
    - if the negation of [a] holds in [s] then it returns [Atom.False], and, otherwise,
    - an equivalent normalized atom built up only from variables is returned. *)


val dom : context -> term -> dom * justification
  (** Given a logical context [s] and a term [a], [cnstrnt s a]
    computes an arithmetic constraint for [a] in [s] using constraint 
    information in [s] and abstraction interval interpretation.
    If no such constraint can be deduced, [None] is returned. *)



(** {6 Propositions} *)

type prop

val prop_pp : prop -> unit
 
val prop_mk_true : unit -> prop
val prop_mk_false : unit -> prop
val prop_mk_var : name -> prop
val prop_mk_poslit : Atom.t -> prop
val prop_mk_neglit : Atom.t -> prop
val prop_mk_ite : prop -> prop -> prop -> prop
val prop_mk_conj : prop list -> prop
val prop_mk_disj : prop list -> prop
val prop_mk_iff : prop -> prop ->prop
val prop_mk_neg : prop -> prop


val prop_is_true : prop -> bool
val prop_is_false : prop -> bool
val prop_is_var : prop -> bool
val prop_is_atom : prop -> bool
val prop_is_ite : prop -> bool
val prop_is_disj : prop -> bool
val prop_is_iff : prop -> bool
val prop_is_neg : prop -> bool
val prop_is_let : prop -> bool

val prop_d_var : prop -> name
val prop_d_atom : prop -> atom
val prop_d_ite : prop -> prop * prop * prop
val prop_d_disj : prop -> prop list
val prop_d_iff : prop -> prop * prop
val prop_d_neg : prop -> prop
val prop_d_let : prop -> name * prop * prop


(** {6 Propositional Satisfiability} *)

type assignment

val assignment_pp : assignment -> unit

val assignment_valuation : assignment -> (name * bool) list
val assignment_literals : assignment -> atom list


val prop_sat : context -> prop -> assignment option


(** {6 Commands} *)

(** An imperative state [istate] does not only include a logical 
 context of type [state] but also a symbol table and input and 
 output channels. A global [istate] variable is manipulated and
 destructively updated by commands. *)

val init : int -> unit
  (** Initialization. [init n] sets the verbose level to [n]. The higher
    the verbose level, the more trace information is printed to [stderr]
    (see below). There are no trace messages for [n = 0]. In addition, 
    initialization makes the system to raise the [Sys.Break] exception upon
    user interrupt [^C^C].  The [init] function should be called before
    using any other function in this API. *)

val set_outchannel : outchannel -> unit
val set_inchannel : inchannel -> unit
val set_prompt : string -> unit
val set_eot : string -> unit

val cmd_rep : unit -> unit
  (** [cmd_rep] reads a command from the current input channel according
    to the grammar for the nonterminal [commandeof] in module [Parser] (see
    its specification in file [parser.mly], the current internal [istate] 
    accordingly, and outputs the result to the current output channel. *)

val cmd_batch : inchannel -> unit
  (** Similar to {!Ics.cmd_rep}, but syntax error messages contain line numbers,
    and processing is aborted after state is unsatisfiable. *)

val flush : unit -> unit


(** {6 Controls}. *)

val reset : unit -> unit
  (** [reset()] clears all the global tables. This does not only 
    include the current context but also internal tables used for 
    hash-consing and memoization purposes. *)
    
val gc : unit -> unit
  (** [gc()] triggers a full major collection of ocaml's garbage collector. *)

val do_at_exit : unit -> unit
  (** [do_at_exit] clears out internal data structures. *)

val sleep : int -> unit
  (** Sleeping for a number of seconds. *)


(** {6 Tracing} *)

(** Rudimentary control on trace messages, which are 
 sent to [stderr]. These functions are mainly included
 for debugging purposes, and are usually not being used
 by the application programmer. *)

val trace_reset : unit -> unit
  (** [trace_reset()] disables all tracing. *)

val trace_add : string -> unit
  (** [trace_add str] enables tracing of functions associated with trace level [str].
    For example, [trace_add "rule"] traces the calls for processing all generated equalities,
    disequalities, and constraints. *)

val trace_remove : string -> unit
  (** [trace_remove str] removes [str] from the set of active trace levels *)

val trace_get : unit -> string list
    (** [trace_get()] returns the set of active trace levels. *)


(** {6 Lists} *)

val is_nil : 'a list -> bool
val cons : 'a -> 'a list -> 'a list
val head : 'a list -> 'a
val tail : 'a list -> 'a list


(** {6 Pairs} *)

val pair : 'a -> 'b -> 'a * 'b
  (** [pair a b] builds a pair [(a,b)]. *)
  
val fst : 'a * 'b -> 'a
  (** [fst p] returns [b] if [p] is equal to some [pair a _]. *)
  
val snd : 'a * 'b -> 'b
  (** [snd p] returns [b] if [p] is equal to some [pair _ b]. *)


(** {6 Triples}  *)

val triple : 'a -> 'b -> 'c -> 'a * 'b * 'c
val fst_of_triple : 'a * 'b *'c -> 'a
val snd_of_triple : 'a * 'b *'c -> 'b
val third_of_triple : 'a * 'b *'c -> 'c


(** {6 Quadruples} *)
  
val fst_of_quadruple : 'a * 'b * 'c *'d -> 'a
val snd_of_quadruple : 'a * 'b * 'c *'d -> 'b
val third_of_quadruple : 'a * 'b * 'c *'d -> 'c
val fourth_of_quadruple : 'a * 'b * 'c *'d -> 'd


(** {6 Option types} *)
      
(** Options. An element of type ['a option] either satisfies
 the recognizer [is_some] or [is_none].  In case, [is_some]
 holds, a value of type ['a] can be obtained by [value_of].  *)

val is_some : 'a option -> bool
val is_none : 'a option -> bool

val value_of : 'a option -> 'a





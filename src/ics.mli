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

  Most of the API functions may throw exeptions in case of misuse of the
  API conventions or other errors.

*)

val version : unit -> unit
  (** Outputs this ICS's version number on {i standard output}. *)

type value = int
    (** {i Values} are representations of all data returned by ICS functions in this API. 
      - Every call to a function returning a value yields a {i unique} value.
      - Values are {i monotonically increasing}, that is, if value [u] has been obtained
      before [v] with calls to API functions, then [u < v]. This holds even if, 
      for example, [Ics.deregister] is called in between. *)

val is_registered : value -> value
  (** [Ics.is_registered v] holds iff value [v] represents 
    currently active ICS data. Most ICS interface functions
    fail when called with a value that does not satisfy this predicate. *)

val deregister : value -> value
  (** [Ics.deregister v] frees the memory associated with the ICS object
    for value [v].  If [Ics.is_registered v] does not hold, deregistration 
    fails. *)

val kind: value -> value
  (** Values are {i partitioned} into {i kinds}, and
    [Ics.kind v] returns a name value describing the 
    partition [v] belongs to. *)

val pp : value -> value
  (** Print an arbitrary value to {i standard output}. *)

val eq : value -> value -> value
  (** [Ics.eq u v] holds iff values [u] and [v] point to the same object 
    in the heap. In particular, [u = v] implies [Ics.eq u v] but [Ics.eq u v] 
    may hold even though [u <> v]. *)


(** {6 Boolean Values} *)

val is_bool : value -> value
  (** If [Ics.is_bool v] equals [Ics.tt()], then [v] is 
    called a {i Boolean value}. *)

val ff : unit -> value
  (** The Boolean value [Ics.ff()] value equals [0]. *)

val tt : unit -> value
  (** The Boolean value [Ics.tt()] value equals [1]. *)


(** {6 Integer values} *)

val is_int : value -> value
  (** If [Ics.is_int v] holds, that is, equal to [Ics.tt()], then
    [v] represents an {i integer value}. *)

val integerize : int -> value
  (** Construct an integer value from an integer. *)


(** {6 Name Values} *)

val is_name : value -> value
  (** [Ics.is_name v] holds iff the value [v] represents a {i name};
    in this case, [v] is a {i name value}.  Equality test [Ics.eq] 
    on name values is constant-time. *)

val intern : string -> value
  (** [Ics.intern str] constructs a corresponding name value [n] from 
    a string. *)

val extern : value -> string
  (** [Ics.extern str v] constructs a string from a name value [v];
    it is inverse to [Ics.intern]. *)


(** {6 Multi-precision arithmetic} *)

val is_rat : value -> value 
  (** If [Ics.is_rat v] holds, then [v] represents a 
    multi-precision rational number. Notice that rational
    values are disjoint from integer values. *)

val rat_of_int : value -> value
  (** [Ics.rat_of_int n] constructs a rational value from an integer value. *)

val rat_of_ints : value -> value -> value
  (** If [u] represents a pair [(n, m)] of rational values, then [Ics.rat_of_ints u]
    constructs a normalized  representation of the rational [n/m]. *)


(** {6 Pairs} *)

val is_pair : value -> value
 (** [Ics.is_pair v] holds if [v] reprents a pair of values. *)

val pair : value -> value -> value
  (** If [u1] is a value of kind [k1] and [u2] a value of kind [k2],
    then the value [Ics.pair u1 u2] represents the {i pair} of [u1] and [u2]. *)
  
val fst : value -> value
  (** If [u] is a pair value, then [Ics.fst u] returns the first
    component of [u]. *)
  
val snd : value -> value
  (** If [u] is a pair value, then [Ics.snd u] returns 
    the second component of [u]. *)



(** {6 Lists} *)

val is_list : value -> value
  (** [Ics.is_list v] holds iff value [v] represents a {i list},
    that is, [v] is built from [Ics.nil()] or from [Ics.cons()]. *)

val is_nil : value -> value
  (** [Ics.is_nil v] holds iff [v] represents the empty list. *)

val nil : unit -> value
  (** [Ics.nil()] represents the empty list. 
    [Ics.kind] is undefined for this value. *)

val cons : value -> value -> value
  (** [Ics.cons u v] builds a list from a value [u] of kind [k]
    and a value [v] of kind [k list]. *)

val head : value -> value
  (** [head v] returns the first value in a list value [v]
    representing the nonempty list. *)

val tail : value -> value
  (** [tail v] returns all but the first value in a list [v]
    representing the nonempty list. *)


(** {6 Theories} *)

val is_theory : value -> value
  (** A {b theory} is associated with each function symbol and
    [is_theory u] holds iff [u] is such a theory representation. *)

val theory_of_name : value -> value
 (** [Ics.theory_of_name s] returns theory [th] for
   - [u]    Theory of uninterpreted function symbols.
   - [la]   Linear arithmetic theory.
   - [p]    Product theory.
   - [bv]   Bitvector theory.
   - [cop]  Coproducts.
    - [nl]   Power products. 
   - [app]  Theory of function abstraction and application. 
   - [arr]  Array theory. 
   - [pset] Theory of propositional sets *)

val description : value -> value
  (** Given a theory value [th] such that [Ics.is_theory th],
    [Ics.description th] print a description of this theory
    on standard output. *)


(** {6 Function symbols} *)

val is_funsym : value -> value
  (** Representation of {i function symbols}. *)

val funsym_make : value -> value -> value
  (** [Ics.sym_make u v] creates a function symbol from theory value [u]
    and name value [v]. *)

val funsym_theory_of : value -> value
  (** [funsym_theory_of f] returns the theory value [th] associated 
    with the function symbol value [f]. *)

val funsym_name_of : value -> value
  (** [funsym_name_of f] returns the name value [n] associated 
    with the function symbol value [f]. *)

(** Add builtin function symbols. *)


(** {6 Constraints} *)

val is_cnstrnt : value -> value
  (** Representation of {i term constraints}. *)

val cnstrnt_int : unit -> value
  (** Integer constraint. *)

val cnstrnt_real : unit -> value
  (** Real number constraint. *)

val cnstrnt_nonint : unit -> value
  (** Real but not integer constraint. *)
  
val cnstrnt_bv : value -> value
  (** Bitvector constraint [cnstrnt u] with [u] an nonnegative integer
    value for constraining the length of bitvectors.  *)


(** {6 Terms} *)

val is_term : value -> value
  (** [Ics.is_term t] holds iff value [t] represents a {i term},
    where terms are either 
    - variables or
    - applications of function symbols to an argument list of terms. *)

val term_mk_var : value -> value
  (** Given a string [s], [Ics.term_mk_var s] constructs 
    a {i term variable} with associated name [s]. The resulting
    term value satisfies [Ics.is_term]. *)

val term_mk_app : value -> value -> value
  (** For a function symbol [f], a term list value [al],
    (in particular, [Ics.is_list al] holds),
    [Ics.term_mk_app s al] constructs an application of 
    the function symbol with name [f] to the argument terms [al].  
    The result satisfies [Ics.is_term]. *)

val term_of_name : value -> value
  (** [Ics.term_of_name] parses a name value according to the grammar 
    for the nonterminal {!Parser.termeof} (see its specification in 
    file [parser.mly]) and builds a corresponding term. 
    The resulting value satisfies [Ics.is_term]. *)
val term_input : unit -> value
  (** [Ics.term_input ()] builds a 
    term value by reading from standard input.  This function is thus similar 
    to [Ics.term_of_string] but input is obtained from a channel instead of
    reading a string. The resulting value [Ics.term_input] satisfies
    [Ics.is_term]. *)

val term_output : value -> value
  (** If [Ics.is_term a] holds, then
    [Ics.term_output a] prints term [a] on standard output. *)

val term_to_name : value -> value
  (** If [Ics.is_term a], then [Ics.term_to_string a] prints 
    a term to a string. This string should be parsable 
    by [Ics.term_of_string]. *)

(** {6 Derived Term Constructors} *)

val term_mk_num : value -> value
val term_mk_multq : value -> value -> value
val term_mk_add : value -> value -> value
val term_mk_mult : value -> value -> value



(** {6 Atoms} *)

val is_atom : value -> value
  (** Recognizer for {i atom values} *)

val atom_of_name : value -> value
  (** Parsing a name value to obtain an atom. *)

val atom_to_name : value -> value
  (** Printing an atom value to a name value. *)
     
val atom_mk_true : unit -> value
  (** Constructing the trivially true atom. *)

val atom_mk_false : unit -> value
  (** Constructing the trivially unsatisfiable atom. *)

val atom_mk_equal  : value -> value -> value
  (** [atom_mk_equal a b] constructs an atom value for representing
    the equality between term values [a] and [b]. *)

val atom_mk_diseq  : value -> value -> value
  (** [atom_mk_diseq a b] constructs an atom value for representing
    the disequality of term values [a] and [b]. *)

val atom_mk_le : value -> value -> value
  (** [atom_mk_le a b] constructs an atom value for representing [a <= b]. *)

val atom_mk_lt : value -> value -> value
 (** [atom_mk_lt a b] constructs an atom list value for representing [a < b]. *)

val atom_mk_ge : value -> value -> value
  (** [atom_mk_ge a b] constructs an atom value for representing [a >= b]. *)

val atom_mk_gt : value -> value -> value
  (** [atom_mk_gt a b] constructs an atom list value for representing [a > b]. *)

val atom_negate : value -> value
  (** Constructs the negation of an atom. *)


(** {6 Justifications} *)

val is_justification : value -> value
  (** A {i Justification} is an unsatisfiable conjunction of atoms. *)

val justification_to_atoms : value -> value
  (** Returns a justification as an atom list value. *)


(** {6 Processing} *)

val is_context: value -> value
  (** A {i logical context} represents a conjunction of atoms. *)

val context_ctxt_of : value -> value
  (** [context_ctxt_of s] returns the logical context of [s] as a set of atoms. *)
  
val context_occ : value -> value -> value -> value
  (** [context_occ th s x] iff [x] occurs in the is in the equality set 
    for theory [th] in [s]. *)
  
val context_find : value -> value -> value -> value
  (** [find th s x] is [a] if [x = a] is in the solution set for theory [th]
    in [s]; otherwise, the result is just [x]. *)
  
val context_inv : value -> value -> value
  (** [inv s a] is [x] if there is [x = a] in the solution set for
    theory [th]; otherwise [Not_found] is raised. *)
  
val context_use : value -> value -> value -> value
  (** [use th s x] consists of the set of all term variables [y] such
    that [y = a] in [s], and [x] is a variable [a]. *)

val context_empty : unit -> value
  (** [context_empty()] represents the empty logical context. *)

val is_status : value -> value
  (** Inhabitants of type status are used as return values for {!Ics.process}.
    There are three possible outcomes.
    - [Redundant] implies the argument [a] in {!Ics.process}[s a] is valid in context [s].
    - [Inconsistent] implies the argument [a] conjoined with [s] in {!Ics.process}[s a] is inconsistent.
    - [Consistent] neither a redundancy nor an inconsistency could be detected. *)
    
val is_consistent   : value -> value
val is_redundant    : value -> value
val is_inconsistent : value -> value

val d_consistent : value -> value
  (** In case [is_consistent st] holds, [d_consistent st] returns the extended context. *)
    
val process : value -> value -> value
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

val can : value -> value -> value
  (** Given a logical context [s] and an atom [a],
    [can s a] computes a semicanonical form of [a] in [s], that is,
    - if [a] holds in [s] it returns [Atom.True], 
    - if the negation of [a] holds in [s] then it returns [Atom.False], and, otherwise,
    - an equivalent normalized atom built up only from variables is returned. *)


(** {6 Propositional logic} *)

val is_prop : value -> value
  (** Representation of propositional formulas with propositional 
    variables and atoms as literals. A propositional formual is either
    - one of the propositional constants [tt], [ff]
    - a propositional variable [x],
    - a literal [l] with [l] an atom (atoms are closed under negation),
    - a conjunction [p1 & ... & pn],
    - a disjunction [p1 | ... | pn],
    - a negation [~p]. *)

val prop_of_name : value -> value
  (** Parsing a string to obtain a propositional formula.
    The syntax of propositional formulas is roughly given by
    the grammar above, and brackets [[]] are used for grouping.
    For details of the grammar see file [parser.mly]. *)

val prop_to_name : value -> value
  (** Pretty-print a propositional variable to a string. *)
 
val prop_mk_true : unit -> value
  (** The trivially true propositional formula. *)

val prop_mk_false : unit -> value
  (** The trivially false propositional formula. *)

val prop_mk_var : value -> value
  (** Given a name value [n], that is [Ics.is_name n] holds,
    [Ics.prop_mk_var n] constructs a propositional variable [p].
    In particular, [Ics.is_prop p] holds. *)

val prop_mk_poslit : value -> value
  (** Injecting an atom into a propositional formula. *)

val prop_mk_neglit : value -> value
 (** Injecting a negated atom into a propositional formula. *)

val prop_mk_ite : value -> value -> value -> value
 (** [prop_mk_ite p q r] constructs a propositional formula
   equivalent to [prop_mk_disj (prop_mk_conj p q) (prop_mk_conj (prop_mk_neg p) r)]. *)

val prop_mk_conj : value -> value
  (** [prop_mk_conj [p1;...;pn]] constructs a representation of the 
    conjunction of [p1 & ... & pn] with the empty list [[]] equivalent to [prop_mk_true()]. *)

val prop_mk_disj : value -> value
 (** [prop_mk_disj p q] constructs a representation of the disjunction of [p] and [q]. *)

val prop_mk_iff : value -> value ->value
 (** [prop_mk_iff p q] constructs a representation of the equivalence of [p] and [q]. *)

val prop_mk_neg : value -> value
 (** [prop_mk_neg p] constructs a representation of the negation of [p]. *)

val prop_sat : value -> value -> value
  (** [prop_sat s p] determines if the propositional formula 
    [p] is satisfiable in context [s].  It returns
    - [None], if [p] is unsatisfiable,
    - [Some(ms)], if [p] is satisfiable; in this case, [ms] 
    implicitly represents a set of candidate models. *)


(** {6 Imperative states} *)

(** An imperative state [istate] does not only include a logical 
 context of type [state] but also a symbol table and input and 
 output channels. A global [istate] variable is manipulated and
 destructively updated by commands. *)

val init : value -> unit
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

val cmd_batch : unit -> value
  (** [Ics.cmd_batch ()] reads
    commands from standard input. In contrast, to [Ics.cmd_rep],
    however, syntax error messages contain line numbers and processing is 
    aborted after state is unsatisfiable. *)

val flush : unit -> unit
  (** Flush currently active output channel. *)



(** {6 Parameters} *)

val set : value -> value -> unit
  (** [set name v] sets the value of parameter [name] to [v]. *)

val get : value -> value
  (** [get name] gets the value of parameter [name]. *)



(** {6 Controls} *)

val reset : unit -> unit
  (** [reset()] clears all the global tables. This does not only 
    include the current context but also internal tables used for 
    hash-consing and memoization purposes. *)
    
val gc : unit -> unit
  (** [gc()] triggers a full major collection of ocaml's garbage collector. *)

val sleep : value -> unit
  (** Sleeping for a number of seconds. *)



(*s {\bf Terms.} The API provides an abstract notion of terms, together
    with constructors, test functions and destructors. 
    The type [q] of multi-precision rational numbers is abstract. *)

type term

type q

(*s Constructors. *)

val var : string -> term

val is_int : term -> term
val is_real : term -> term
val is_pos : term -> term
val is_neg : term -> term
val is_nonpos : term -> term
val is_nonneg : term -> term
    
val app : term -> term list -> term
    
val num    : q -> term
val div    : q -> q -> term
val plus   : term list -> term
val plus2  : term -> term -> term    
val minus  : term -> term -> term
val unary_minus : term -> term
val times  : term list -> term
val times2 : term -> term -> term

val tup    : term list -> term
val proj   : int -> int -> term -> term
    
val update : term -> term -> term -> term
    
val equal  : term -> term -> term
val diseq  : term -> term -> term
val lt     : term -> term -> term
val le     : term -> term -> term
val gt     : term -> term -> term
val ge      : term -> term -> term
val integer_pred : term -> term
val mem    : term -> term -> term

val ptrue  : unit -> term
val pfalse : unit -> term
val ite    : term -> term -> term -> term

val neg : term -> term
val conj : term -> term -> term
val disj : term -> term -> term
val xor : term -> term -> term
val imp : term -> term -> term
val iff : term -> term -> term

val empty_set : int -> term
val full_set  : int -> term
val setite    : term -> term -> term -> term
val compl     : int -> term -> term
val inter     : int -> term -> term -> term
val union     : int -> term -> term -> term
val diff      : int -> term -> term -> term
val sym_diff  : int -> term -> term -> term

val bv_eps  : unit -> term
val bv_zero : int -> term
val bv_one  : int -> term
val bv_const: string -> term
val bv_conc : int * term -> int * term -> term
val bv_extr : int * term -> int -> int -> term
val bv_and  : int -> term -> term -> term
val bv_or   : int -> term -> term -> term
val bv_xor  : int -> term -> term -> term

val unsigned: term -> term

val fresh : term list -> term
val new_var : string -> term
    
(*s Test functions. *)

val is_var : term -> bool
val is_app : term -> bool
val is_ite : term -> bool

val is_tup : term -> bool
val is_proj : term -> bool

val is_lookup : term -> bool
val is_update : term -> bool
    
val is_ptrue  : term -> bool
val is_pfalse : term -> bool
val is_equal  : term -> bool

val is_empty_set : term -> bool
val is_full_set  : term -> bool
val is_compl     : term -> bool
val is_union     : term -> bool
val is_inter     : term -> bool
val is_setite    : term -> bool





(*s Structural equality and ordering over terms. *)

val eq_term : term -> term -> bool

val compare : term -> term -> int

(*s {\bf States.} States containing processed equalities,
    dis-equalities, inequalities, etc. are implemented by the
    following abstract type [state]. *)

type state = State.t

val empty_state : unit -> state
  
(*s When processing a new atom (an equality or a dis-equality), the result 
    may be of several kinds.
    Either we get a new consistent state, or the atom was already valid in
    the current state, or we get an inconsistent state. The following type
    [result] is used to describe all these cases. *)

type result = Dp.result

val is_consistent   : result -> bool
val is_redundant    : result -> bool
val is_inconsistent : result -> bool

(*s Processing of new atoms is provided by the following two functions
    [process] and [process_list]. *)

val process : state -> term -> result

(*s A given state may be used to ask for the validity or the unsatisfiability
    of a given proposition. One can also ask for the mapping of a term in a
    given state, or for the canonized form of a term. *)

val is_valid : state -> term -> bool
val is_unsat : state -> term -> bool

val find : state -> term -> term
val use  : state -> term -> term list
val universe : state -> term -> bool

val norm : state -> term -> term
val canon : state -> term -> term

val polarity : state -> term -> unit
    
val typ : state -> term -> unit
    

(*s {\bf Controls.} The following functions are provided to control
    the implementation of the decision procedure.  [reset] clears the
    global tables used in the code (for hash-consing and memoization
    purposes). [gc] triggers a full major collection of ocaml's
    GC (Should not be used, however). [set_verbose] controls the messages
    printed by the code. Default is 0, which means that nothing is
    printed. The higher the value, the more numerous the messages
    are. *)

val reset : unit -> unit
val gc : unit -> unit
val set_verbose : int -> unit

(*s {\bf Pretty-printing.} The following functions are mostly exported for
    debugging purposes. *)

val pp_term : term -> unit
    
val pp_find : state -> unit
val pp_use : state -> unit
val pp_universe : state -> unit

val flush : unit -> unit

(*s {\bf Imperative API.} All the functions presented up to that point were
    purely functional and all the data-types were persistent. In the 
    following, we also provide an imperative API, for use in an imperative
    context. \par
    There is a new data-type for states, [istate], which is updated in-place.
    The function [current_state] returns the value of type [state] contained
    in a given imperative state, so that all the previous functions 
    ([canon], [is_valid], etc.) can be reused on an imperative state.
    The function call [(iprocess s a)] processes an atom [a] in a given 
    imperative state [s], returning a value [v] of type [result].
    If the result is a new consistent state, then [s] is updated 
    in-place, and the state contained in [v] can be discarded (it is now
    equal to [current_state s]). \par
    The imperative states are equipped with an internal notion of stack,
    handled using the [push] and [pop] functions. *)

type istate

val empty_istate : unit -> istate
val current_state : istate -> state

val iprocess : istate -> term -> result

val push : istate -> unit
val pop : istate -> unit

(*i Exports for the purpose of the test program. 
 *)

module Tmap : sig
  type 'a t
  val empty : 'a t
  val add : term -> 'a -> 'a t -> 'a t
  val find : term -> 'a t -> 'a
  val remove : term -> 'a t -> 'a t
  val mem :  term -> 'a t -> bool
  val iter : (term -> 'a -> unit) -> 'a t -> unit
  val map : ('a -> 'b) -> 'a t -> 'b t
  val fold : (term -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
end

val sigma : State.t -> term -> term
val solve : State.t -> term * term -> (term * term) list

(*i*)


(*i Exports for Lisp. *)

val num_of_int : int -> q
val string_of_num : q -> string
val num_of_string : string -> q

val is_nil : 'a list -> bool
val cons : 'a -> 'a list -> 'a list
val head : 'a list -> 'a
val tail : 'a list -> 'a list

val pair : 'a -> 'b -> 'a * 'b
val fst : 'a * 'b -> 'a
val snd : 'a * 'b -> 'b 

val tag : term -> int

(*i*)

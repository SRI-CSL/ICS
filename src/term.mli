
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

(*s Terms are the basic data structures of ICS. *)

(*i*)
open Hashcons
open Mpa
(*i*)

(*s Terms.  A term is either a variable [Var(s)], where the name [s] is a string, an
 application [App(f,l)] of a `function symbol' to a list of arguments, an update
 expression [Update(a,i,v)], or a term interpreted in one of the theories of linear
 arithmetic [Arith], propositional logic, [Prop], propositional sets [Set], tuples
 [Tuple], or bitvectors [Bv]. By definition, all entitities of type [t] are hash-consed.
 Therefore, equality tests between terms can be done in constant time using the equality
 [===] from the module [Hashcons].

 Arithmetic terms are either numerals of the form [Num(q)], n-ary addition [Add(l)],
 linear multiplication [Multq(q,a)], nonlinear multiplication [Mult(l)], or division.
 Arithmetic terms built up solely from [Num], [Add], and [Multq] are considered to
 be interpreted, since [Mult] and [Div] are considered to be uninterpreted, in general.
 However, certain simplification rules for these uninterpreted function symbols are
 built-in.

 Propositional terms are either [False], [True], or conditionals [Ite(a,b,c)].
 Other propositional connectives can be encoded using these constructor.

 A tuple term is either a tuple [Tup(l)] or the [i]-th projection [Proj(i,n,_)]
 from an [n]-tuple.
	
 Set of terms are implemented using Patricia trees.  Operations
 on these sets are described below in the submodule [Set]. *)


type t =
  | Var of Var.t
  | App of Sym.t * t list
 

(*s Constructing and destructing terms *)

val mk_var : Name.t -> t
val mk_const : Sym.t -> t
val mk_app : Sym.t -> t list -> t

val mk_fresh_var : Name.t -> int option -> t
val is_fresh_var : t -> bool
val name_of : t -> Name.t

val destruct : t -> Sym.t * t list

val sym_of : t -> Sym.t
val args_of : t -> t list

(*s Equality of terms. *)

val eq : t -> t -> bool

val eql : t list -> t list -> bool

val cmp : t -> t -> int

val (<<<): t -> t -> bool

val orient : t * t -> t * t

val max : t -> t -> t
val min : t -> t -> t

(*s Test if term is a constant. *)

val is_const : t -> bool

val is_var : t -> bool
val is_app : t -> bool


val is_interp_const : t -> bool
 

(*s Fold operator on terms. *)

val fold : (t -> 'a -> 'a) -> t -> 'a -> 'a


(*s Iteration operator on terms. *)

val iter : (t -> unit) -> t -> unit


(*s Predicate holds for all subterms. *)

val for_all : (t -> bool) -> t -> bool

(*s [subterm a b] holds if [a] is a subterm of [b]. *)

val subterm : t -> t -> bool

(*s [occurs x a] holds if term [x] occurs in [a]. *)

val occurs : t -> t -> bool

    
(*s Mapping over list of terms. Avoids unnecessary consing. *)

val mapl : (t -> t) -> t list -> t list


(*s Association lists for terms. *)

val assq : t -> (t * 'a) list -> 'a


(*s Printer. *)

val set_pretty_print : bool -> unit

val pp : Format.formatter -> t -> unit

val to_string : t -> string


(*s Pretty-printing pairs as equalities/disequalities/constraints. *)

val pp_equal : (t * t) Pretty.printer
val pp_diseq : (t * t) Pretty.printer
val pp_in : (t * Cnstrnt.t) Pretty.printer

(*s Sets and maps of terms. *)

type trm = t

module Set : (Set.S with type elt = trm)

module Map : (Map.S with type key = trm)

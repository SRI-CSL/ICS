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

(** Function symbols

  @author Harald Ruess

 This module provides constructors for all the function symbols
 for the theories in {!Th.t}.
*)

(**
  The set of {b function symbols} is partitioned into {i uninterpreted}
  and {i interpreted} function symbols, and the interpreted function
  symbols are partitioned themselves into function symbols for the 
  interpreted theories in {!Th.t} of 
  - linear arithmetic,
  - products,
  - coproducts,
  - bitvectors,
  - functional arrays,
  - power products, and
  - function abstraction and application.

  In particular, function symbols from different theories are {i pairwise
  disjoint}. Some theories contain an infinite number of function and constant 
  symbols. For example, the theory of linear arithmetic contains a constant 
  symbol for every rational number.

  This module provides functions for creating, manipulating, and comparing
  function symbols.  Since representations of function symbols are 
  {i hashconsed}, equality tests {!Sym.eq} and comparison {!Sym.cmp} 
  are performed in constant time.

  Symbols should only be constructed using the explicitly provided 
  constructors as the definition of the type {!Sym.sym} is only given
  to allow for convenient pattern matching. 

  Also, {!Tools.do_at_reset} resets internal structures and therefore
  invalidates all uses of symbols. In particular, symbols should never
  be stored in global constants. 
*)

type t = sym * int
  (** Representation type for function symbols. This includes the
    function symbol itself together with a hash value. This type
    is purposely not kept abstract as to allow for pattern matching. *)

and sym = 
  | Uninterp of uninterp
  | Arith of arith
  | Product of product
  | Coproduct of coproduct
  | Bv of bv
  | Pp of pprod
  | Cl of cl
  | Arrays of arrays 
  | Propset of propset


(** An uninterpreted function symbol in {!Th.u} just consists of a name.
  There is no {i arity} associated with it. *)
and uninterp = Name.t


(** Function symbols for linear arithmetic {!Th.la} are
  - [Num(q)] for representing rational number [q],
  - [Add] for addition,
  - [Multq(q)] for multiplication by a rational [q]. *)
and arith = 
  | Num of Mpa.Q.t  
  | Add
  | Multq of Mpa.Q.t


(** Function symbols for the theory {!Th.p} of products are
  - [Cons] for constructing pairs
  - [Car] for projection to first component
  - [Cdr] for projection to second component. *)
and product = Cons | Car | Cdr


(** Function symbols for the theory {!Th.cop} of cotuples are
  - [In(Left)] for left injection,
  - [In(Right)] for right injection,
  - [Out(Right)] for right unpacking,
  - [Out(Left)] for left unpacking. *)
and coproduct = 
  | In of direction 
  | Out of direction

and direction = Left | Right


(** Function symbols of the theory {!Th.arr} of arrays
  - [Create] for creating constant arrays,
  - [Select] for array lookup, and
  - [Update] for array update. *)
and arrays = 
  | Create
  | Select 
  | Update


(** Function symbols of the theory {!Th.app} of functions
  - Apply of function application, and
  - combinators [S], [K], [I] *)
and cl = 
  | Apply
  | S
  | K
  | I
  | C
  | Reify of t * int

and propset = Empty | Full | Ite


(** Function symbols for the theory {!Th.bv} of bitvectors are
  - [Const(b)] for constructing constant bitvectors such as [0111001].
  - [Conc(n, m)], for integers [n, m >= 0], for concatenating bitvectors of width [n] and [m],
  - [Sub(i, j, n)], for integers [0 <= i <= j < n], for extracting bits [i] through [j] in 
  a bitvector of width [n]. *)
and bv =
  | Const of Bitv.t
  | Conc of int * int
  | Sub of int * int * int


(** Function symbols of the theory {!Th.nl} of nonlinear arithmetic
  or {i power products} are
  - [Mult] for nonlinear multiplication *)
and pprod = 
  | Mult


type tsym = t 
    (** nickname *)


val theory_of : t -> Th.t
  (** [theory_of f] returns the theory of type {!Th.t} associated with [f]. *)
  

val eq : t -> t -> bool
  (** [eq f g] succeeds iff [f] and [g] represent the same function symbol.
    This test is performed in constant time (in particular, independent
    of the length of names of uninterpreted function symbols). *)


val cmp : t -> t -> int
  (** [cmp f g] returns [0] iff [eq f g] holds, and [cmp f g] is positive
    iff [cmp g f] is negative. Otherwise, the result is unspecified. 
    [cmp f g] might thus be viewed as representing a {i total ordering} [<=] on 
    function symbols with [f <= g] iff [cmp f g] is, say, nonpositive. *)


val hash : t -> int
  (** Nonnegative hash value for function symbols. This value is not
    unique to a function symbol. *)


val pp : 'a Pretty.printer -> (t * 'a list) Pretty.printer
  (** Pretty-printing applications of symbols to an argument list.
    The exact form of printing applications is determined by the 
    flag {!Pretty.flag}. For details see also {!Sym.Uninterp.pp} etc.
    below. *)


(** Operations on {i uninterpreted function symbols}.
  - [get f] returns the uninterpreted function symbol associated with [f],
  - [make n] constructs a hashconsed representation of an uninterpreted function
  symbol of name [n], and
  - [pp p fmt (op, al)] pretty-prints the appliation of [op] to the list of 
  arguments [al] depending on the value of {!Pretty.flag}. See also {!Pretty.apply}. *)
module Uninterp : sig
  val get : t -> uninterp
  val make : Name.t -> t
  val is : t -> bool
  val pp : 'a Pretty.printer -> (uninterp * 'a list) Pretty.printer
end


(** Operation on {i linear arithmetic} function symbols. 
  - [mk_num q], [mk_multq q], and [mk_add] construct hashconsed function
  symbols for {i representing} [Num(q)], [Multq(q)], and [Add], respectively.
  - [get f] returns the arithmetic function symbol represented by [f],
  or raises [Not_found]. 
  - The tests [is_num f], [is_multq f], [is_add f] succeed iff [f] represents
  a [Num _], [Multq _], or [Add], respectively.
  - [d_num f] ([d_multq f]) returns [q] if [f] represents [Num(q)] ([Multq(q)]) and 
  raises [Not_found] otherwise.
  - [pp p fmt (op, al)] pretty-prints the appliation of [op] to the list of 
  arguments [al] depending on the value of {!Pretty.flag}. In particular,
  if {!Pretty.flag} is set to [Pretty.Mixfix], then the application of 
  addition is printed infix. See also {!Pretty.apply}. It assumes a 
  {i constant} application to [Num _], a {i unary} application to [Multq _], 
  and an {i nary} application to [Add]. *)
module Arith : sig
  val mk_num : Mpa.Q.t -> t
  val mk_multq : Mpa.Q.t -> t
  val mk_add : t 
  val get : t -> arith
  val is_num : t -> bool
  val is_multq : t -> bool
  val is_add : t -> bool
  val d_num : t -> Mpa.Q.t
  val d_multq : t -> Mpa.Q.t
  val pp : 'a Pretty.printer -> (arith * 'a list) Pretty.printer
 end 


(** Operation on function symbols of the {i product} theory {!Th.p}. 
  - [mk_cons], [mk_car], and [mk_cdr] are the function symbols
  {i representing} [Cons], [Car], [Cdr], respectively.
  - [get f] returns the function symbol in {!Th.p} represented by [f],
  or raises [Not_found]. 
  - The tests [is_cons f], [is_car f], [is_cdr f] succeed iff [f] 
  represents [Cons], [Car], or [Cdr], respectively.
  - [pp p fmt (op, al)] pretty-prints the appliation of [op] to the list 
  of arguments [al] depending on the value of {!Pretty.flag}. See  also
  {!Pretty.apply}. It assumes a unary application to [Car] and [Cdr] and
  a binary application to [Cons]. *)
module Product : sig
  val mk_cons : t
  val mk_car : t
  val mk_cdr : t  
  val get : t -> product
  val is_cons : t -> bool
  val is_car : t -> bool
  val is_cdr : t -> bool
  val pp : 'a Pretty.printer -> (product * 'a list) Pretty.printer
end


(** Operation on function symbols of the {i coproduct} theory {!Th.cop}. 
  - [mk_inl], [mk_inf], [mk_outl] and [mk_outr] are the function symbols
  {i representing} [In(Left)], [In(Right)], [Out(Right)], and [Out(Left)], respectively.
  - [get f] returns the function symbol in {!Th.cop} represented by [f],
  or raises [Not_found]. 
  - The tests [is_inl f], [is_inr f], [is_outl f], and [is_outr] succeed iff [f] 
  represents [InL], [In(Right)], [Out(Left)], or [Out(Right)], respectively.
  - [pp p fmt (op, [a])] pretty-prints the appliation of [op] to the 
  unary list [[a]] depending on the value of {!Pretty.flag}. See 
  also {!Pretty.apply}. *)
module Coproduct : sig
  val mk_inl : t
  val mk_inr : t
  val mk_outl : t
  val mk_outr : t  
  val get : t -> coproduct
  val is : t -> bool
  val is_inl : t -> bool
  val is_inr : t -> bool
  val is_outl : t -> bool
  val is_outr : t -> bool
  val pp : 'a Pretty.printer -> (coproduct * 'a list) Pretty.printer
end


(** Operation on function symbols of {i nonlinear multiplication} theory {!Th.nl}. 
  - [mk_mul], [mk_expt n], [mk_outl] are the function symbols
  {i representing} [Mult] and [Expt(n)], respectively.
  - [get f] returns the function symbol in {!Th.nl} represented by [f],
  or raises [Not_found]. 
  - The test [is_mult f] ([is_expt f]) succeeds iff [f] 
  represents [Mult] (some [Expt(n)]). 
  - [d_expt f] returns [n] if [f] represents [Expt(n)]; otherwise
  [Not_found] is raised.
  - [pp p fmt (op, al)] pretty-prints the appliation of [op] to the 
  argument list [al] depending on the value of {!Pretty.flag}. See 
  also {!Pretty.apply}. It assumes applications to [Expt(n)] to be
  {i unary}, whereas [Mult] is considered to be an {i nary} function
  symbol. *)
module Pprod : sig
  val mk_mult : t
  val get : t -> pprod
  val is : t -> bool
  val is_mult : t -> bool
  val pp : 'a Pretty.printer -> (pprod * 'a list) Pretty.printer
end



(** Operation on function symbols of the {i bitvector} theory {!Th.bv}. 
  - Constructors [mk_const b], [mk_conc n m], [mk_sub n i j]
  for {i representing} [Const(b)], [Conc(n, m)], and [Sub(n, i, j)], respectively.
  - [get f] returns the function symbol in {!Th.bv} represented by [f],
  or raises [Not_found]. 
  - The test [is_const f] ([is_conc f], [is_sub f]) succeeds iff [f] 
  represents some [Const(b)] ([Conc(n, m)], [Sub(n, i, j)]).
  - [d_conc f] ([d_const f], [d_sub f]) yields [b] ([(n, m)], [(n, i, j)])
  if [f] represents [Const(b)] ([Conc(n, m)], [Sub(n, i, j)]); otherwise
  [Not_found] is raised.
  - [pp p fmt (op, al)] pretty-prints the appliation of [op] to the 
  argument list [al] depending on the value of {!Pretty.flag}. See 
  also {!Pretty.apply}. It assumes applications to [Const(.)] to be
  {i nullary}, applications to [Conc(.,.)] are {i binary}, and [Sub(.,.,.)]
  is {i unary}. 
  - The {i width} associated with each bitvector function symbol [op] is
   returned by [width op].  For constant bitvector symbols, this width
  is defined by the length of the associated bitvector of type {!Bitv.t},
  the width of [Conc(n, m)] is [n + m], and the width of [Sub(n, i, j)]
  is [j - i + 1], namely the width of the extracted sub-bitvector. *)
module Bv: sig
  val get : t -> bv
  val mk_const : Bitv.t -> t
  val mk_conc : int -> int -> t
  val mk_sub : int -> int -> int -> t
  val is : t -> bool
  val is_const : t -> bool
  val is_conc : t -> bool
  val is_sub : t -> bool
  val d_const : t -> Bitv.t
  val d_conc : t -> int * int
  val d_sub : t -> int * int * int
  val width : bv -> int
  val pp : 'a Pretty.printer -> (bv * 'a list) Pretty.printer
end


(** Operation on function symbols of the theory {!Th.arr}
  of {i functional arrays}. 
  - Constants [mk_create], [mk_select], [mk_update] for {i representing} 
  [Create], [Select], [Update], respectively.
  - [get f] returns the function symbol in {!Th.arr} represented by [f],
  or raises [Not_found]. 
  - The test [is_create f] ([is_select f], [is_update f]) succeeds iff [f] 
  represents some [Create] ([Select], [Update]).
  - [pp p fmt (op, al)] pretty-prints the appliation of [op] to the 
  argument list [al] depending on the value of {!Pretty.flag}. See 
  also {!Pretty.apply}. It assumes applications to [Create] to be
  {i unary}, [Select] is {i binary}, and [Update] is {i ternary}. *)
module Array : sig
  val get : t -> arrays
  val mk_create : t
  val mk_select : t
  val mk_update : t
  val is : t -> bool
  val is_create : t -> bool
  val is_select : t -> bool
  val is_update : t -> bool
  val pp : 'a Pretty.printer -> (arrays * 'a list) Pretty.printer
end

   
(** Operation on function symbols of the theory {!Th.app} of
  {i combinatory logic} with case split.
  - Constructors [mk_apply r] and [mk_abs] {i representing} 
    [Apply(r)] and [Abs], respectively.
  - [get f] returns the function symbol in {!Th.app} represented by [f],
  or raises [Not_found]. 
  - The test [is_apply f] ([is_abs f]) succeeds iff [f] 
    represents some [Apply(r)] ([Abs]).
  - [pp p fmt (op, al)] pretty-prints the appliation of [op] to the 
  argument list [al] depending on the value of {!Pretty.flag}. See 
  also {!Pretty.apply}. It assumes applications to [Apply(.)] to be
  {i binary} and [Abs] is {i unary}. *)
module Cl : sig
  val get : t -> cl
  val apply : t
  val s : t
  val k : t
  val i : t
  val c : t
  val reify : tsym * int -> t
  val is : t -> bool
  val is_apply : t -> bool
  val is_s : t -> bool
  val is_k : t -> bool
  val is_i : t -> bool
  val is_c : t -> bool
  val is_reify : t -> bool
  val d_reify : t -> tsym * int
  val pp : 'a Pretty.printer -> (cl * 'a list) Pretty.printer
end

(** Theory of {i propositional sets} *)
module Propset : sig
  val get : t -> propset
  val mk_empty : t
  val mk_full : t
  val mk_ite : t
  val is : t -> bool
  val is_empty : t -> bool
  val is_full : t -> bool
  val is_ite : t -> bool
  val pp : 'a Pretty.printer -> (propset * 'a list) Pretty.printer
end


val get : t -> sym
  (** [get f] returns a theory-specific operator together with
    a tag for specifying the theory corresponding to [f]. *)
  

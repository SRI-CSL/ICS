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

(** {b Function symbols}

  @author Harald Ruess
  @author N. Shankar
  
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
*)

type t
  (** Representation type for function symbols. *) 


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


(** {6 Uninterpreted Function Symbols} *)

(** An uninterpreted function symbol in {!Th.u} just consists of a name.
  There is no {i arity} associated with it. *)
type uninterp = Name.t

(** Operations on uninterpreted function symbols.
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


(** {6 Linear Arithmetic} *)

(** Function symbols for linear arithmetic {!Th.a} are
  - [Num(q)] for representing rational number [q],
  - [Add] for addition,
  - [Multq(q)] for multiplication by a rational [q]. *)
type arith = 
  | Num of Mpa.Q.t  
  | Add
  | Multq of Mpa.Q.t
     

(** Operation on linear arithmetic function symbols. 
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
  if {!Pretty.flag} is set to {!Pretty.Mixfix}, then the application of 
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


(** {6 Products} *)

(** Function symbols for the theory {!Th.p} of products are
  - [Cons] for constructing pairs
  - [Car] for projection to first component
  - [Cdr] for projection to second component. *)
type product = Cons | Car | Cdr


(** Operation on function symbols of the product theory {!Th.p}. 
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


(** {6 Coproducts} *)

(** Function symbols for the theory {!Th.cop} of cotuples are
  - [In(Left)] for left injection,
  - [In(Right)] for right injection,
  - [Out(Right)] for right unpacking,
  - [Out(Left)] for left unpacking. *)
type coproduct = 
  | In of direction 
  | Out of direction

and direction = Left | Right

(** Operation on function symbols of the product theory {!Th.cop}. 
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


(** {6 Power products} *)

(** Function symbols of the theory {!Th.nl} of nonlinear arithmetic
  or {i power products} are
  - [Mult] for nonlinear multiplication,
  - [Expt(n)] for exponentiation with integer [n]. *)
type pprod = 
  | Mult
  | Expt of int

(** Operation on function symbols of the product theory {!Th.nl}. 
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
  val mk_expt : int -> t  
  val get : t -> pprod
  val is : t -> bool
  val is_mult : t -> bool
  val is_expt : t -> bool
  val d_expt : t -> int
  val pp : 'a Pretty.printer -> (pprod * 'a list) Pretty.printer
end


(** {6 Bitvectors} *)

(** Function symbols for the theory {!Th.bv} of bitvectors are
  - [Const(b)] for constructing constant bitvectors such as [0111001].
  - [Conc(n, m)], for integers [n, m >= 0, for concatenating bitvectors of width [n] and [m],
  - [Sub(i, j, n)], for integers [0 <= i <= j < n], for extracting bits [i] through [j] in 
  a bitvector of width [n]. *)
type bv =
  | Const of Bitv.t
  | Conc of int * int
  | Sub of int * int * int
  

(** Operation on function symbols of the product theory {!Th.bv}. 
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


(** {6 Functional Arrays} *)

(** Function symbols of the theory {!Th.arr} of arrays
  - [Create] for creating constant arrays,
  - [Select] for array lookup, and
  - [Update] for array update. *)
type arrays = 
  | Create
  | Select 
  | Update
    
(** Operation on function symbols of the product theory {!Th.arr}. 
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


(** {6 Applicaton and Abstraction} *)

(** Function symbols of the theory {!Th.app} of functions
  - [Apply of function application, and
  - [Abs] of function abstraction. *)
type apply = 
  | Apply
  | Abs

   
(** Operation on function symbols of the product theory {!Th.app}. 
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
module Fun : sig
  val get : t -> apply
  val apply : t
  val abs : t
  val is : t -> bool
  val is_apply : t -> bool
  val is_abs : t -> bool
  val pp : 'a Pretty.printer -> (apply * 'a list) Pretty.printer
end

(** {6 Theory of propositional sets} *)

type propset = Empty | Full | Ite

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




(** {6 Partitioning} *)

type sym = 
  | Uninterp of uninterp
  | Arith of arith
  | Product of product
  | Coproduct of coproduct
  | Bv of bv
  | Pp of pprod
  | Fun of apply
  | Arrays of arrays 
  | Propset of propset

val get : t -> sym
  (** [get f] returns a theory-specific operator together with
    a tag for specifying the theory corresponding to [f]. *)
  

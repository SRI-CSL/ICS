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

(** Datatype for function symbols.  The set of function symbols is
  partitioned into function symbols for the theories of
  - uninterpreted functions
  - linear arithmetic
  - pairs
  - cotuples
  - bitvectors
  - arrays
  - power products
  - function abstraction and application
  - arithmetic interpretations of bitvectors. *)


type arith = 
  | Num of Mpa.Q.t  
  | Add
  | Multq of Mpa.Q.t
      (** Function symbols for linear arithmetic are
	- [Num(q)] for a rational number [q],
	- [Add] for addition,
	- [Multq(q)] for multiplication by a rational [q]. *)

type pair =
  | Cons
  | Car
  | Cdr
      (** Function symbols for the theory of Pairs. *)


type coproduct = InL | InR | OutL | OutR
    (** Function symbols for the theory of cotuples are
      - [InL] for left-injection,
      - [InR] for right-injection,
      - [OutR] for right-unpacking,
      - [OutL] for left-unpacking. *)


type bv =
  | Const of Bitv.t
  | Conc of int * int
  | Sub of int * int * int
      (** Function symbols for the theory of bitvectors are
	- [Const(b)] for constructing constant bitvectors,
	- [Conc(n, m)] for concatenating bitvectors of length [n] and [m],
	- [Sub(i,j,n)] for extracting bits [i] through [j] in a bv of length [n],
	- [Bitwise(n)] for bitwise conditional on bitvectors of length [n]. *)

type pprod = 
  | Mult
  | Expt of int
      (** Function symbols of the theory of power products are
	- [Mult] for nonlinear multiplication,
	- [Expt(n)] for exponentiation with integer [n]. *)

type apply = 
  | Apply of Dom.t option
  | Abs
      (** Function symbols of the theory of functions
	- [Apply(r)] of function application
	- [Abs] of function abstraction *)

type arrays = 
  | Create
  | Select 
  | Update
      (** Function symbols of the theory of arrays
	- [Select] for array lookup
	- [Update] for array update. *)


type t = 
  | Uninterp of Name.t       (* Uninterpreted function symbols. *)
  | Arith of arith           (* Linear arithmetic function symbols. *)
  | Pair of pair             (* Pairs. *)
  | Coproduct of coproduct   (* 2-ary coproducts *)
  | Bv of bv                 (* Bitvector function symbols. *)
  | Pp of pprod              (* Power products. *)
  | Fun of apply             (* Lambda abstraction and application *)
  | Arrays of arrays         (* Theory of arrays. *)


module Arith : sig
  val d_sym : t -> arith
  val num : Mpa.Q.t -> t
  val multq : Mpa.Q.t -> t
  val add : t
 end 

module Pair : sig
  val cons : t
  val car : t
  val cdr : t
end


module Coproduct : sig
  val inl : t
  val inr : t
  val outl : t
  val outr : t
end

module Pprod : sig
  val mult : t
  val expt : int -> t
  val is_expt : t -> bool
  val d_expt : t -> int
end

module Bv: sig
  val const : Bitv.t -> t
  val conc : int -> int -> t
  val sub : int -> int -> int -> t
  val width : bv -> int
end

module Array : sig
  val create : t
  val select : t
  val update : t
end

module Fun : sig

  val apply : Dom.t option -> t
    (** Family of function symbols for representing function application. *)

  val abs : t
    (** Function symbol for representing functional abstraction. *)
end




val eq : t -> t -> bool
  (** Equality test *)


val cmp : t -> t -> int
  (** Comparison. *)

val hash : t -> int
  (** nonnegative hash value *)


val to_string : t -> string
  (** Pretty printing to string *)

val pp : 'a Pretty.printer -> (t * 'a list) Pretty.printer
  (** Pretty-printing applications of symbols to an argument list. *)


module Hash :  (Hashtbl.S with type key = t)
  (** Hash table with symbols as keys. *)

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
  - tuples
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

type product = 
  | Tuple
  | Proj of int * int
      (** Function symbols for the theory of tuples are
	- [Tuple] for tupling
	- [Proj(i,n)] for projecting the [i]-th component in an [n]-tuple. *)

type coproduct = InL | InR | OutL | OutR
    (** Function symbols for the theory of tuples are
      - [InL] for left-injection,
      - [InR] for right-injection,
      - [OutR] for right-unpacking,
      - [OutL] for left-unpacking. *)


type bv =
  | Const of Bitv.t
  | Conc of int * int
  | Sub of int * int * int
  | Bitwise of int
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
  | Select 
  | Update
      (** Function symbols of the theory of arrays
	- [Select] for array lookup
	- [Update] for array update. *)

type bvarith = 
  | Unsigned
      (** Function symbols of the theory of arithmetic interpretations of bv
	- [Unsigned] for the unsigned interpretation *)

type t = 
  | Uninterp of Name.t       (* Uninterpreted function symbols. *)
  | Arith of arith           (* Linear arithmetic function symbols. *) 
  | Product of product       (* N-ary products *)
  | Coproduct of coproduct   (* 2-ary coproducts *)
  | Bv of bv                 (* Bitvector function symbols. *)
  | Pp of pprod              (* Power products. *)
  | Fun of apply             (* Lambda abstraction and application *)
  | Arrays of arrays         (* Theory of arrays. *)
  | Bvarith of bvarith       (* Bitvector interpretations. *)


val eq : t -> t -> bool
  (** Equality test *)


val cmp : t -> t -> int
  (** Comparison. *)

val hash : t -> int
  (** nonnegative hash value *)


val pp : Format.formatter -> t -> unit
  (** Pretty printing *)


val width : t -> int option
  (** Width of a bitvector symbol. *)


(** {6 Miscellaneous symbols} *)

val tuple : t
val car : t
val cdr : t

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


type t
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


(** {6 Uninterpreted Function Symbols} *)

type uninterp = Name.t

module Uninterp : sig
  val get : t -> uninterp
  val uninterp : Name.t -> t
end


(** {6 Linear Arithmetic} *)

(** Function symbols for linear arithmetic are
  - [Num(q)] for a rational number [q],
  - [Add] for addition,
  - [Multq(q)] for multiplication by a rational [q]. *)
type arith = 
  | Num of Mpa.Q.t  
  | Add
  | Multq of Mpa.Q.t
     

module Arith : sig
  val get : t -> arith
  val num : Mpa.Q.t -> t
  val multq : Mpa.Q.t -> t
  val add : t
  val d_num : t -> Mpa.Q.t
  val d_multq : t -> Mpa.Q.t
 end 


(** {6 Products} *)

(** Function symbols for the theory of Pairs. *)
type pair =
  | Cons
  | Car
  | Cdr

module Pair : sig
  val get : t -> pair
  val cons : t
  val car : t
  val cdr : t
end


(** {6 Coproducts} *)


(** Function symbols for the theory of cotuples are
  - [InL] for left-injection,
  - [InR] for right-injection,
  - [OutR] for right-unpacking,
  - [OutL] for left-unpacking. *)
type coproduct = InL | InR | OutL | OutR

module Coproduct : sig
  val get : t -> coproduct
  val inl : t
  val inr : t
  val outl : t
  val outr : t
end


(** {6 Power products} *)


(** Function symbols of the theory of power products are
  - [Mult] for nonlinear multiplication,
  - [Expt(n)] for exponentiation with integer [n]. *)
type pprod = 
  | Mult
  | Expt of int

module Pprod : sig
  val get : t -> pprod
  val mult : t
  val expt : int -> t
  val is_expt : t -> bool
  val d_expt : t -> int
end


(** {6 Bitvectors} *)

(** Function symbols for the theory of bitvectors are
  - [Const(b)] for constructing constant bitvectors,
  - [Conc(n, m)] for concatenating bitvectors of length [n] and [m],
  - [Sub(i,j,n)] for extracting bits [i] through [j] in a bv of length [n],
  - [Bitwise(n)] for bitwise conditional on bitvectors of length [n]. *)
type bv =
  | Const of Bitv.t
  | Conc of int * int
  | Sub of int * int * int
     

module Bv: sig
  val get : t -> bv
  val const : Bitv.t -> t
  val conc : int -> int -> t
  val sub : int -> int -> int -> t
  val width : bv -> int
end


(** {6 Functional Arrays} *)

(** Function symbols of the theory of arrays
  - [Select] for array lookup
  - [Update] for array update. *)
type arrays = 
  | Create
  | Select 
  | Update
    
module Array : sig
  val get : t -> arrays
  val create : t
  val select : t
  val update : t
end


(** {6 Applicaton and Abstraction} *)

(** Function symbols of the theory of functions
  - [Apply(r)] of function application
  - [Abs] of function abstraction *)
type apply = 
  | Apply of Dom.t option
  | Abs

module Fun : sig

  val get : t -> apply

  val apply : Dom.t option -> t
    (** Family of function symbols for representing function application. *)

  val abs : t
    (** Function symbol for representing functional abstraction. *)

  val is_apply : t -> bool
  val is_abs : t -> bool
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

val theory_of : t -> Th.t

type sym = 
  | Uninterp of uninterp
  | Arith of arith
  | Pair of pair
  | Coproduct of coproduct
  | Bv of bv
  | Pp of pprod
  | Fun of apply
  | Arrays of arrays 

val get : t -> sym


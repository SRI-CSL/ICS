
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
 * 
 * Author: Harald Ruess
 *)

(** Module [Sym]: Interpreted and uninterpreted function symbols *)



(** {Interpreted symbols} *)

type arith = 
  | Num of Mpa.Q.t  
  | Add
  | Multq of Mpa.Q.t

type product = 
  | Tuple
  | Proj of int * int

type coproduct = InL | InR | OutL | OutR

type bv =
  | Const of Bitv.t
  | Conc of int * int
  | Sub of int * int * int
  | Bitwise of int

type pprod = 
  | Mult
  | Expt of int

type apply = 
  | Apply of Cnstrnt.t option
  | Abs

type arrays = 
  | Select 
  | Update

type bvarith = 
  | Unsigned


(** {Symbols} *)

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


val pp : Format.formatter -> t -> unit
(** Pretty printing *)



val width : t -> int option
(** Width of a bitvector symbol. *)


(** {Miscellaneous symbols} *)

val tuple : t
val car : t
val cdr : t

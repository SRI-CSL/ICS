
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

(*s Module [Sym]: Interpreted and uninterpreted function symbols *)


type arith = 
  | Num of Mpa.Q.t  
  | Add
  | Multq of Mpa.Q.t

type tuple = 
  | Product 
  | Proj of int * int

type bv =
  | Const of Bitv.t
  | Conc of int * int
  | Sub of int * int * int
  | Bitwise of int

type t = 
  | Uninterp of Name.t
  | Arith of arith
  | Tuple of tuple
  | Bv of bv


(*s Equality test *)

val eq : t -> t -> bool

(*s Comparison. *)

val cmp : t -> t -> int


(*s Pretty printing *)

val pp : Format.formatter -> t -> unit


(*s Width of a bitvector symbol. *)

val width : t -> int option


(*s Classification of function symbols. *)

type theories = 
  | U
  | T
  | BV
  | A

val theory_of : t -> theories

val name_of_theory : theories -> string

val theory_of_name : string -> theories

val interp_theory_of_name : string -> theories

(*s Some predefined function symbols. *)

val select : t
val update : t
val unsigned : t
val floor : t
val ceiling : t
val mult : t
val expt : t
val div : t
val sin : t
val cos : t




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


type linarith = 
  | Num of Mpa.Q.t  
  | Multq of Mpa.Q.t
  | Add

type tuple = 
  | Product 
  | Proj of int * int

type bv =
  | Const of Bitv.t
  | Conc of int * int
  | Sub of int * int * int
  | Bitwise of int

type interp = 
  | Arith of linarith
  | Tuple of tuple
  | Bv of bv

type internal =
  | Label of int
  | Slack of int * Number.t
  | FreshBv of int
  | FreshT of int

type uninterp = 
  | External of Name.t
  | Internal of internal

type sym = 
  | Uninterp of uninterp
  | Interp of interp

type t

(*s Equal. *)

val eq : t -> t -> bool


(*s Constructing and destructing a symbol. *)

val make : sym -> t

val destruct : t -> sym


(*s Constructing Symbols. *)

val mk_uninterp : Name.t -> t
val mk_internal : internal -> t
val mk_interp : interp -> t


(*s Arithmetic Symbols. *)

val mk_num : Mpa.Q.t -> t
val mk_multq : Mpa.Q.t -> t
val mk_add : t


(*s Symbols from theory of tuples. *)

val mk_tuple : t
val mk_proj : int -> int -> t


(*s Symbols from theory of bitvectors. *)

val mk_bv_const : Bitv.t -> t
val mk_bv_conc : int -> int -> t
val mk_bv_sub : int -> int -> int -> t
val mk_bv_bitwise : int -> t


(*s Builtin symbols. *)

val mk_expt : t
val mk_mult : t
val mk_sin : t
val mk_cos : t
val mk_unsigned : t
val mk_update : t
val mk_select : t
val mk_floor : t
val mk_ceiling : t

(*s Comparison. *)

val cmp : t -> t -> int

(*s Pretty printing *)

val pp : bool -> Format.formatter -> t -> unit


val is_arith : t -> bool
val is_tuple : t -> bool
val is_interp : t -> bool

val is_uninterp : t -> bool
val d_uninterp : t -> uninterp
val d_interp : t -> interp option

val is_internal : t -> bool

val is_interpreted_const : t -> bool

(*s Create a fresh label. *)

val mk_label : int -> t
val mk_fresh_label : unit -> t

val mk_slack : int -> Number.t -> t
val mk_fresh_slack : Number.t -> t

(*s Width of a bitvector symbol. *)

val width : t -> int option


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
 * Author: Jean-Christophe Filliatre, Harald Ruess
 i*)

(*s Multi-precision arithmetic. The purpose of this module is to
  abstract the necessary arithmetic functions of any multi-precision
  package such as Ocaml's bignums, GNU MP etc in order to switch
  easily between packages. *)

(*s Multi-precision integers. *)

module Z : sig

  type t

  val zero : t
  val one : t
  val two : t

  val add : t -> t -> t
  val sub : t -> t -> t
  val succ : t -> t
  val mult : t -> t -> t
  val divexact : t -> t -> t
  val expt : t -> int -> t

  val equal : t -> t -> bool
  val compare : t -> t -> int
  val lt : t -> t -> bool
  val le : t -> t -> bool
  val gt : t -> t -> bool
  val ge : t -> t -> bool

  val of_int : int -> t
  val gcd : t -> t -> t      (*s Greatest Common Divisor. *)
  val lcm : t -> t -> t      (*s Least Common Multiple. *)
  val pow : int -> int -> t

  val to_string : t -> string

  val pp : Format.formatter -> t -> unit
end

(*s Multi-precision rationals. *)

module Q : sig

  type t

  val negone : t
  val zero : t
  val one : t
  val two : t
  val is_zero : t -> bool
  val is_one : t -> bool
  val is_negone : t -> bool
  val of_int : int -> t
  val of_ints : int -> int -> t

  val add : t -> t -> t
  val sub : t -> t -> t
  val minus : t -> t
  val mult : t -> t -> t
  val div : t -> t -> t
  val inv : t -> t
  val expt : t -> int -> t

  val floor : t -> Z.t
  val ceil  : t -> Z.t

  val compare : t -> t -> int
  val equal : t -> t -> bool
  val lt : t -> t -> bool
  val le : t -> t -> bool
  val gt : t -> t -> bool
  val ge : t -> t -> bool

  type cmp = Equal | Greater | Less

  val cmp : t -> t -> cmp

  val sign : t -> Sign.t

  val denominator : t -> Z.t
  val numerator : t -> Z.t
      
  val is_integer : t -> bool
  val to_z : t -> Z.t
  val of_z : Z.t -> t
      
  val hash : t -> int

  val to_string : t -> string
  val of_string : string -> t

  val pp : Format.formatter -> t -> unit
end







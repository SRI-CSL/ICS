
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

(** Multi-precision arithmetic.

  @author Jean-Christophe Filliatre
  @author Harald Ruess 
*)

(** The purpose of this module is to
  abstract the necessary arithmetic functions of any multi-precision
  package such as Ocaml's bignums, GNU MP etc in order to switch
  more easily between different multi-precision arithmetic packages.  *)


(** {6 Integers} *)

module Z : sig

  type t
    (** Abstract type of integers.*)

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
  val to_int : t -> int

  val gcd : t -> t -> t      (* Greatest Common Divisor. *)
  val lcm : t -> t -> t      (* Least Common Multiple. *)
  val pow : int -> int -> t

  val to_string : t -> string

  val pp : Format.formatter -> t -> unit
end
(** Multi-precision integers. *)

(** {6 Rationals.} *)

module Q : sig

  type t
    (** Abstract type of rationals. *)

  val negone : t
  val zero : t
  val one : t
  val two : t
  val is_zero : t -> bool
  val is_one : t -> bool
  val is_negone : t -> bool
  val is_pos : t -> bool
  val is_neg : t -> bool
  val is_nonneg : t -> bool
  val is_nonpos : t -> bool
  val of_int : int -> t
  val of_ints : int -> int -> t

  val min : t -> t -> t
  val max : t -> t -> t

  val add : t -> t -> t
  val sub : t -> t -> t
  val minus : t -> t
  val mult : t -> t -> t
  val div : t -> t -> t
  val inv : t -> t
  val expt : t -> int -> t

  val abs : t -> t

  val floor : t -> Z.t
  val ceil  : t -> Z.t

  val frac : t -> t    (* [q = floor(q) + frac(q)] *)
  val def : t -> t     (* [q = ceil(q) - def(q)] *)

  val compare : t -> t -> int
  val equal : t -> t -> bool
  val lt : t -> t -> bool
  val le : t -> t -> bool
  val gt : t -> t -> bool
  val ge : t -> t -> bool

  type cmp = Equal | Greater | Less

  val cmp : t -> t -> cmp

  val denominator : t -> Z.t
  val numerator : t -> Z.t
      
  val is_integer : t -> bool
  val to_z : t -> Z.t
  val of_z : Z.t -> t
      
  val hash : t -> int

  val to_string : t -> string

  exception ZeroDivision

  val of_string : string -> t

  val pp : Format.formatter -> t -> unit

  module Hash :  (Hashtbl.S with type key = t)

end
(** Multi-precision rationals. *)







(*i
 * ICS - Integrated Canonizer and Solver
 * Copyright (C) 2001-2004 SRI International
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the ICS license as published at www.icansolve.com
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * ICS License for more details.
 i*)

(*s Multi-precision arithmetic. The purpose of this module is to
  abstract the necessary arithmetic functions of any multi-precision
  package such as Ocaml's bignums, GNU MP etc in order to switch
  easily between packages. 
*)

(*s Multi-precision integers. *)

module Z : sig

  type t

  val mult : t -> t -> t
  val divexact : t -> t -> t

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

  val pp : Format.formatter -> t -> unit
end

(*s Multi-precision rationals. *)

module Q : sig

  type t

  val zero : t
  val one : t
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

  val denominator : t -> Z.t
      
  val is_integer : t -> bool
  val to_z : t -> Z.t
  val of_z : Z.t -> t
      
  val hash : t -> int

  val to_string : t -> string
  val of_string : string -> t

  val pp : Format.formatter -> t -> unit
end







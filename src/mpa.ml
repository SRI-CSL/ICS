
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

(*s Multi-precision arithmetic. *)

(*i module GMP = struct i*)

open Gmp

module Z = struct

  type t = Z.t

  let of_int = Z.from_int
  let mult = Z.mul
  let divexact = Z.divexact
  let gcd = Z.gcd
  let lcm a b = Z.divexact (Z.mul a b) (Z.gcd a b)
  let pow = Z.pow_ui_ui 
		 
  let compare = Z.cmp
  let equal x y = Z.cmp x y == 0
  let lt x y = Z.cmp x y < 0
  let le x y = Z.cmp x y <= 0
  let gt x y = Z.cmp x y > 0
  let ge x y = Z.cmp x y >= 0
		  
  let to_string z = Z.string_from z 10
 
  let pp fmt x = Format.fprintf fmt "%s" (Z.string_from x 10)
end

module Q = struct
  
  type t = Q.t

  let zero = Q.from_ints 0 1
  let one = Q.from_ints 1 1
  let of_int n = Q.from_ints n 1
  let of_ints = Q.from_ints

  let add = Q.add
  let sub = Q.sub
  let minus = Q.neg
  let mult = Q.mul
  let div = Q.div
  let inv = Q.inv

  let floor x = Gmp.Z.fdiv_q (Q.get_num x) (Q.get_den x)
  let ceil x  = Gmp.Z.cdiv_q (Q.get_num x) (Q.get_den x)

  let compare = Q.cmp
  let equal = Q.equal
  let is_zero x = Q.equal zero x
  let is_one x = Q.equal one x
  let is_negone x = Q.equal (Q.neg one) x
  let lt x y = Q.cmp x y < 0
  let le x y = Q.cmp x y <= 0
  let gt x y = Q.cmp x y > 0
  let ge x y = Q.cmp x y >= 0

  type cmp = Equal | Greater | Less

  let cmp x y =
    let b = compare x y in
    if b == 0 then Equal else if b > 0 then Greater else Less

  let denominator = Q.get_den
  let numerator = Q.get_num

  let is_integer q = (Gmp.Z.cmp_si (Q.get_den q) 1) = 0
  let to_z = Q.get_num
  let of_z = Q.from_z

  let hash = Hashtbl.hash

  let to_string q = 
    let d = Q.get_den q in
    if Gmp.Z.cmp_si d 1 == 0 then
      Gmp.Z.string_from (Q.get_num q) 10
    else
      (Gmp.Z.string_from (Q.get_num q) 10) ^ "/" ^ (Gmp.Z.string_from d 10)

  let of_string s =
    try
      let k = String.index s '/' in
      let l = String.length s in
      let n = Gmp.Z.from_string (String.sub s 0 k) 10 in
      let d = Gmp.Z.from_string (String.sub s (succ k) (l - k - 1)) 10 in
      Q.from_zs n d
    with Not_found ->
      Q.from_z (Gmp.Z.from_string s 10)

  let pp fmt x = Format.fprintf fmt "%s" (to_string x)

end

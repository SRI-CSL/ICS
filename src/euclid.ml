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

(** Euclidean Solver *)

module type Rat =
sig
  type q
  val eq : q -> q -> bool
  val ( + ) : q -> q -> q
  val zero : q
  val inv : q -> q
  val ( * ) : q -> q -> q
  val one : q
  val ( / ) : q -> q -> q
  val floor : q -> q
  val is_int : q -> bool
end

module type S =
sig 
  type t
  val euclid : t -> t -> t * t * t
  val solve : t list -> t -> (t * t list) option
end

module Make(R: Rat) = struct

  type t = R.q
  
  open R

  let ( - ) a b = a + inv(b)

  (** Given two rational numbers [a0], [b0], [euclid a0 b0] finds
    integers [x0], [y0], [(a0, b0)] satisfying
                 [a0 * x0 + b0 * y0 = (a0, b0)],
    where [(a0, b0)] denotes the greatest common divisor of [a0], [b0].
    For example,  [euclid 1547 560] equals [(7, 21, -58)]
    
    The value of [(a, b)] is unchanged in the loop in [euclid], since
    [(a, b) = (a - (a/b)*b, b)]; thus, using [(a, 0) = a],
    the first result of [euclid] computes [(a0, b0)]. Other
    invariants are:
        [c * a + e * b = a]
        [d * a + f * b = b]
    Now it is obvious that [a * x0 + b * y0 = (a, b)]. *)	    
  let euclid a0 b0 =
    let rec loop k a b c d e f =
      if eq zero a & not(eq zero b) then (b, d, f)
      else if eq zero b & not(eq zero a) then (a, c, e)
      else if (k mod 2 = 0) & not(eq zero b) then
	let u = floor(a / b) in
	loop (succ k) (a - u * b) b
	              (c - u * d) d
	              (e - u * f) f
      else if (k mod 2 = 1) & not(eq zero a) then
	let v = floor(b / a) in
	loop (succ k) a (b - v * a)
	              c (d - v * c)
	              e (f - v * e)
      else assert false
    in
    loop 0 a0 b0
      one  zero
      zero one

  (** Solving a linear diophantine equation with nonzero, rational coefficients
    [ci], for [i = 1,...,n] with [n >= 1]:
         [c0*x_0 + \ldots c_n * xn = b] (1)
    The algorithm proceeds by recursion on [n]. The case [n = 1] is
    trivial. Let [n >= 2]. Find, with the Euclidean algorithm
    [c'] and integers [d], [e] satisfying
    [c' = (c0, c1) = c0 * d + c1 * e]
    Next solve the linear diophantine equation (in [n] variables)
         [c'*x + c2 * x2 + ... + cn * xn = b] (2)
    If equation has no integral solution,
    then neither has.
    Otherwise, if [x,x2,...,xn] is an integral solution
    of (2), then [d*x, e*x,x2,...,xn] gives an integral solution of (1). *)
  let solve cl b =
    let rec loop = function
      | [] -> assert false
      | [c0] -> (c0, [b / c0])
      | c0 :: c1 :: l ->
	  let (d, e1, e2) = euclid c0 c1 in
	  match loop (d :: l) with
	    | e, x :: xs ->
		(e, (e1 * x) :: (e2 * x) :: xs)
	    | _ -> assert false
    in
    let (d,xs) = loop cl in
    if is_int (b / d) then Some(d,xs) else None
	
end









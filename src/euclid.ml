
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

module type Rat =
sig
  type q
  val eq : q -> q -> bool
  val ( + ) : q -> q -> q
  val inv : q -> q
  val zero : q
  val ( * ) : q -> q -> q
  val one : q
  val ( / ) : q -> q -> q
  val floor : q -> q
  val is_int : q -> bool
end

module Make(R: Rat) = struct
  
  open R

  let ( - ) a b = a + inv(b)

(*s Given two rational numbers $a$, $b$, [euclid] finds
  integers $x_0$, $y_0$ satisfying
  $$a * x_0 + b * y_0 = (a, b)\mbox{,}$$
  where $(a, b)$ denotes the greatest common divisor of $a$,$b$.
  
  e.g. $[euclid]~1547~560~=~(7, 21, -58)$
  
  The value of $(a, b)$ is unchanged in the loop in [euclid], since
  $(a, b) = (a - (a/b)*b, b)$; thus, using $(a, 0) = a$,
  the first result of [euclid] computes $(a_0, b_0)$. Other
  invariants are:
  \begin{eqnarray*}
  c * a_0 + e * b_0 & = & a \\
  d * a_0 + f * b_0 & = & b
  \end{eqnarray*}
  Now it is obvious that $a * x_0 + b * y_0 = (a, b)$.
*)
		      
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
    loop 0 a0   b0
      one  zero
      zero one

(*s
  Solving a linear diophantine equation with nonzero, rational coefficients
  $c_i$, for $i = 1,\ldots,n$ with $n \geq 1$.
  \begin{eqnarray}
  c_0*x_0 + \ldots c_n * x_n & = & b \label{eqn:lin.diophantine.orig}
  \end{eqnarray}
  The algorithm proceeds by recursion on $n$. The case $n = 1$ is
  trivial. Let $n \geq 2$. Find, with the Euclidean algorithm
  $c'$ and integers $d$, $e$ satisfying
  $$a' = (c_0, c_1) = c_0 * d + c_1 * e\mbox{.}$$
  Next solve the linear diophantine equation (in $n$ variables)
  \begin{eqnarray}
  c'*x + c_2 * x_2 + \ldots + c_n * x_n & = & b \label{eqn:lin.diophantine.rec}
  \end{eqnarray}
  If equation~\ref{eqn:lin.diophantine.rec} has no integral solution,
  then neither has \ref{eqn:lin.diophantine.orig}.
  Otherwise, if $x,x_2,\ldots, x_n$ is an integral solution
  of~\ref{eqn:lin.diophantine.rec}, then
  $d*x, e*x,x_2,\ldots,x_n$
  gives an integral solution of~\ref{eqn:lin.diophantine.orig}.
*)

  let solve cl b =
    let rec loop = function
      | [] -> assert false
      | [c0] -> (c0, [b / c0])
      | c0 :: c1 :: l ->
	  let (d,e1,e2) = euclid c0 c1 in
	  match loop (d :: l) with
	    | e, x :: xs ->
		(e, (e1 * x) :: (e2 * x) :: xs)
	    | _ -> assert false
    in
    let (d,xs) = loop cl in
    if is_int (b / d) then Some(d,xs) else None
	
end









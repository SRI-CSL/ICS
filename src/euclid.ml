(*i*)
open Mpa
(*i*)

type vector = Q.t list

(*s Given two rational numbers $a$, $b$, [euclid] finds
  integers $x_0$, $y_0$ satisfying
  $$a * x_0 + b * y_0 = (a, b)\mbox{,}$$
  where $(a, b)$ denotes the greatest common divisor of $a$,$b$.
  
  e.g. $[euclid]~1547~560~=~(7, 21, -58) 
  
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
    if Q.is_zero a & not(Q.is_zero b) then (b, d, f)
    else if Q.is_zero b & not(Q.is_zero a) then (a, c, e)
    else if (k mod 2 = 0) & not(Q.is_zero b) then
      let u = Q.of_z(Q.floor(Q.div a b)) in
      loop (succ k) (Q.sub a (Q.mult u b)) b
	            (Q.sub c (Q.mult u d)) d
	            (Q.sub e (Q.mult u f)) f
    else if (k mod 2 = 1) & not(Q.is_zero a) then
      let v = Q.of_z(Q.floor(Q.div b a)) in
      loop (succ k) a (Q.sub b (Q.mult v a))
	            c (Q.sub d (Q.mult v c))
	            e (Q.sub f (Q.mult v e))
    else assert false
  in
  loop 0 a0   b0
         Q.one  Q.zero
         Q.zero Q.one

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
		
let solve1 cl b =
  let rec loop = function
    | [] -> assert false
    | [c0] -> (c0, [Q.div b c0])
    | c0 :: c1 :: l ->
	let (d,e1,e2) = euclid c0 c1 in
	match loop (d :: l) with
	  | e, x :: xs ->
	      (e, (Q.mult e1 x) :: (Q.mult e2 x) :: xs)
	  | _ -> assert false
  in
  let (d,xs) = loop cl in
  if Q.is_integer (Q.div b d) then Some(d,xs) else None


(*s Compute the general solution of a linear Diophantine
    equation with coefficients [al], the gcd [d] of [al]
    and a particular solution [pl]. In the case of four
    coeffients, compute, for example,
     \begin{verbatim}
     (p0 p1 p2 p3) + k/d * (a1 -a0 0 0) + l/d * (0 a2 -a1 0) + m/d * (0 0 a3 -a2)
     \end{verbatim}
    Here, [k], [l], and [m] are fresh variables. Note that
    any basis of the vector space of solutions [xl]
    of the equation [al * xl = 0] would be appropriate.
  *)

module type Ops =
  sig
    type t
    val num : Q.t -> t
    val fresh : unit -> t
    val ( + ) : t -> t -> t
    val ( - ) : t -> t -> t
    val ( * ) : t -> t -> t
  end

module type S =
  sig
    type t
    val solve: Q.t list -> Q.t -> (Q.t * t list) option
  end

module Make(A: Ops) : (S with type t = A.t) =
  struct
    open A

    type t = A.t

    let solve cl b =
      match solve1 cl b with
	| None -> None 
	| Some (d,pl) ->
	    let rec loop al zl =
	      match al, zl with
		| [_], [_] -> zl
		| a0 :: ((a1 :: al'') as al'),  z0 :: z1 :: zl'' ->
		    let k = fresh () in
		    let e0 = z0 + k * (num (Q.div a1 d)) in
		    let e1 = z1 - k * (num (Q.div (Q.minus a0) d)) in
		    e0 :: loop al' (e1 :: zl'')
		| _ -> assert false
	    in
	    Some (d, loop cl (List.map num pl))
  end


(*i*)
open Hashcons
open Mpa
open Term
open Poly
open Format
(*i*)

(*s {\bf Normalization of inequalities.} In order to detect equalities and
    inconsistencies, the polynomials are stored in a normalized way, where
    the set of coefficients have a \emph{gcd} of 1. 
    Given an arbitrary polynomial $p$ with rational coefficients, i.e.
    $$p = \sum_{i=1}^nc_i\cdot x_i 
      \quad\mbox{with}\quad c_i=\frac{a_i}{b_i}$$
    the coefficients of $p$ are multiplied by the following ratio
    $$k = \frac{b_1\lor b_1\lor\cdots\lor b_n}
               {a_1\land a_1\land\cdots\land a_n}$$
    that is the \emph{lcm} of the denominators divided by the \emph{gcd}
    of the numerators. 
 
    We proceed in two steps. First we make all the coefficients integer,
    by replacing $\frac{a_i}{b_i}$ by $m\cdot\frac{a_i}{b_i}$ where $m
    = b_1\lor b_1\lor\cdots\lor b_n$ is computed with [poly_lcm]. 
    Then we make the \emph{gcd} of these new coefficients equal to 1, 
    where the \emph{gcd} is computed by [poly_gcd]. 
    Here we assume that [p] is non-null, and consequently the result
    of [poly_gcd] is always non-zero. *)

let le x y =
  let c = Term.new_var "c" Term.All in
  let slack = Arith.mult2 (c,c) in
  Equal.equal (Arith.add2 (x,slack)) y

let lt x y =
  Bool.conj (le x y) (Equal.diseq x y)

let ge x y = le y x

let gt x y = lt x y
	



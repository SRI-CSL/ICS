

(*s Given two rational numbers [p], [q],
    [euclid p q] finds integers [x], [y],
    [(p,q)] satisfying
          [p * x + q * y = (p,q)],
    where [(p,q)] denotes the greatest common divisor of [p],[q]\@.

    [solve \list{$c_1;\ldots;c_n$} $b$] $ yields a particular solution
    for a linear diophantine equation
       \begin{eqnarray}
       c_0*x_0 + \ldots c_n * x_n & = & b \label{eqn:lin.diophantine.orig}
      \end{eqnarray}
    with nonzero, rational coefficients $c_i$, for $i = 1,\ldots,n$ with $n \geq 1$.
    In case such a solution exists, it returns the gcd of the coefficients and
    a list of solutions $l_i$ for variable $x_i$\@.
  *)


module type Rat = sig
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
 

module Make(R: Rat):
sig
  val euclid : R.q -> R.q -> R.q * R.q * R.q
  val solve : R.q list -> R.q -> (R.q * R.q list) option
end

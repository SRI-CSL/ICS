
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

(*s Module [Euclid]: Euclidean solver for diophantine equations. *)


(*s The input signature of the functor [Euclid.Make]. *)

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
  
    (*s Given two rational numbers [p], [q], [euclid p q] finds integers [x], [y],
      [(p,q)] satisfying [p * x + q * y = (p,q)],
      where [(p,q)] denotes the greatest common divisor of [p],[q]\@. *)
  
  val euclid : R.q -> R.q -> R.q * R.q * R.q

     (*s [solve \list{$c_1;\ldots;c_n$} $b$] yields a particular solution
       for a linear diophantine equation
           $$c_0*x_0 + \ldots + c_n * x_n = b$$
       with nonzero, rational coefficients [$c_i$], for [$i = 1,\ldots,n$] with [$n \geq 1$].
       In case such a solution exists, it returns the gcd of the coefficients and
       a list of solutions [$l_i$] for variable [$x_i$]\@. *)
      
  val solve : R.q list -> R.q -> (R.q * R.q list) option
      
end













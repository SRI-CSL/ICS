(*
 * The contents of this file are subject to the ICS(TM) Community Research
 * License Version 2.0 (the ``License''); you may not use this file except in
 * compliance with the License. You may obtain a copy of the License at
 * http://www.icansolve.com/license.html.  Software distributed under the
 * License is distributed on an ``AS IS'' basis, WITHOUT WARRANTY OF ANY
 * KIND, either express or implied. See the License for the specific language
 * governing rights and limitations under the License.  The Licensed Software
 * is Copyright (c) SRI International 2003, 2004.  All rights reserved.
 * ``ICS'' is a trademark of SRI International, a California nonprofit public
 * benefit corporation.
 *)

(** Module [Euclid]: Euclidean solver for diophantine equations. 
  
  @author Harald Ruess
*)

(** Input signature of the functor {!Euclid.Make}
  May be instantiated with a structure isomorphic to the rationals. *)
module type RAT = sig
  type t
    (** Rationals. *)

  val eq : t -> t -> bool
    (** Equality. *)

  val pp : Format.formatter -> t -> unit
    (** Print a rational. *)

  val ( + ) : t -> t -> t
    (** Addition. *)

  val zero : t
    (** Neutral element of addition. *)

  val neg : t -> t
    (** Negation of a a rational. *)

  val ( * ) : t -> t -> t
    (** Multiplication. *)

  val one : t
    (* Neutral element of multiplication. *)

  val ( / ) : t -> t -> t
    (** Inverse of Multiplication. *)

  val floor : t -> t
    (** Floor function on rationals. *)
    
  val is_int : t -> bool
    (** Integer test of a rational. *)
end


(** Polynomials as used for the input signature 
  for {!Euclid.General}. *)
module type POLYNOMIAL = sig
  type q
    (** Coefficients of polynomial. *)

  type t
    (** Representation of polynomials. *)

  val pp : Format.formatter -> t -> unit
    (** Printing polynomials. *)

  val fresh : unit -> t
    (** Construct a {i fresh} polynomial 
      representing an indeterminate. The notion
    of freshness depends on the context of use. *)

  val of_q : q -> t
    (** Create a constraint polynomial. *)

  val add : t -> t -> t
    (** Add two polynomials. *)

  val multq : q -> t -> t
    (** Multiply a polynomial by a constant factor. *)
end


(** Particular solution of linear diophantine equations. *)
module Particular(Q: RAT) : sig
  val euclid : Q.t -> Q.t -> Q.t * Q.t * Q.t
    (** Given two rational numbers [p], [q], 
      [euclid p q] finds integers  [x], [y], [(p, q)] satisfying  
      [p * x + q * y = (p, q)], where [(p, q)] denotes the 
      greatest common divisor of [p], [q]. *)

  exception Unsolvable
    (** Raised by [solve]. *)
      
  val solve : Q.t list -> Q.t -> Q.t * Q.t list
    (** [solve [c1;...;cn] b] yields a particular solution
      for a linear diophantine equation [c0 * x0 + ... + cn * xn = b]
      with nonzero, rational coefficients [ci], for [i = 1,...,n] 
      with [n >= 1]. In case such a solution exists, it returns the gcd 
      of the coefficients and a list of solutions [li] for variable [xi]. *)  

end



(** General solution of linear diophantine equations. *)
module Solve(Q: RAT)(P: POLYNOMIAL with type q = Q.t) : sig
  exception Unsolvable

  val solve : Q.t list -> Q.t -> P.t list
end


(**/**)

module Test : sig
  val num_of_tests : int ref
  val max_num_of_variables : int ref
  val max_rat : int ref
  val run : unit -> unit
end

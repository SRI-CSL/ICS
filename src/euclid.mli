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

module type Rat = sig
  type q
    (** Rationals. *)

  val eq : q -> q -> bool
    (** Equality. *)

  val ( + ) : q -> q -> q
    (** Addition. *)

  val zero : q
    (** Neutral element of addition. *)

  val inv : q -> q
    (** Inverse of Addition. *)

  val ( * ) : q -> q -> q
    (** Multiplication. *)

  val one : q
    (* Neutral element of multiplication. *)

  val ( / ) : q -> q -> q
    (** Inverse of Multiplication. *)

  val floor : q -> q
    (** Floor function on rationals. *)
    
  val is_int : q -> bool
    (** Integer test of a rational. *)

end
  (** The argument signature of the functor {!Euclid.Make}
    May only be instantiated with a structure isomorphic to
    the rationals. *)


module type S =
  sig
  type t

  val euclid : t -> t -> t * t * t
    (** Given two rational numbers [p], [q], [euclid p q] finds integers 
      [x], [y], [(p, q)] satisfying  [p * x + q * y = (p, q)], 
      where [(p, q)] denotes the greatest common divisor of [p], [q]. *)
      
  val solve : t list -> t -> (t * t list) option
    (** [solve [c1;...;cn] b] yields a particular solution
      for a linear diophantine equation [c0 * x0 + ... + cn * xn = b]
      with nonzero, rational coefficients [ci], for [i = 1,...,n] 
      with [n >= 1]. In case such a solution exists, it returns the gcd 
      of the coefficients and a list of solutions [li] for variable [xi]. *)   
end
  (** Input signature of the functor {!Euclid.Make}. *)


module Make(R: Rat): S with type t = R.q
  (** Functor building an implementation of the {!Euclid.S}
    signature given an input structure {!Euclid.Rat} isomorphic
    to the rationals. *)













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

(** Associative-commutative simplification

  @author Harald Ruess
  @author N. Shankar
*)



(** Specification of a signature [th] with one binary,
  {i associative-commutative} (AC) function symbol [f]. *)
module type SIG = sig
  val th : Th.t
  val f : Sym.t
end 

(** Canonizer and iterators for AC theories. *)
module type TERM = sig

  val d_interp : Term.t -> Term.t * Term.t
    (** If [a] is of the form [b*c], then [d_interp a] returns (b, c). *)

  val is_interp : Term.t -> bool
    (** [is_interp a] holds iff [a] is of the form [b*c]. *)
    
  val make : Term.t -> Term.t -> Term.t
    (** For canonical [a], [b], [make a b] returns a canonical 
      term [c] with [AC |= c = a * b]. *)

  val iterate : Term.t -> int -> Term.t
    (** For [n>=1], [iterate a n] iterates [make a] [n-1] times.
      We sometimes write [a^n] to denote this term. *)

  val multiplicity: Term.t -> Term.t -> int
    (** [multiplicity x a] counts the number 
      of interpreted occurrences of [x] in [a]. *)
    
  val decompose : Term.t -> (Term.t * int) * Term.t option
    (** Decompose [x*x*...*x*y*....] into [(x, n)] with [n] the
      multiplicity of [x] in [a] and [y*...]. *)
 
  val fold : (Term.t -> int -> 'a -> 'a) -> Term.t -> 'a -> 'a
    (** If [a] is of the form [x1^m1*(x2^m2*...*xn^mn)], then [fold f a e]
      is [f x1 m1 (f x2 m2 (... (f xn mn e)...))]. *)

  val iter : (Term.t -> int -> unit) -> Term.t -> unit
    (** If [a] is of the form [x1^m1*(x2^m2*...*xn^mn)], then [iter f a]
      is [f x1 m1; f x2 m2; ...; f xn mn]. *)

  val sigma : Sym.t -> Term.t list -> Term.t
    (** If [f] equals [*], then [sigma f [a1; a2]] reduces
      to [make a1 a2]; otherwise the uninterpreted application [f(a1, a2)]
      is built. *)

  val map : (Term.t -> Term.t) -> Term.t -> Term.t
    (** [map f a] applies [f] at uninterpreted positions and 
      recanonizes the result. *)
end 

module Make(Sig: SIG): TERM
  (** Canonizer for a theory with one AC symbol, say [*]. 
    A term [a] is said to be in in {i canonical} form if it is
    of the form [x1 * (x2 * (x3 * ... (xn-1 * xn)...))] with [xi]
    either variables or terms with a top-level symbol disequal from [*].
    Furthermore, [xi << xj] for [i < j]. 
    
    For canonical terms [a], [b] it is the case that 
    {!Term.eq}[a b] iff [AC |= a = b], that is [a] and [b]
    are valid in the theory [AC]. *)

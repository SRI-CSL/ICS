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


(** Classification of function symbols
  
  @author Harald Ruess
*)

(** Function symbols are classified according to which {i theory} 
  they belong to.  We distinguish between three classes of
  {i equality theories}.
  - {i Uninterpreted} theory
  - {i Canonizable} theories have a unique normal forms.
  - {i Shostak} theories are {i canonizable} and {i solvable},
  that is, besides having unique normal forms, equalities can
  always be solved for variables. 

  Currently, ICS support the Shostak theories
  - [A] of linear arithmetic,
  - [P] of products,
  - [BV] of bitvectors,
  - [COP] of coproducts (direct sums),

  and the canonizable theories
  - [NL] of nonlinear arithmetic,
  - [APP] of strongly normalizable lambda terms, and
  - [ARR] of functional arrays

  The corresponding function symbols of these theories are 
  defined in module {!Sym.t}. 
*)


type t = 
  | U                               (* Uninterpreted *)
  | LA | P | BV | COP | SET | L     (* Shostak theories *)
  | NL | ARR                        (* Canonizable *)

val pp : Format.formatter -> t -> unit
  (** Pretty-printing theory identifier. *)

val to_string : t -> string
  (** [to_string i] returns a name for theory [i]. *)

val of_string : string -> t
  (** [of_string n] returns theory identifier [i]
    if [to_string i] equals to [n]; otherwise the 
    result is undefined. *)

val fold : (t -> 'a -> 'a) -> 'a -> 'a
  (** [fold f e] applies [f i] to each theory [i] 
    and accumulates the result, starting with [e],
    in an unspecified order. *)
  
val iter : (t -> unit) -> unit
  (** [fold f] applies [f i] for each theory [i] 
    in some unspecified order. *)

val for_all : (t -> bool) -> bool
  (** [forall p] holds if [p i] holds for all theories [i]. *)

module Map : (Map.S with type key = t)

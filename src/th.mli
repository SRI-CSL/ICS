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


(** Classification of function symbols
  
  @author Harald Ruess

  Function symbols are classified according to which theory they
  belong to.
 *)


type t

val eq : t -> t -> bool

val u : t          (** Theory of uninterpreted function symbols. *)

val la : t         (** Linear arithmetic theory. *)

val p : t          (** Product theory. *)

val bv : t         (** Bitvector theory. *)

val cop : t        (** Theory of coproducts. *)

val pprod : t      (** Theory of power products. *)

val app : t        (** Theory of function abstraction and application. *)

val arr : t        (** Array theory. *)

val bvarith : t    (** Theory of bitvector interpretation(s). *)

val to_int : t -> int
  (** [to_int th] returns a unique nonnegative integer for theories [th]. *)

val of_int : int -> t
  (** [of_int i] returns the theory [th] if [to_int th] is [i]. 
    The result value is undefined otherwise. *)

val is_fully_uninterp : t -> bool
  (** [is_fully_uninterp th] is equivalent to [eq th u]. *)


val is_fully_interp : t -> bool
  (** [la], [p], [bv], [cop] are fully interpreted. *)

val to_string : t -> string
  (** [to_string th] returns a string associated with the theory [th]. *)

val of_string : string -> t
 (** [of_string s] returns [th] if [to_string th] is [s]; otherwise
   [Invalid_argument] is raised. *)

val pp : t Pretty.printer
  (** Pretty-printing of a theory [th] using {!Th.to_string}[(th)]
    as the print name for [th]. *)
  
val of_sym : Sym.t -> t
  (** Classification of function symbols. *)


val map : t -> (Term.t -> Term.t) -> Term.t -> Term.t
  (** Theory-specific map function. *)

val sigma : Sym.t -> Term.t list -> Term.t
  (** Theory-specific sigmatizer. *)

val solve : t -> Fact.equal -> Fact.equal list
  (** Theory-specific solvers *)


(** {6 Arrays of index theory.} *)


module Array : sig

  type 'a arr

  val create: 'a -> 'a arr

  val copy : 'a arr -> 'a arr

  val get : 'a arr -> t -> 'a
 
  val set : 'a arr -> t -> 'a -> unit

  val reset : 'a arr -> 'a -> unit

  val iter : (t -> 'a -> unit) -> 'a arr -> unit

  val for_all : ('a -> bool) -> 'a arr -> bool

  val for_all2 : ('a ->'b -> bool) -> 'a arr -> 'b arr -> bool

end

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

(** Justifications

  @author Harald Ruess

  Every atom [atm] may have an associated {i justification} [rho]; in this
  case we also write [rho |- atm]. There are three different kinds of
  justifications
  - [All] justifies every
  - A set of {i dependencies} [{atm1, ...,atmn}]
  - A proof skeleton (disabled in ICS 2.0)

  Depending on the current proof mode, as obtained with {!Jst.Mode.get},
  justifications of these kinds are generated.
*)

(** Proof Mode *)
module Mode : sig

  type t = No | Dep (* | Yes *)

  val of_string : string -> t
  val to_string : t -> string

  val is_none : unit -> bool

  val get : unit -> t
  val set : t -> unit

end


type t
   
val pp : t Pretty.printer

val axioms_of: t -> Atom.Set.t

exception Inconsistent of t

val axiom : Atom.t -> t

val dep0 : t

val dep1 : t -> t

val dep2 : t -> t -> t

val dep3 : t -> t -> t -> t

val dep4 : t -> t -> t -> t -> t

val dep5 : t -> t -> t -> t -> t -> t

val dep : t list -> t

type jst = t
    (** Nickname. *)

(** Justifying Relations *)
module Three : sig

  type t =
    | Yes of jst
    | No of jst
    | X

  val to_three : jst list ref -> ('a -> 'b -> t) -> 'a -> 'b -> Three.t
    (** [to_three fcts p a[ accumulate facts in the result of [p a]
        in global variable [fcts] and returns a corresponding result of type
        {!Three.t}. *)

  val of_three : ('a -> Three.t) -> 'a -> t
end


(** Equality Transformers *)
module Eqtrans : sig
  type t = Term.t -> Term.t * jst

  val id : t
    (** [id a] returns [(a, rho)] such that [rho |- a = a]. *)

  val compose : t -> t -> t
    (** If [g a = (b, rho)] and [f b = (c, tau)] with [rho |- a = b]
      and [tau |- b = c], then [compose f g a] returns [(c, sigma)]
      with [sigma |- a = c]. *)

  val compose3 : t -> t -> t -> t
    (** [compose3 f g h] is defined as [compose f (compose g h)]. *)

  val totalize : t -> t

  val compose_partial1 : t -> t -> t
    (** [compose_partial1 f g a] behaves like [compose f g a] when [f] 
      does not throw an exception. In this case, the result is [g a]. *)

  val replace : Term.map -> t -> t

  val apply : Term.apply -> Atom.Equal.t * jst -> t

  val pointwise : t -> Term.t list -> Term.t list * jst

  val mapargs : (Sym.t -> Term.t list -> Term.t * jst) -> (Sym.t -> t) -> t
    (** [mapargs app f a] maps [f op] over the arguments [al] of
       an application [a] of the form [op(al)]. If [a] is not
       an application, [Not_found] is raised. *)

  val trace : Trace.level -> string -> t -> t
end


module Pred : sig
  type t = Term.t -> jst option
  val disj : t -> t -> t
  val apply : Eqtrans.t -> t -> t
  val trace : Trace.level -> string -> t -> t
end


module Pred2 : sig
  type t = Term.t -> Term.t -> jst option
  val apply : Eqtrans.t -> t -> t
  val trace : Trace.level -> string -> t -> t
end


module Rel1 : sig
  type t = Term.t -> Three.t
  val apply : Eqtrans.t -> t -> t
  val orelse : t -> t -> t
  val yes_or_no : Pred.t -> Pred.t -> t
  val trace : Trace.level -> string -> t -> t
end

module Rel2 : sig
  type t = Term.t -> Term.t -> Three.t
  val apply : Eqtrans.t -> t -> t
  val orelse : t -> t -> t
  val yes : Pred2.t -> t
  val yes_or_no : Pred2.t -> Pred2.t -> t
  val trace : Trace.level -> string -> t -> t
end





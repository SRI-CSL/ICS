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

(** Theory specifications

  @author Harald Ruess
*)

type t = {
  th : Theory.t;
  signature : Funsym.Set.t;
  rewrites : Axioms.Rewrite.t list;
  chains : Axioms.Chain.t list;
}

val pp : Format.formatter -> t -> unit

val make : Theory.t -> Funsym.Set.t -> Axioms.Rewrite.t list -> Axioms.Chain.t list -> t

(** A {i theory specification} consists of a signature
  of function symbols and a set of axioms on this signature. *)
module type SPEC = sig
  val th : Theory.t
  val signature : Funsym.Set.t
  module Axs : Axioms.AXIOMS
end
  

module type S = sig
  val th : Theory.t
 (* module Sig : Funsym.SIG *)
  val map : (Term.t -> Term.t) -> Term.t -> Term.t
  val can : Term.interp
  val is_diseq : Term.t -> Term.t -> bool
  module Infsys : Can.INFSYS
end

module Make(T: SPEC): S
  (** Compiling a theory specification into an inference system.*)

module Register(S: S) : sig end
  (** Registration of function symbols, term operators, and
    inference system for the specified theory. *)

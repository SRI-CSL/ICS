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

(** Equality over uninterpreted function symbols.

  @author Harald Ruess
  @author N. Shankar
*)

val theory : Theory.t
  (** Theory of uninterpreted function symbols. These
    function symbols are all restricted to have arity zero
    or one (nary function symbols need to be encoded using
    the {!Product} theory). *)

val mk_app : Name.t -> Term.t -> Term.t
  (** [mk_app n a] constructs a unary application [f(a)] with [f]
    the function symbol with name [n] and theory [U.theory]. *)

val mk_const : Name.t -> Term.t
  (** [mk_const n] construcs an uninterpreted constant [f(x)] with
    constant symbol [f] of name [n] and theory [U.theory]. *)

val is_uninterp : Term.t -> bool
  (** [is_uninterp a] holds iff the theory associated with
    the top-level function symbol is [U.theory] and it has
    either zero or one arguments. *)

val is_flat : Term.t -> bool
  (** [is_flat a] holds iff term [a] an application of the form [f(x1, ..., xn)]
    with [f] an uninterpreted function symbol (see {!Sym.Uninterp}) 
    and all [xi] are variables. *)
  
val is_pure : Term.t -> bool
  (** [is_pure a] holds iff all function symbols in  term [a] 
    are uninterpreted. In particular, [is_pure x] holds also 
    for variables [x]. *)


(** A {i configuration} consists of a set of renamings [u = f()]
  or [u = f(x)]. Such a configuration is {i injective}
  in the sense that [u = v] whenever [u = a] and [v = a] in the 
  configuration. *)
module Config : sig

  type t

  val is_empty : t -> bool

  val pp: Format.formatter -> t -> unit

  val in_dom : t -> Term.t -> bool

  val in_cod : t -> Term.t -> bool 

  val apply : t -> Term.t -> Term.t * Judgement.equal
    (** [apply s u] is [a, e] with [e |- u = a] if [u = a] 
      with [a = f()] or [a = f(x)], [x] a variable, in the
      the configuration. *)

  val inv : t -> Term.t -> Term.t * Judgement.equal
    
  val model : t -> Term.Model.t

end 

(** Inference system. *)
module Infsys: sig

  val current : unit -> Config.t

  val initialize : Config.t -> unit

  val reset : unit -> unit

  val finalize : unit -> Config.t

  val can : Term.t -> Term.t * Judgement.equal

  val abstract : Term.t -> Judgement.atom -> unit

  val process_equal : Judgement.equal -> unit

  val process_diseq : Judgement.diseq -> unit

  val propagate_equal : Term.t -> unit
  
  val propagate_diseq : Judgement.diseq -> unit

  val normalize : unit -> unit

end







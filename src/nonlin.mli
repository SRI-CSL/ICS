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


(* Builtin simplifications

   @author Harald ruess
*)

val mk_mult : Term.t -> Term.t -> Term.t

val mk_multl : Term.t list -> Term.t

val mk_expt : int -> Term.t -> Term.t

val mk_div : Term.t -> Term.t -> Term.t

val mk_inv : Term.t -> Term.t

val map : Term.map

val apply : Term.Equal.t -> Term.t -> Term.t

(** {6 Cross multiplication} *)

val crossmultiply : Term.Equal.t -> Term.Equal.t

val crossmultiply_nonneg : Term.t -> Term.t


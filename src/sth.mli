
(*i
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
 * 
 * Author: Harald Ruess
 i*)

module type Sth = sig
  val map: (Term.t -> Term.t) -> Term.t -> Term.t
  val solve : Term.t * Term.t -> (Term.t * Term.t) list
end


module Make(S: Sth): sig

  type t

  val solutions : t -> Solution.t

  val apply : t -> Term.t -> Term.t
  val find : t -> Term.t -> Term.t
  val inv : t -> Term.t -> Term.t
  val use : t -> Term.t -> Term.Set.t

(*s Empty state. *)

  val empty : t

  val is_empty : t -> bool

(*s [extend s (a,b)] installs a new binding [a |-> b] into [s]. 
  It assumes that [a] is not yet in the domain of [s]. 
  Also, [a],[b] must be valid lhs and rhs, respectively. *)

  val extend : Term.t -> t -> Term.t * t

(*s [merge (a,b) s] installs an equality [a = b] into [s]. *)

  val close :  V.t * t * V.focus -> V.t * t * V.focus

(*s Instantiating domain variables with canonical representatives. *)

  val inst : V.t -> t -> t

end



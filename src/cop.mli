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

(** Inference system for coproducts

  @author Harald Ruess
 
  The theory {!Th.cop} of coproducts is defined in module {!Coproduct}.
*)


module E: Shostak.EQS
  (** Set of directed equalities [x = a] with [x] a variable,
    [a] a {!Th.cop}-pure term, and none of the right-hand side
    variables occurs in any of the left-hand sides. *)

module Infsys: (Infsys.IS with type e = E.t) 
  (** Inference system for the theory of coproducts. *)


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

(** Shostak inference system for the theory of products

  @author Harald Ruess
  @author N. Shankar
*)


module Infsys: (Infsys.EQ with type e = Solution.Set.t) 
  (** Inference system for the theory {!Th.p} of products 
    as described in module {!Product}.  This inference
    system is obtained as an instantiation of the generic
    Shostak inference system {!Shostak.Infsys} with a
    specification of the {i convex} product theory by
    means of the 
    - product canonizer {!Product.map}
    - product solver {!Product.solve}

    In particular, there is no branching for this theory,
    and the inference system is complete as the product
    solver itself is complete. *)

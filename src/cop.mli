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

(** Inference system for coproducts

  @author Harald Ruess
 *)


module Infsys: (Infsys.EQ with type e = Solution.Set.t) 
  (** Inference system for the theory {!Th.cop} of coproducts
    as defined in module {!Coproduct}.

    This inference system maintains a set of directed 
    equalities [x = a] with [x] a variable, [a] a {!Th.cop}-pure term, 
    and none of the right-hand side variables occurs in any of the left-hand sides. 

    This inference system is obtained as an instantiation of the generic
    Shostak inference system [Shostak.Infsys] with a specification of the 
    coproduct theory by means of the 
    - coproduct canonizer {!Coproduct.map}
    - coproduct solver {!Coproduct.solve} *)
  

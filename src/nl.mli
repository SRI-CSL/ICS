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

(** Nonlinear arithmetic.

  @author Harald Ruess

  A {i nonlinear arithmetic term} is an ordered sum-of-monomials built-up from 
  rational constants, nonlinear multiplication, and addition. Since, every 
  linear arithmetic term (see module {!Linarith}) is also a nonlinear arithmetic
  term we view the theory of nonlinear arithmetic as the extension of linear
  arithmetic with nonlinear multiplication.
  
  This module defines the theory [nl] of {i nonlinear multiplication}
  and provides constructors for building canonical nonlinear arithmetic
  terms, and an inference system for processing atoms over nonlinear terms.
*)


(** Inference system for nonlinear multiplication. 
  In addition to AC inferences, this inference system
  - deduces, for example, [x >= 0] if [x = y * z] and [y, z >= 0]
  - branches on [x = 0] or [a = 1] if [x = x * a]. *)

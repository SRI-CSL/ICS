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

(** Inference system for the theory of nonlinear multiplication. 

  @author Harald Ruess
  @author N. Shankar
*)

(** A nonlinear {i context} consists of equalities of the form [x = y*z] 
  with [x], [y], [z] variables.

  The following invariants are maintained.
  - Right-hand sides of context equalities [x = a] are kept in 
  canonical form.  That is, if the variable equality [y = z]
  has been merged using {!Nl.merge}, then the noncanonical [y]
  is not appearing on any right-hand side. 
  - Also, if [x = a] and [y = b] in a context, then the 
  variables [x] and [y] are different (that is, they are not {!Term.eq})
  - If [u = y * v] in a context, then [y] is always {i atomic} in the 
  sense that it is an {i original} variable from one of the arguments
  of {!Nl.process} or {!Nl.name}, whereas [u], [v] may be {i generated}
  variables.

  Forward chaining is used to keep contexts {i confluent}
  - [x' = y * v], [z = x*u], [x =v x'] ==> [z = y * v * u]
  with [=v] generated by the current variable partitioning.

  This module provides
  - functions for accessing nonlinear contexts,
  - a canonizer for nonlinear products,
  - procedure {!Nl.process} for propagating equalities over pure 
  nonlinear terms,
  - {!Nl.merge} for propagating variable equalities,
  - and {!Nl.propagate} for propagating linear equalities 
  into a nonlinear context.
*)


module E: Can.EQS
  (** Equality set for the inference system for nonlinear multiplication.
    As an invariance we maintain that equalities are of the
    form [x = y * z] with [x], [y], [z] variables. *)

module Infsys: (Infsys.IS with type e = E.t)
  (** Inference system for nonlinear multiplication as 
    an extension of the AC inference system {!Ac.Make}
    instantiated with the signature {!Pprod.Sig} of nonlinear
    multiplication.  

    In addition to AC inferences, this inference system
    - deduces, for example, [x >= 0] if [x = y * z] and [y, z >= 0]
    - branches on [x = 0] or [a = 1] if [x = x * a]. *)


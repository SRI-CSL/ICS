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

(** Definition of a specifications from abstract syntax.

  @author Harald Ruess
*)

(** Abstract syntax for theory definition. *)
module Ast : sig

  type t = {
    name : string;
    description : string;
    signature : Name.t list;
    axioms : axiom list
  }
   
  and axiom =
    | Rewrite of rewrite 
    | Chain of chain
	
  and rewrite = Name.t * atom list * term * term
      
  and chain = Name.t * atom list * atom
      
  and atom = 
    | Equal of term * term
    | Diseq of term * term
	
  and term =
    | Var of Name.t
    | App of Name.t * term list
    
end

module Make(A: sig val ast : Ast.t end): Spec.SPEC
  (** Construct a theory specification from abstract
    syntax tree description. This may fail with [Invalid_argument]
    if any of the semantic requirements on specifications
    are not validated. *)



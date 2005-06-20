(*
 * The contents of this file are subject to the ICS(TM) Community Research
 * License Version 2.2 (the ``License''); you may not use this file except in
 * compliance with the License. You may obtain a copy of the License at
 * http://www.icansolve.com/license.html.  Software distributed under the
 * License is distributed on an ``AS IS'' basis, WITHOUT WARRANTY OF ANY
 * KIND, either express or implied. See the License for the specific language
 * governing rights and limitations under the License.  The Licensed Software
 * is Copyright (c) SRI International 2003, 2004.  All rights reserved.
 * ``ICS'' is a trademark of SRI International, a California nonprofit public
 * benefit corporation.
 *)

(** Propositional formulas with equality.

 This module includes functionality for constructing {i binary decision diagrams}  (BDDs).

  @author Harald Ruess
*)

module type VAR = sig
  type t
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val hash : t -> int
  val pp : Format.formatter -> t -> unit
end


module type INFSYS = sig
  type var
  type t
  val empty : t
  val initialize : t -> unit
  val reset : unit -> unit
  val unchanged : unit -> bool
  val current : unit -> t
  module Valid : (Sets.S with type elt = var)
  module Unsat : (Sets.S with type elt = var)
  val valid : unit -> Valid.t
  val unsat : unit -> Unsat.t
  val isValid : var -> bool
  val isUnsat : var -> bool  
  val pp : Format.formatter -> unit
  exception Unsat
  val processValid : var -> unit
  val processUnsat : var -> unit
end

module Make(Var: VAR) =  struct
  type var = Var.t

  module Valid = Sets.Make(Var)
  module Unsat = Sets.Make(Var)

  type t = {
    mutable valid : Valid.t;
    mutable unsat : Unsat.t;
  }

  let empty = { 
    valid = Valid.empty();
    unsat = Unsat.empty()
  }
  
  let init = ref empty
   
  module Config = struct
    module Valid = Config.Set(Valid)
    module Unsat = Config.Set(Unsat)
  end

  let valid = Config.Valid.current
  let unsat = Config.Unsat.current

  let initialize s =
    init := s;
    Config.Valid.initialize s.valid;
    Config.Unsat.initialize s.unsat

  let reset () = 
    initialize empty
      
  let unchanged () = 
    Config.Valid.unchanged() &&
    Config.Unsat.unchanged()
      
  let current () = 
    if unchanged () then !init else {
      valid = Config.Valid.current();
      unsat = Config.Unsat.current()
    }

  let isValid = Config.Valid.mem 

  let isUnsat = Config.Unsat.mem

  let pp fmt = 
    Format.fprintf fmt "@[";
    Format.fprintf fmt "valid: "; Valid.pp fmt (valid());
    Format.fprintf fmt "unsat: "; Unsat.pp fmt (unsat());
    Format.fprintf fmt "@]@?"
   
  exception Unsat

  let processValid p = 
    if isValid p then () else
      begin
	Config.Valid.add p;
	if isUnsat p then raise Unsat
      end
	  
  let processUnsat p = 
    if isUnsat p then () else 
      begin 
	Config.Unsat.add p;
	if isValid p then raise Unsat
      end
end


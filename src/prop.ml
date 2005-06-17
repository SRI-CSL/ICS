(*
 * The contents of this file are subject to the ICS(TM) Community Research
 * License Version 2.1 (the ``License''); you may not use this file except in
 * compliance with the License. You may obtain a copy of the License at
 * http://www.icansolve.com/license.html.  Software distributed under the
 * License is distributed on an ``AS IS'' basis, WITHOUT WARRANTY OF ANY
 * KIND, either express or implied. See the License for the specific language
 * governing rights and limitations under the License.  The Licensed Software
 * is Copyright (c) SRI International 2003, 2004.  All rights reserved.
 * ``ICS'' is a trademark of SRI International, a California nonprofit public
 * benefit corporation.
 *)

module type PROPVAR = Type.ORDERED

module type PROP = sig
  type var
  type t
  val mk_true : t
  val mk_false : t
  val mk_posvar : var -> t
  val mk_negvar : var -> t
  val mk_conj : t -> t -> t
  val mk_disj : t -> t -> t
  val union : var -> var -> t -> t
  val separate : var -> var -> t -> t
  val cofactorPos : t -> var -> t
  val cofactorNeg : t -> var -> t
  val is_valid : t -> bool
  val is_unsat : t -> bool
  val occurs : var -> t -> bool
  val andElim : t -> var list * var list * t
end

module type INTERFACE = sig
  type var
  val valid : var -> unit
  val unsat : var -> unit
end

module type INFSYS = sig
  type var
  type t
  val empty : t
  val initialize : t -> unit
  val reset : unit -> unit
  val unchanged : unit -> bool
  val is_valid : unit -> bool
  val current : unit -> t
  exception Unsat
  val processConjoin : t -> unit
  val processUnion : var -> var -> unit
  val processSeparate : var -> var -> unit  
  val processSub : var -> var -> unit 
  val propagateValid : var -> unit
  val propagateUnsat : var -> unit
end

module Make
  (Var: PROPVAR)
  (Prop: PROP with type var = Var.t)
  (I: INTERFACE with type var = Var.t) = 
struct
  type var = Var.t

  type t = Prop.t

  let empty = Prop.mk_true

  let init = ref empty

  let curr = ref empty

  let initialize p =
    init := p; curr := p

  let reset () = initialize empty

  let unchanged () = 
    !init == !curr

  let is_valid () = Prop.is_valid !curr

  let current () = !curr

  exception Unsat

  let rec update p = 
    curr := p;
    close();
    if Prop.is_unsat !curr then raise Unsat

  and close() =
    let pl, nl, next = Prop.andElim !curr in
      if pl <> [] || nl <> [] then
	begin
	  curr := next;
	  List.iter I.valid pl;
	  List.iter I.unsat nl
	end

  let processConjoin p = 
    if Prop.is_valid p then () else
      update (Prop.mk_conj p !curr)

  let processUnion x y =
    update (Prop.union x y !curr)

  let processSeparate x y =
    update (Prop.separate x y !curr)

  let processSub x y =
    update (Prop.mk_conj (Prop.mk_disj (Prop.mk_negvar x) (Prop.mk_posvar y)) !curr)

  let propagateValid x = 
    if Prop.occurs x !curr then
      update (Prop.cofactorPos !curr x)

  let propagateUnsat x = 
    if Prop.occurs x !curr then
      update (Prop.cofactorNeg !curr x) 
end

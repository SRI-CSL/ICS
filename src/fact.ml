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

let rec pp_justification fmt j =
  if !print_justification then
    begin
      Jst.pp fmt j;
      Format.fprintf fmt " |- "
    end;
  Format.fprintf fmt "@?"
    
and print_justification = ref false



(** Sigma normal forms for individual theories. *)
let sigma f al =
  match Sym.get f with
    | Sym.Arith(op) ->  Arith.sigma op al
    | Sym.Product(op) -> Product.sigma op al
    | Sym.Bv(op) ->  Bitvector.sigma op al
    | Sym.Coproduct(op) -> Coproduct.sigma op al
    | Sym.Propset(op) -> Propset.sigma op al
    | Sym.Cl(op) -> Apply.sigma op al
    | Sym.Pp(op) -> Pprod.sigma op al
    | Sym.Uninterp _ -> Term.App.mk_app f al
    | Sym.Arrays(op) -> Funarr.sigma Term.is_equal op al

let instantiate (b, c, rho) a =
  let rec inst a =
    if Term.eq a c then b else
      try
	let (f, al) = Term.App.destruct a in
	let bl = Term.mapl inst al in
	  sigma f bl
      with
	  Not_found -> a
  in
  let a' = inst a in
    if Term.eq a' a then Jst.Eqtrans.id a else
      (a', rho)


(** {6 Equality facts} *)

module Equal = struct

  type t = Term.t * Term.t * Jst.t

  let lhs_of (a, _, _) = a 
  let rhs_of (_, b, _) = b

  let make a b rho = 
    if Term.cmp a b >= 0 then (a, b, rho) else (b, a, rho)

  let map2 (f, g) ((a, b, rho) as fct) = 
    let (a', alpha') = f a 
    and (b', beta') = g b in
      if a == a' && b == b' then fct else
	make a' b' (Jst.dep3 rho alpha' beta')

  let map f = map2 (f, f)
  let map_lhs f = map2 (f, Jst.Eqtrans.id)
  let map_rhs f = map2 (Jst.Eqtrans.id, f)

  let instantiate e =
    map (instantiate e)

  let pp fmt (a, b, rho) = 
    pp_justification fmt rho;
    Pretty.infix Term.pp "=" Term.pp  fmt (a, b)

  let both_sides p (a, b, _) = p a && p b
    
  let is_var = both_sides Term.is_var
  let is_pure i = both_sides (Term.is_pure i)

  let status (a, b, _) =
    match Term.status a, Term.status b with
      | Term.Variable, Term.Variable -> Term.Variable
      | (Term.Mixed _ as s1) , _ -> s1
      | _, (Term.Mixed _ as s2) -> s2
      | (Term.Pure(i) as s1), Term.Variable -> s1
      | Term.Variable, (Term.Pure(j) as s2) -> s2
      | Term.Pure(i), (Term.Pure(j) as s2) -> 
	  if i = j then s2 else Term.Mixed(i, a)

end


(** {6 Disequality facts} *)

module Diseq = struct

  type t = Term.t * Term.t * Jst.t

      
  let lhs_of (a, _, _) = a
  let rhs_of (_, b, _) = b
			
  let pp fmt (a, b, rho) =   
    pp_justification fmt rho;
    Pretty.infix Term.pp " <> " Term.pp fmt (a, b)
			
  let is_var (a, b, _) = 
    Term.is_var a && Term.is_var b

  let make a b rho = 
    if Term.cmp a b >= 0 then (a, b, rho) else (b, a, rho)

  let map f ((a, b, rho) as d) =  
    let (a', alpha') = f a 
    and (b', beta') = f b in
      if a == a' && b == b' then d else
	make a' b' (Jst.dep3 rho alpha' beta')

  let instantiate e =
    map (instantiate e)

  let both_sides p (a, b, _) = p a && p b

  let is_pure i = both_sides (Term.is_pure i)

  module Set = Set.Make(
    struct
      type t = Term.t * Term.t * Jst.t
      let compare (a1, b1, _) (a2, b2, _) = 
	let c1 = Term.compare a1 a2 in
	  if c1 <> 0 then c1 else Term.compare b1 b2
    end)

  let status (a, b, _) =
    match Term.status a, Term.status b with
      | (Term.Mixed _ as s1), _ -> s1
      | _, (Term.Mixed _ as s2) -> s2
      | Term.Variable, Term.Variable -> Term.Variable
      | Term.Pure(i), (Term.Pure(j) as s2) -> 
	  if i = j then s2 else Term.Mixed(i, a)
      | (Term.Pure(i) as s1), Term.Variable -> s1
      | Term.Variable, (Term.Pure(j) as s2) -> s2
	  
end
  

(** {6 Nonnegativity facts} *)

module Nonneg = struct

  type t = Term.t * Jst.t
      
  let term_of (a, _) = a
			
  let pp fmt (a, rho) =   
    pp_justification fmt rho;
    Pretty.post Term.pp fmt (a, ">=0")
			
  let make a rho = (a, rho)
			   
  let map f ((a, rho) as nn) =
    let (a', alpha') = f a in
      if a == a' then nn else
	make a' (Jst.dep2 rho alpha')

  let instantiate e =
    map (instantiate e)

  let is_var nn = 
    Term.is_var (term_of nn)

  let is_pure i nn =
    Term.is_pure i (term_of nn)

  let status (a, _) = Term.status a

end


(** {6 Positivity facts} *)

module Pos = struct

  type t = Term.t * Jst.t
      
  let term_of (a, _) = a
			
  let pp fmt (a, rho) =   
    pp_justification fmt rho;
    Pretty.post Term.pp fmt (a, ">0")

  let make a rho = (a, rho)
			   
  let map f ((a, rho) as p) =
    let (a', alpha') = f a in
      if a == a' then p else
	(a', Jst.dep2 rho alpha')

  let instantiate e =
    map (instantiate e)

  let is_var p = 
    Term.is_var (term_of p)

  let is_pure i p =
    Term.is_pure i (term_of p)

  let status (a, _) = Term.status a

end


(** {6 Facts} *)

type t = Atom.t * Jst.t

let pp fmt (a, rho) =
  pp_justification fmt rho;
  Atom.pp fmt a

let of_equal (a, b, rho) = (Atom.mk_equal (a, b), rho)
let of_diseq (a, b, rho) = (Atom.mk_diseq (a, b), rho)
  
let eq (a1, _) (a2, _) = Atom.equal a1 a2

let map f ((a, rho) as fct) =
  match Atom.atom_of a with
    | Atom.TT -> fct
    | Atom.FF -> fct
    | Atom.Equal(a, b) ->
	let a', tau = f a
	and b', sigma = f b in
	  if a == a' && b == b' then fct else
	    (Atom.mk_equal (a', b'), Jst.dep3 rho tau sigma)
    | Atom.Diseq(a, b) ->
	let a', tau = f a
	and b', sigma = f b in
	  if a == a' && b == b' then fct else
	    (Atom.mk_diseq (a', b'), Jst.dep3 rho tau sigma)
    | Atom.Nonneg(a) ->
	let a', tau = f a in
	  if a == a' then fct else
	    (Atom.mk_nonneg a', Jst.dep2 rho tau)
    | Atom.Pos(a) ->
	let a', tau = f a in
	  if a == a' then fct else
	    (Atom.mk_pos a', Jst.dep2 rho tau)

let replace e =
  map (instantiate e)

  
 
    


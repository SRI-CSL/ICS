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
  Trace.msg "foo34" "Fact.sigma" (f, al) (Sym.pp Term.pp);
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

let instantiate (e, rho) a =
  let (b, c) = Atom.Equal.destruct e in
  let rec inst a =
    if Term.eq a c then b else        (** ??? *)
      try
	let (f, al) = Term.App.destruct a in
	let bl = Term.mapl inst al in
	  Trace.msg "foo34" "Inst" (f, []) (Sym.pp Term.pp);
	  Trace.msg "foo34" "Inst" al (Pretty.list Term.pp);
	  sigma f bl
      with
	  Not_found -> a
  in
  let a' = inst a in
    if Term.eq a' a then Jst.Eqtrans.id a else
      (a', rho)

let instantiate (e, rho) =
  Jst.Eqtrans.trace "foo34" "Instantiate" (instantiate (e, rho))


(** {6 Equality facts} *)

module Equal = struct

  type t = Atom.Equal.t * Jst.t

  let lhs_of (e, _) = Atom.Equal.lhs e
  let rhs_of (e, _) = Atom.Equal.rhs e

  let destruct (e, rho) =
    let (a, b) = Atom.Equal.destruct e in
      (a, b, rho)

  let of_equal (e, rho) = (e, rho)

  let make (a, b, rho) = (Atom.Equal.make (a, b), rho)

  let map2 (f, g) ((e, rho) as fct) = 
    let (a, b) = Atom.Equal.destruct e in
    let (a', alpha') = f a 
    and (b', beta') = g b in
      if a == a' && b == b' then fct else
	(Atom.Equal.make (a', b'), Jst.dep3 rho alpha' beta')

  let map f = map2 (f, f)
  let map_lhs f = map2 (f, Jst.Eqtrans.id)
  let map_rhs f = map2 (Jst.Eqtrans.id, f)

  let instantiate e =
    map (instantiate e)

  let pp fmt (e, rho) = 
    pp_justification fmt rho;
    Atom.Equal.pp fmt e

  let both_sides p (e, _) = Atom.Equal.both_sides p e
  let is_var = both_sides Term.is_var
  let is_pure i = both_sides (Term.is_pure i)

  let status (e, _) = Atom.Equal.status e

  module Set = Set.Make(
    struct
      type t = Atom.Equal.t * Jst.t
      let compare (e1, _) (e2, _) = Atom.Equal.compare e1 e2
    end)

end


(** {6 Disequality facts} *)

module Diseq = struct

  type t = Atom.Diseq.t * Jst.t

  let destruct (d, rho) =
    let (a, b) = Atom.Diseq.destruct d in
      (a, b, rho)
      
  let lhs_of (d, _) = Atom.Diseq.lhs d
  let rhs_of (d, _) = Atom.Diseq.rhs d
			
  let pp fmt (d, rho) =   
    pp_justification fmt rho;
    Atom.Diseq.pp fmt d
			
  let is_var (d, _) = Atom.Diseq.is_var d
			
  let of_diseq (d, rho) = (d, rho)
  let make (a, b, rho) = (Atom.Diseq.make (a, b), rho)
			   
  let map f ((d, rho) as deq) =  
    let (a, b) = Atom.Diseq.destruct d in
    let (a', alpha') = f a 
    and (b', beta') = f b in
      if a == a' && b == b' then deq else
	(Atom.Diseq.make (a', b'), Jst.dep3 rho alpha' beta')


  let instantiate e =
    map (instantiate e)

  let both_sides p (d, _) = Atom.Diseq.both_sides p d

  let is_pure i = both_sides (Term.is_pure i)

  module Set = Set.Make(
    struct
      type t = Atom.Diseq.t * Jst.t
      let compare (d1, _) (d2, _) = Atom.Diseq.compare d1 d2
    end)


  let status (d, _) = Atom.Diseq.status d

end


(** {6 Facts} *)

type t = 
  | Equal of Equal.t
  | Diseq of Diseq.t

type fact = t  (* nickname *)

let pp fmt = function
  | Equal(e) -> Equal.pp fmt e
  | Diseq(d) -> Diseq.pp fmt d

let of_equal e = Equal(e)

let of_diseq(d) = Diseq(d)


(** {6 Input facts} *)

module E = Equal
module D = Diseq

module Input = struct

  type t = {
    mutable eqs : E.Set.t;
    mutable diseqs : D.Set.t 
  }

  let empty = {
    eqs = E.Set.empty;
    diseqs = D.Set.empty
  }

  let copy s = 
    if !Tools.destructive then
      {eqs = s.eqs; diseqs = s.diseqs}
    else 
      s

  let is_empty s = 
    E.Set.is_empty s.eqs &&
    D.Set.is_empty s.diseqs

  let eq i j =
    i.eqs == j.eqs &&
    i.diseqs == j.diseqs


  let add s = function
    | Equal(e) -> 
	let eqs' = E.Set.add e s.eqs in
	if !Tools.destructive then (s.eqs <- eqs'; s) else 
	  {s with eqs = eqs'}
    | Diseq(d) -> 
	let diseqs' =  D.Set.add d s.diseqs in
	  if !Tools.destructive then (s.diseqs <- diseqs'; s) else
	    {s with diseqs = diseqs'}


  let to_list s =
    let eqs = E.Set.fold (fun e acc -> of_equal e :: acc) s.eqs [] 
    and diseqs = D.Set.fold (fun d acc -> of_diseq d :: acc) s.diseqs [] in
      eqs @ diseqs
   
  let pp fmt s =
    let l = to_list s in
      if l = [] then
	Pretty.string fmt "{}"
      else 
	begin
	  Pretty.set pp fmt l;
	  Format.fprintf fmt "@?"
	end 
			  

  module Equal = struct

    let is_empty s = E.Set.is_empty s.eqs 

    let add s e = 
      let eqs' = E.Set.add e s.eqs in
	if !Tools.destructive then 
	  (s.eqs <- eqs'; s)
	else 
	  {s with eqs = eqs'}

    let remove s e = 
      let eqs' = E.Set.remove e s.eqs in
	if !Tools.destructive then
	  (s.eqs <- eqs'; s)
	else 
	  {s with eqs = eqs'}

    let choose s = 
      let e = E.Set.choose s.eqs in
	(e, remove s e)

    let to_list s =
      E.Set.elements s.eqs

    let fold f s = E.Set.fold f s.eqs

    let instantiate e s =
      let inst = E.instantiate e in
      E.Set.fold 
	(fun e acc ->
	   let e' = inst e in
	     if e == e' then acc else
	       E.Set.add e' (E.Set.remove e acc))
	s s

  end 

  module Diseq = struct

    let is_empty s = D.Set.is_empty s.diseqs
    
    let add s d = 
      let diseqs' = D.Set.add d s.diseqs in
	if !Tools.destructive then
	  (s.diseqs <- diseqs'; s)
	else 
	  {s with diseqs = diseqs'}
		    
    let remove s d = 
      let diseqs' = D.Set.remove d s.diseqs in
	if !Tools.destructive then
	  (s.diseqs <- diseqs'; s)
	else 
	  {s with diseqs = diseqs'}
      
    let choose s = 
      let d = D.Set.choose s.diseqs in
	(d, remove s d)

    let choose s = 
      Trace.call "foo45" "Diseq.choose" s pp;
      let (d', s') = choose s in
	Trace.exit "foo45" "Diseq.choose" (d', s') (Pretty.pair  D.pp pp);
	(d', s')
	
    let to_list s = 
      D.Set.elements s.diseqs

    let instantiate e s =
      let inst = D.instantiate e in
	D.Set.fold 
	  (fun d acc ->
	     let d' = inst d in
	       if d == d' then acc else
		 D.Set.add d' (D.Set.remove d acc))
	s s
	
    let fold f s = D.Set.fold f s.diseqs

  end 

  let instantiate e s =
    let eqs' = Equal.instantiate e s.eqs
    and diseqs' = Diseq.instantiate e s.diseqs in
      if !Tools.destructive then
	(s.eqs <- eqs'; s.diseqs <- diseqs'; s)
      else 
	{s with eqs = eqs'; diseqs = diseqs'}
    

  let is_empty s = 
    Equal.is_empty s &&
    Diseq.is_empty s



end 

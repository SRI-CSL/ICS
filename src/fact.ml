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

  let pp fmt (e, rho) = 
    pp_justification fmt rho;
    Atom.Equal.pp fmt e

  let both_sides p (e, _) = Atom.Equal.both_sides p e
  let is_var = both_sides Term.is_var
  let is_pure i = both_sides (Term.is_pure i)
  let is_diophantine  = both_sides Arith.is_diophantine

  let theory_of (e, _) =
    let (a, b) = Atom.Equal.destruct e in
      match a, b with
	| Term.Var _, Term.Var _ -> None
	| Term.Var _, Term.App(g, _, _) -> Th.inj (Sym.theory_of g)
	| Term.App(f, _, _), Term.Var _  -> Th.inj (Sym.theory_of f)
	| Term.App(f, _, _), Term.App _  -> Th.inj (Sym.theory_of f)


  let equiv f ((e, rho) as eqn) =
    let (a, b) = Atom.Equal.destruct e in
      try
	let (a', b') = f (a, b) in
	  if a == a' && b == b' then eqn else
	    let e' = Atom.Equal.make (a', b') in
	      (e', rho)
      with
	  Exc.Inconsistent -> 
	    raise(Jst.Inconsistent rho)
	    
  let equivn f (e, rho) =
    let (a, b) = Atom.Equal.destruct e in
      try
	let el = f (a, b) in
	let inj e = (Atom.Equal.make e, rho) in
	  List.map inj el
      with
	  Exc.Inconsistent -> 
	    raise(Jst.Inconsistent rho)

  let holds (e, rho) =
    match Atom.Equal.holds e with
      | Three.Yes -> Jst.Three.Yes(rho)
      | Three.No -> Jst.Three.No(rho)
      | Three.X -> Jst.Three.X
	          
    let trace lvl name = Trace.func lvl name pp pp

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
			
  let is_diophantine (d, _) = Atom.Diseq.is_diophantine d
  let is_var (d, _) = Atom.Diseq.is_var d
			
  let of_diseq (d, rho) = (d, rho)
  let make (a, b, rho) = (Atom.Diseq.make (a, b), rho)
			   
  let map f ((d, rho) as deq) =  
    let (a, b) = Atom.Diseq.destruct d in
    let (a', alpha') = f a 
    and (b', beta') = f b in
      if a == a' && b == b' then deq else
	(Atom.Diseq.make (a', b'), Jst.dep3 rho alpha' beta')

  let to_var term_to_var  ((d, rho) as deq) =
    let (a, b) = Atom.Diseq.destruct d in
      match a, b with
	| Term.Var _, Term.Var _ -> deq
	| Term.Var _, Term.App(g, _, _) ->
	    let j = Sym.theory_of g in
	    let (y, beta) = term_to_var j b in
	      (Atom.Diseq.make (a, y), Jst.dep2 rho beta)
	| Term.App(f, _, _), Term.Var _ ->
	    let i = Sym.theory_of f in
	    let (x, alpha) = term_to_var i a in
	      (Atom.Diseq.make (x, b), Jst.dep2 rho alpha)
	| Term.App(f, _, _), Term.App(g, _, _) ->
	    let i = Sym.theory_of f in
	    let j = Sym.theory_of g in
	    let (x, alpha) = term_to_var i a in
	    let (y, beta) = term_to_var j b in
	      (Atom.Diseq.make (x, y), Jst.dep3 rho alpha beta)
		
  let d_diophantine (d, rho) =  
    let (a, b) = Atom.Diseq.destruct d in
    let q = Arith.d_num b in
      (a, q, rho)

  module Set = Set.Make(
    struct
      type t = Atom.Diseq.t * Jst.t
      let compare (d1, _) (d2, _) = Atom.Diseq.compare d1 d2
    end)

end


(** {6 Nonnegativity facts} *)

module Nonneg = struct

  type t = Atom.Nonneg.t * Jst.t

  let pp fmt (nn, rho) =  pp_justification fmt rho; Atom.Nonneg.pp fmt nn
  let make (a, rho) = (Atom.Nonneg.make a, rho)
  let of_nonneg (nn, rho) = (nn, rho)
  let destruct (nn, rho) = (Atom.Nonneg.destruct nn, rho)
      
  let map f ((nn, rho) as nonneg) =
    let a = Atom.Nonneg.destruct nn in
    let (a', alpha') = f a in
      if a == a' then nonneg else
	let nn' = Atom.Nonneg.make a' in
	  (nn', Jst.dep2 rho alpha')
    
end


(** {6 Positivity constraints} *)

module Pos = struct

  type t = Atom.Pos.t * Jst.t
      
  let pp fmt (p, rho) =   pp_justification fmt rho; Atom.Pos.pp fmt p
  let destruct (p, rho) = (Atom.Pos.destruct p, rho)
  let of_pos (p, rho) = (p, rho)
  let make (a, rho) = (Atom.Pos.make a, rho)  
			
  let map f ((p, rho) as pos) =
    let a = Atom.Pos.destruct p in
    let (a', alpha') = f a in
      if a == a' then pos else
	(Atom.Pos.make a', Jst.dep2 rho alpha')
end


(** {6 Facts} *)

type t = Atom.t * Jst.t

let rec pp fmt (atm, j) =
  pp_justification fmt j;
  Atom.pp fmt atm;
  Format.fprintf fmt "@?"
    
let atom_of (a, _) = a
		       
let justification_of (_, j) = j
						
let mk_axiom atm = (atm, Jst.axiom atm)

let mk_holds atm = (atm, Jst.dep0)
      
let rec map (is_equal, is_nonneg, is_pos) f atm =
  match Atom.atom_of atm with
    | Atom.TT -> 
	mk_holds atm
    | Atom.FF -> 
	mk_holds atm
    | Atom.Equal(a, b) -> 
	let (a', alpha) = f a and (b', beta) = f b in
	  if a == a' && b == b' then mk_holds atm else
	    let rho = Jst.dep2 alpha beta in
	      (match is_equal a' b' with
		 | Jst.Three.Yes(tau) -> (Atom.mk_true, Jst.dep2 rho tau)
		 | Jst.Three.No(tau) -> (Atom.mk_false, Jst.dep2 rho tau)
		 | Jst.Three.X -> (Atom.mk_equal (a', b'), rho))
    | Atom.Diseq(a, b) ->
	let (a', alpha) = f a and (b', beta) = f b in
	  if a == a' && b == b' then mk_holds atm else
	    let rho = Jst.dep2 alpha beta in
	      (match is_equal a' b' with
		   | Jst.Three.No(tau) -> (Atom.mk_true, Jst.dep2 rho tau)
		   | Jst.Three.Yes(tau) -> (Atom.mk_false, Jst.dep2 rho tau)
		   | Jst.Three.X -> (Atom.mk_diseq (a', b'), rho))
    | Atom.Nonneg(a) -> 
	let (a', alpha) = f a in
	  if a == a' then mk_holds atm else
	    (match is_nonneg a' with
	       | Jst.Three.Yes(tau) -> (Atom.mk_true, Jst.dep2 alpha tau)
	       | Jst.Three.No(tau) -> (Atom.mk_false, Jst.dep2 alpha tau)
	       | Jst.Three.X -> (Atom.mk_nonneg a', alpha))
    | Atom.Pos(a) -> 
	let (a', alpha) = f a in
	  if a == a' then mk_holds atm else
	    (match is_pos a' with
	       | Jst.Three.Yes(tau) -> (Atom.mk_true, Jst.dep2 alpha tau)
	       | Jst.Three.No(tau) -> (Atom.mk_false, Jst.dep2 alpha tau)
	       | Jst.Three.X -> (Atom.mk_pos a', alpha))  

let map preds f = 
  Trace.func "foo" "Map" Atom.pp (Pretty.pair Atom.pp Jst.pp) (map preds f)


(** {6 Stacks} *)

  
module type STACK = sig
  type t
  val clear : unit -> unit
  val push : Th.t option -> t -> unit
  val pop : unit -> Th.t option * t
  val is_empty : unit -> bool
end

module type T = sig
  type t
  val pp : t Pretty.printer
end

module Stack(Arg: T) = struct 

  type t = Arg.t

  let stack = Stack.create ()

  let th_to_string = function
    | None -> "v"
    | Some(i) -> Th.to_string i

  let enabled = ref true

  let clear () = 
    if !enabled then
      Stack.clear stack
		   
  let push i e =
    if !enabled then
      Stack.push (i, e) stack
	
  let pop () =
    if !enabled then
      Stack.pop stack
    else 
      failwith "Fatal error: popping a disabled stack"
	
  let is_empty () = 
    Stack.is_empty stack
      
end
  
module Eqs = Stack(
  struct
    type t = Equal.t
    let pp = Equal.pp
  end)
  
module Diseqs = Stack(
  struct
    type t = Diseq.t
    let pp = Diseq.pp
  end)

module Nonnegs = Stack(
  struct
    type t = Nonneg.t
    let pp = Nonneg.pp
  end)
  
let with_disabled_stacks f a =
  try
    Eqs.enabled := false;
    Diseqs.enabled := false;
    Nonnegs.enabled := false;
    let b = f a in
      Eqs.enabled := true;
      Diseqs.enabled := true;
      Nonnegs.enabled := true;
      b
  with
      exc ->
	Eqs.enabled := true;
	Diseqs.enabled := true;  
	Nonnegs.enabled := true;
	raise exc
    

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

(** Abstract syntax tree *)
module Ast = struct

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


module Make(T: sig val ast: Ast.t end): Spec.SPEC = struct

  open T
  open Ast

  let th = Theory.create ast.name ast.description

  module Sig = struct

    let th = th

    type t = Name.t

    let inj =
      let table = Name.Hash.create 3 in
      let _ = Tools.add_at_reset (fun () -> Name.Hash.clear table) in
	fun op -> 
	  try
	    Name.Hash.find table op
	  with
	      Not_found -> 
		let f = Funsym.create th op in
		  Name.Hash.add table op f; f

    let out f = failwith "to do"
(*
      let i = Funsym.theory_of f in
	if Theory.eq i th then 
	  Funsym.get th f 
	else 
	  raise Not_found
*)
		    
    let eq = Name.eq

    let pp = Name.pp

  end 

  let rec mk_term = function
    | Var(x) -> 
	Axioms.Lterm.Var(x)
    | App(n, al) ->
	let f = Sig.inj n in
	let bl = List.map mk_term al in
	  Axioms.Lterm.App(f, bl)

  let mk_atom = function
    | Equal(a, b) ->
	Axioms.Atom.mk_equal (mk_term a) (mk_term b)
    | Diseq(a, b) ->
	Axioms.Atom.mk_diseq (mk_term a) (mk_term b)

  let mk_rewrite (id, hyps, pat, res) = 
    let hyps' = List.map mk_atom hyps
    and pat' = mk_term pat
    and res' = mk_term res in
      match pat' with
	| Axioms.Lterm.Var _ -> 
	    invalid_arg "Variable on lhs of rewrite"
	| Axioms.Lterm.App(f', al') ->
	    Axioms.Rewrite.make id hyps' (f', al') res'

  let mk_chain (id, hyps, conc) = 
    let hyps' = List.map mk_atom hyps
    and conc' = mk_atom conc in
      Axioms.Chain.make id hyps' conc'

  let mk_axioms = 
    let rec translate (rl, cl) = function
      | [] -> (rl, cl)
      | Rewrite(r) :: al -> 
	  translate (mk_rewrite r :: rl, cl) al
      | Chain(c) :: al -> 
	  translate (rl, mk_chain c :: cl) al
    in
      translate ([],[])

  module Axs = struct
    let rl, cl = mk_axioms ast.axioms
    let rewrites = rl
    let chains = cl
  end 
    
end

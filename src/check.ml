
(*i
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
 * 
 * Author: Harald Ruess
 i*)


(*s Traversing a propositional structure. [s] is the current 
 context, [tbl] is a table of positively and negatively implied
 atoms for states [s], [undecided] is the list
 of literals which have not yet been decided. In the beginning,
 [undecided] consists of all literals, positive or negative,
 occuring in the proposition. 

  let Satisfiable(ps) = process s x in
  let ptbl = tbl union {ps |-> ({ l in undecided_lits | ps |- l } as pfoo) union tbl(s) }
  let pundecide_lits = undecided_lits - { l | l in pfoo OR ~l in pfoo }  in
  let Satisfiable(ns) = process s ~x in
  let ntbl = tbl union {ns |-> ({ l in undecided_lits | ns |- l } as nfoo) union tbl(s) }
  let nundecided_lits = undecided_lits - { l | l in nfoo OR ~l in nfoo }   in
  let p = build ps ptbl pundecide_lits (pa,pb,pc) in
  let n = build ns ntbl nundecided_lits (na,nb,nc) in
  if p === n then p else make(Ite(x,p,n))

*)


type decided = { 
  pos : Atom.Set.t; 
  neg : Atom.Set.t;
  undec : Atom.Set.t 
}

let pp fmt dec =
  Format.fprintf fmt "@[[pos = ";
  Pretty.atoms fmt dec.pos;
  Format.fprintf fmt "; neg = ";
  Pretty.atoms fmt dec.neg;
  Format.fprintf fmt "; undec = ";
  Pretty.atoms fmt dec.undec;
  Format.fprintf fmt "]@]";

type status = Pos | Neg | Undec

let lookup x dec =
  if Atom.Set.mem x dec.pos then Pos
  else if Atom.Set.mem x dec.neg then Neg
  else Undec

let add s dec =
  Atom.Set.fold 
    (fun x acc ->
       match Can.atom s x with
	 | Atom.True ->
	     {acc with pos = Atom.Set.add x acc.pos; 
                       undec = Atom.Set.remove x acc.undec}
	 | Atom.False ->
	     {acc with neg = Atom.Set.add x acc.neg;
                       undec = Atom.Set.remove x acc.undec}
	 | _ ->
	     acc)
    dec.undec
    dec

let rec prop s b =
  let dec = 
    add s { pos = Atom.Set.empty; 
	    neg = Atom.Set.empty; 
	    undec = Prop.literals_of b}
  in
  build s dec b

and build s dec b =
  match Prop.destruct b with
    | Prop.True -> Prop.mk_tt
    | Prop.False -> Prop.mk_ff
    | Prop.Ite(x,p,n) ->
	match lookup x dec with
          | Pos -> build s dec p
          | Neg -> build s dec n
          | Undec -> 
	      let y = Atom.mk_neg x in
	      match Process.atom s x, Process.atom s y with
		| Process.Satisfiable(ps), Process.Satisfiable(ns) ->
		    let pdec = add ps dec in
		    let ndec = add ns dec in
		    let p = build ps pdec p in
		    let n = build ns ndec n in
		    if Prop.eq p n then p else Prop.mk_ite x p n
		| Process.Valid, _           (* Following cases can only happen *)
		| _, Process.Inconsistent -> (* for possible incompletenesses of [can]. *)
		    build s dec p
		| _, Process.Valid
		| Process.Inconsistent, _ ->
		    build s dec n


(*s Check for Satisfiability. *)

let rec sat s p =
  let q = Prop.mk_imp (Dp.p_of s) p in
  let dec = 
    add s { pos = Atom.Set.empty; 
	    neg = Atom.Set.empty; 
	    undec = Prop.literals_of q }
  in
  sat1 s dec q

and sat1 s dec b =
  match Prop.destruct b with
    | Prop.True -> Some(s)
    | Prop.False -> None
    | Prop.Ite(x,p,n) ->
	match lookup x dec with
          | Pos -> sat1 s dec p
          | Neg -> sat1 s dec n
          | Undec -> 
	      let y = Atom.mk_neg x in
	      match Process.atom s x, Process.atom s y with
		| Process.Satisfiable(ps), Process.Satisfiable(ns) ->
		    (match sat1 ps (add ps dec) p with
		       | None -> sat1 ns (add ns dec) n
		       | res -> res)
		| Process.Valid, _           (* Following cases can only happen *)
		| _, Process.Inconsistent -> (* for possible incompletenesses of [can]. *)
		    sat1 s dec p
		| _, Process.Valid
		| Process.Inconsistent, _ ->
		    sat1 s dec n




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
  neg : Atom.Set.t
}

let pp fmt dec =
  Format.fprintf fmt "@[[pos = ";
  Pretty.atoms fmt dec.pos;
  Format.fprintf fmt "; neg = ";
  Pretty.atoms fmt dec.neg;
  Format.fprintf fmt "]@]"

let empty = { 
  pos = Atom.Set.empty; 
  neg = Atom.Set.empty
}

type status = Pos | Neg | Undec

let lookup x dec =
  if Atom.Set.mem x dec.pos then Pos
  else if Atom.Set.mem x dec.neg then Neg
  else Undec

let addpos x dec = {dec with pos = Atom.Set.add x dec.pos}
let addneg x dec = {dec with neg = Atom.Set.add x dec.neg}


let rec prop s b =
  build s empty b

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
	      match Shostak.process_a s x, Shostak.process_a s y with
		| Shostak.Satisfiable(ps), Shostak.Satisfiable(ns) ->
		    let pdec = addpos x dec in
		    let ndec = addneg x dec in
		    let p = build ps pdec p in
		    let n = build ns ndec n in
		    if Prop.eq p n then p else Prop.mk_ite x p n
		| Shostak.Valid, _           (* Following cases can only happen *)
		| _, Shostak.Inconsistent -> (* for possible incompletenesses of [can]. *)
		    build s dec p
		| _, Shostak.Valid
		| Shostak.Inconsistent, _ ->
		    build s dec n


(*s Check for Satisfiability. *)

let rec sat s p =
  sat1 s empty p

and sat1 s dec b = 
  match Prop.destruct b with
    | Prop.True -> 
        Some(s)
    | Prop.False -> None
    | Prop.Ite(x,p,n) ->
	match lookup x dec with
          | Pos -> sat1 s dec p
          | Neg -> sat1 s dec n
          | Undec -> 
	      let y = Atom.mk_neg x in
	      match Shostak.process_a s x, Shostak.process_a s y with
		| Shostak.Satisfiable(ps), Shostak.Satisfiable(ns) ->
		    (match sat1 ps (addpos x dec) p with
		       | None -> sat1 ns (addneg x dec) n
		       | res -> res)
		| Shostak.Valid, _           (* Following cases can only happen *)
		| _, Shostak.Inconsistent -> (* for possible incompletenesses of [can]. *)
		    sat1 s dec p
		| _, Shostak.Valid
		| Shostak.Inconsistent, _ ->
		    sat1 s dec n



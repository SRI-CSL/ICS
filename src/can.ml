
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
 * Author: Harald Ruess, N. Shankar
 i*)

(*i*)
open Term
open Context
(*i*)

(*s Miscellaneous. *)

type sign = Pos of Term.t | Neg of Term.t

exception Found of sign

(*s Don't use [find] for uninterpreted theory. *)

let rec find th s a  =
  if Th.eq th Th.u || Th.eq th Th.app then  
    a
  else if Th.is_fully_interp th then
    Context.find th s a
  else 
    findequiv th s a

and findequiv th s a =
  try
    choose s
      (fun x ->
	 try 
	   Some(apply th s x) 
	 with Not_found -> None)
      a
  with
      Not_found -> a
    

(*s Canonization of terms. *)

let rec term s =
  Trace.func "canon" "Term" Term.pp Term.pp
    (can s)

and can s a =
  match a with
    | Var _ ->
	v s a
    | App(Sym.Arith(op), al) ->
	arith s op al
    | App(f, al) ->
	let th = Th.of_sym f in
	let interp x = find th s (can s x) in
	let al' = mapl interp al in
	let a' = if al == al' then a else sigma s f al' in
	  lookup s a'

and arith s op l =    (* special treatment for arithmetic *)
  match op, l with       (* for optimizing memory usage. *)
    | Sym.Num(q), [] -> 
	lookup s (Arith.mk_num q)
    | Sym.Multq(q), [x] -> 
	lookup s (Arith.mk_multq q (find Th.la s (can s x)))
    | Sym.Add, [x; y] -> 
	let a' = find Th.la s (can s x) 
	and b' = find Th.la s (can s y) in
	  lookup s (Arith.mk_add a' b')
    | Sym.Add, _ :: _ :: _ -> 
	let interp a = find Th.la s (can s a) in
	let l' =  mapl interp l in
	  lookup s (Arith.mk_addl l')
    | _ ->  
	let str = "Ill-formed term " ^ 
		  (Pretty.to_string Sym.pp (Sym.Arith(op))) ^
		  (Pretty.to_string (Pretty.list Term.pp) l)
	in
	  failwith str
      



(*s Canonical Term Equality. *)

let eq s a b =
  Term.eq (term s a) (term s b)



(*s Canonization of atoms. *)

let rec atom s = 
  Trace.func "canon" "Atom" Atom.pp Atom.pp
    (function 
       | Atom.True -> Atom.True
       | Atom.Equal(e) -> equal s e
       | Atom.Diseq(d) -> diseq s d
       | Atom.In(c) ->  cnstrnt s c
       | Atom.False -> Atom.False)
   
and equal s e =
  let (a, b, _) = Fact.d_equal e in
  let x' = can s a
  and y' = can s b in
  match Context.is_equal s x' y' with
    | Three.Yes ->
	Atom.mk_true()
    | Three.No -> 
	Atom.mk_false()
    | Three.X -> 
	let (x'', y'') = crossmultiply s (x', y') in
	Atom.mk_equal (Fact.mk_equal x'' y'' None)
 
and diseq s d =
  let (a, b, _) = Fact.d_diseq d in
  let x' = can s a
  and y' = can  s b in
    match Context.is_equal s x' y' with
      | Three.Yes -> Atom.mk_false()
      | Three.No -> Atom.mk_true()
      | Three.X ->
	  let (x'', y'') = crossmultiply s (x', y') in
	    Atom.mk_diseq (Fact.mk_diseq x'' y'' None)


and cnstrnt s c =
  let (a, c, _) = Fact.d_cnstrnt c in
  let mk_in x i =
    let (x', i') = normalize s (x, i) in
      Atom.mk_in (Fact.mk_cnstrnt x i' None)
  in
  let a' = can s a in
  try                 
    let d = Context.cnstrnt s a' in
    match Cnstrnt.cmp c d with
      | Binrel.Sub -> 
	  mk_in a' c
      | (Binrel.Super | Binrel.Same) ->
	  Atom.mk_true()
      | Binrel.Disjoint ->
	  Atom.mk_false()
      | Binrel.Singleton(q) ->
	  let n = lookup s (Arith.mk_num q) in
	  Atom.mk_equal (Fact.mk_equal n a' None)
      | Binrel.Overlap(cd) ->
	  mk_in a' cd
  with
      Not_found -> mk_in a' c

and crossmultiply s (a, b) =
  let (a', b') = crossmultiply1 s (a, b) in
    if Term.eq a a' && Term.eq b b' then
      (a, b)
    else 
      let (a'', b'') = crossmultiply s (a', b') in
	(can s a'', can s b'')

and crossmultiply1 s =
  Trace.func "shostak" "Crossmultiply" 
    (Pretty.pair Term.pp Term.pp) 
    (Pretty.pair Term.pp Term.pp)
    (fun (a, b) ->
       let da = Pp.denumerator a in
       let db = Pp.denumerator b in
       let d = Pp.lcm da db in
	 if Pp.is_one d then (a, b) else
	   (Sig.mk_mult a d, Sig.mk_mult b d))


and normalize s (a, c)=     (* following still suspicious. *)
  let (a', c') = normalize1 s (a, c) in
    if Term.eq a a' && Cnstrnt.eq c c' then
      (a, c)
    else 
      let (a'', c'') = normalize s (a', c') in
	(can s a'', c'')

and normalize1 s =
  Trace.func "shostak" "Normalize" Term.pp_in Term.pp_in
    (fun (a, c) ->
       let arrange a q ds =    (* compute [a * ds - q * ds]. *)
	 Arith.mk_sub (Sig.mk_mult a ds) (Arith.mk_multq q ds)
       in
       let lower dom alpha = Cnstrnt.mk_lower dom (Mpa.Q.zero, alpha) in
       let upper dom alpha = Cnstrnt.mk_upper dom (alpha, Mpa.Q.zero) in
	 try 
	   match Cnstrnt.d_upper c with        (* [a < q] or [a <= q]. *)
	     | Some(dom, q, beta) ->
		 (match signed_denum s a with
		    | Pos(ds) -> 
			(arrange a q ds, lower dom beta)
		    | Neg(ds) -> 
			(arrange a q ds, upper dom beta))
	     | _ ->
		 (match Cnstrnt.d_lower c with  (*s [a > q] or [a >= q]. *)
		    | Some(dom, alpha, q) ->
			(match signed_denum s a with
			   | Pos(ds) -> 
			       (arrange a q ds, upper dom alpha)
			   | Neg(ds) -> 
			       (arrange a q ds, lower dom alpha))
		    | _ -> (a, c))
	   with
	       Not_found -> (a, c))

and signed_denum s a =
  try
    List.iter
      (fun m ->
	 let (_, x) = Arith.mono_of m in
	 let d = Pp.denumerator x in
	   if Pp.is_one d then () else
	     let i = Context.cnstrnt s d in
	       if Cnstrnt.is_pos i then
		 raise (Found(Pos(d)))
	       else if Cnstrnt.is_neg i then
		 raise (Found(Neg(d)))
	       else 
		 ())
      (Arith.monomials a);
    raise Not_found
  with
      Found(res) -> res

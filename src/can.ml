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

open Sym
open Term
open Context


(* Don't use [find] for uninterpreted theory. *)

let rec find th s a  =
  if Th.eq th Th.u || Th.eq th Th.app then  
    a
  else if Th.is_fully_interp th then
    let b = Context.find th s a in
      b
  else 
    findequiv th s a

and findequiv th s a =
  try
    choose s
      (fun x ->
	 try Some(apply th s x) 
	 with Not_found -> None)
      a
  with
      Not_found -> a


(** {6 Canonization of terms} *)

let rec term s =
  Trace.func "canon" "Term" Term.pp Term.pp
    (can s)

and can s a =
  match a with
    | Var _ ->
	v s a
    | App(Sym.Arith(op), al) ->
	arith s op al
    | App(Sym.Bvarith(Sym.Unsigned), [x]) ->
	unsigned s x
    | App(Sym.Pp(op), xl) ->
	pprod s op xl
    | App(f, al) ->
	let th = Th.of_sym f in
	let interp x = find th s (can s x) in
	let al' = mapl interp al in
	let a' = if al == al' then a else sigma s f al' in
	  lookup s a'

and pprod s op al =   
  match op, al with
    | Expt(n), [x] ->
	lookup s (Sig.mk_expt n (find Th.la s (can s x)))
    | Mult, xl ->
	lookup s (Sig.mk_multl (Term.mapl (fun x -> find Th.la s (can s x)) xl))
    | _ ->
	assert false

and unsigned s x =
  lookup s (Bvarith.mk_unsigned (find Th.bv s (can s x)))

and arith s op l =       (* special treatment for arithmetic *)
  match op, l with       (* for optimizing memory usage. *)
    | Sym.Num(q), [] -> 
	lookup s (Arith.mk_num q)
    | Sym.Multq(q), [x] -> 
	let y = can s x in
	lookup s (Arith.mk_multq q (find Th.la s y))
    | Sym.Add, [x; y] -> 
	let a' = find Th.la s (can s x) 
	and b' = find Th.la s (can s y) in
	  lookup s (Arith.mk_add a' b')
    | Sym.Add, _ :: _ :: _ -> 
	let f a = find Th.la s (can s a) in
	let l' =  mapl f l in
	  lookup s (Arith.mk_addl l')
    | _ ->  
	let str = "Ill-formed term " ^ 
		  (Pretty.to_string Sym.pp (Sym.Arith(op))) ^
		  (Pretty.to_string (Pretty.list Term.pp) l)
	in
	  failwith str
      
let eq s a b =
  Term.eq (term s a) (term s b)


(** {6 Canonization and normalization of atoms} *)

let rec atom s = 
  Trace.func "canon" "Atom" Atom.pp Atom.pp
    (function 
       | Atom.True -> Atom.True
       | Atom.Equal(a, b) -> equal s (a, b)
       | Atom.Diseq(a, b) -> diseq s (a, b)
       | Atom.Less(a, kind, b) ->  less s (a, kind, b)
       | Atom.Greater(a, kind, b) -> greater s (a, kind, b)
       | Atom.In(a, d) -> cnstrnt s (a, d)
       | Atom.False -> Atom.False)
   
and equal s (a, b) = 
  let x' = can s a and y' = can s b in
    match Context.is_equal s x' y' with
      | Three.Yes ->
	  Atom.mk_true
      | Three.No -> 
	  Atom.mk_false
      | Three.X -> 
	  let (x'', y'') = crossmultiply s (x', y') in
	    Atom.mk_equal (x'', y'')
 
and diseq s (a, b) =
  let x' = can s a and y' = can s b in
    match Context.is_equal s x' y' with
      | Three.Yes -> 
	  Atom.mk_false
      | Three.No -> 
	  Atom.mk_true
      | Three.X ->
	  let (x'', y'') = crossmultiply s (x', y') in
	    Atom.mk_diseq (x'', y'')

and crossmultiply s (a, b) =
  let (a', b') = crossmultiply1 s (a, b) in
    if Term.eq a a' && Term.eq b b' then
      (a, b)
    else 
      let (a'', b'') = crossmultiply s (a', b') in
	(can s a'', can s b'')

and crossmultiply1 s (a, b) =
  let da = Pp.denumerator a in
  let db = Pp.denumerator b in
  let (_, _, d) = Pp.lcm (da, db) in
    if Pp.is_one d then (a, b) else
      (Sig.mk_mult a d, Sig.mk_mult b d)

and less s (x, beta, b) =   (* [x <(=) b] *)
  let x = can s x
  and b = Context.find Th.la s (can s b) in (* use arithmetic interp if possible *)
  try                           
    let c = Context.cnstrnt s x in
    let d = Cnstrnt.mk_less Dom.Real (b, beta) in
      (match Cnstrnt.cmp c d with
	| Cnstrnt.Super -> 
	    Atom.mk_less (x, beta, b)
	| (Cnstrnt.Sub | Cnstrnt.Same) ->
	    Atom.mk_true
	| Cnstrnt.Disjoint ->
	    Atom.mk_false
	| Cnstrnt.Overlap ->
	    Atom.mk_less (x, beta, b))
  with
      Not_found ->
	Atom.mk_less (x, beta, b)

and greater s (x, alpha, a) =  (* [x >(=) a] *)
  let x = can s x
  and a =  Context.find Th.la s (can s a) in
    try                           
      let c = Context.cnstrnt s x in
      let d = Cnstrnt.mk_greater Dom.Real (alpha, a) in
	(match Cnstrnt.cmp c d with
	   | Cnstrnt.Super -> 
	       Atom.mk_greater (x, alpha, a)
	   | (Cnstrnt.Sub | Cnstrnt.Same) ->
	       Atom.mk_true
	   | Cnstrnt.Disjoint ->
	       Atom.mk_false
	   | Cnstrnt.Overlap ->
	       Atom.mk_greater (x, alpha, a))
    with
	Not_found ->
	  Atom.mk_greater (x, alpha, a)

and cnstrnt s (a, d) =
  let a = can s a in
    Atom.mk_in (a, d)

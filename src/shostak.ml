
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
open Sym
open Term
open Context
open Mpa
open Three
(*i*)

let nonlinear = ref true

(*s Only interpreted find. *)

let find i s x =
  match i with
    | Sym.U -> x
    | _ -> Context.find i s x
    

(*s Canonization of terms. *)

let rec can_t s a =
  Trace.call "shostak" "Can" a Term.pp;
  let b = canterm s a in
    Trace.exit "shostak" "Can" b Term.pp;
    b

and canterm s a =
  match a with
    | Var _ -> v s a
    | App(f, al) ->
	match f with
	  | Arith(op) -> 
	      canarith s op al
	  | Tuple(op) -> 
	      cantuple s op al
	  | Bv(op) -> 
	      canbv s op al
	  | Builtin(op) -> 
	      canbuiltin s op al
	  | Uninterp(op) -> 
	      let al' = mapl (canterm s) al in
		lookup s (if al == al' then a else mk_app f al')

and canarith s op l = 
  match op, l with
    | Num(q), [] -> 
	lookup s (Arith.mk_num q)
    | Multq(q), [x] -> 
	lookup s (Arith.mk_multq q (Solution.find s.a (canterm s x)))
    | Add, [x; y] -> 
	let a' = Solution.find s.a (canterm s x) 
	and b' = Solution.find s.a (canterm s y) in
	  lookup s (Arith.mk_add a' b')
    | Add, _ :: _ :: _ -> 
	let interp a = Solution.find s.a (canterm s a) in
	let l' =  mapl interp l in
	  lookup s (Arith.mk_addl l')
    | _ -> 
	assert false

and cantuple s op al =
  let interp a = Solution.find s.t (canterm s a) in
  let bl =  mapl interp al in
  lookup s (Tuple.sigma op bl)

and canbv s op al = 
  let interp a = Solution.find s.bv (canterm s a) in
  let bl =  mapl interp al in
  lookup s (Bitvector.sigma op bl)

and canbuiltin s op l =
  match op, l with
    | Update, [x; y; z] -> 
	Sig.update s (canterm s x, canterm s y, canterm s z)
    | Select, [x; y] -> 
	Sig.select s (canterm s x, canterm s y)
    | Floor, [x] -> 
	Sig.floor s (canterm s x)
    | Ceiling, [x] -> 
	Sig.ceiling s (canterm s x)
    | Mult, l -> 
	Sig.multl s (mapl (canterm s) l)
    | Div, [x; y] -> 
	Sig.div s (canterm s x, canterm s y)
    | Expt, [x; y] -> 
	Sig.expt s (canterm s x) (canterm s y)
    | Sin, [x] -> 
	Sig.sin s (canterm s x)
    | Cos, [x] -> 
	Sig.cos s (canterm s x)
    | Unsigned, [x] -> 
	Sig.unsigned s (canterm s x)
    | _ ->
	assert false

(*s Canonical Term Equality. *)

let eq s a b =
  Term.eq (canterm s a) (canterm s b)


(*s Canonization of atoms. *)

let rec can s a = 
  Trace.call "shostak" "Can" a Atom.pp;
  let a' = 
    match a with
      | Atom.True -> Atom.True
      | Atom.Equal(x,y) -> can_e s (x, y)
      | Atom.Diseq(x,y) -> can_d s (x, y)
      | Atom.In(c,x) -> can_c s c x
      | Atom.False -> Atom.False
  in
  Trace.exit "shostak" "Can" a' Atom.pp;
  a'
	  
and can_e s (a, b) =
  let x' = canterm s a
  and y' = canterm s b in
  match Context.is_equal s x' y' with
    | Yes ->
	Atom.mk_true()
    | No -> 
	Atom.mk_false()
    | X -> 
	let (x'', y'') = crossmultiply s (x', y') in
	Atom.mk_equal x'' y''
 
and can_d s (a, b) =
  let x' = canterm s a
  and y' = canterm s b in
    match Context.is_equal s x' y' with
      | Yes -> Atom.mk_false()
      | No -> Atom.mk_true()
      | X ->
	  let (x'', y'') = crossmultiply s (x', y') in
	    Atom.mk_diseq x'' y''

and crossmultiply s (a, b) =
  let m = Sig.mult s (denums s a, denums s b) in
    (Sig.mult s (a, m), Sig.mult s (b, m))

and denums s a = 
  List.fold_left
    (fun acc m ->
       let (_, pp) = Arith.mono_of m in
	 match pp with
	   | App(Builtin(Div), [_; y]) -> Sig.mult s (y, acc)
	   | _ -> acc)
    Arith.mk_one 
    (Arith.monomials a)

and can_c s c a =
  let mk_in x i =
    let (x', i') = (* normalize s *) (x, i) in
      Atom.mk_in i' x'
  in
  let a' = canterm s a in
  try                 
    let d = cnstrnt s a' in
    match Cnstrnt.cmp c d with
      | Binrel.Sub -> 
	  mk_in a' c
      | (Binrel.Super | Binrel.Same) ->
	  Atom.mk_true()
      | Binrel.Disjoint ->
	  Atom.mk_false()
      | Binrel.Singleton(q) ->
	  let n = lookup s (Arith.mk_num q) in
	  Atom.mk_equal n a'
      | Binrel.Overlap(cd) ->
	  mk_in a' cd
  with
      Not_found -> mk_in a' c

and normalize s (a, c) =
  try
    match Cnstrnt.d_upper c with     (* [a < q] or [a <= q]. *)
      | Some(dom, q, beta) ->
	  let ds = denums s a in
          let d = cnstrnt s ds in
	    if Cnstrnt.is_pos d then
	      (Sig.sub s (Sig.mult s (a, ds)) (Sig.multq s q ds), 
	       Cnstrnt.mk_lower dom (Mpa.Q.zero, beta))
	    else if Cnstrnt.is_neg d then
	      (Sig.sub s (Sig.mult s (a, ds)) (Sig.multq s q ds), 
	       Cnstrnt.mk_upper dom (beta, Mpa.Q.zero))
	    else 
		(a, c)
      | _ ->
	  (match Cnstrnt.d_lower c with  (*s [a > q] or [a >= q]. *)
	     | Some(dom, alpha, q) ->
		 let ds = denums s a in
		 let d = cnstrnt s ds in
		   if Cnstrnt.is_pos d then
		       (Sig.sub s (Sig.mult s (a, ds)) (Sig.multq s q ds),
			Cnstrnt.mk_upper dom (alpha, Mpa.Q.zero))
		   else if Cnstrnt.is_neg d then
		       (Sig.sub s (Sig.mult s (a, ds)) (Sig.multq s q ds),
			Cnstrnt.mk_lower dom (Mpa.Q.zero, alpha))
		   else 
		       (a, c)
	     | _ ->
		 (a, c))
    with
	Not_found ->
	  (a, c)
	  


(*s Abstraction. *)

let rec abstract s a = 
  Trace.call "shostak" "Abstract" a Atom.pp;
  let (s', a') = abstract_atom s a in
    Trace.exit "shostak" "Abstract" a' Atom.pp;
    (s', a')

and abstract_atom s = function
  | Atom.True -> 
      (s, Atom.True)
  | Atom.False -> 
      (s, Atom.False)
  | Atom.Equal(a, b) ->
      let (s', x') = abstract_toplevel_term s a in
      let (s'', y') = abstract_toplevel_term s' b in
	(s'', Atom.mk_equal x' y')
  | Atom.Diseq(a, b) -> 
      let (s', x') = abstract_toplevel_term s a in
      let (s'', y') = abstract_toplevel_term s' b in
	(s'', Atom.mk_diseq x' y')
  | Atom.In(c, a) ->
      let (s', x') = abstract_toplevel_term s a in
	(s', Atom.mk_in c x')

and abstract_toplevel_term s a =
  abstract_term U s a
	    
and abstract_args i s al =
  match al with
    | [] -> 
	(s, [])
    | b :: bl ->
	let (s', bl') = abstract_args i s bl in
	let (s'', b') = abstract_term i s' b in
	  if Term.eq b b' && bl == bl' then
	    (s'', al)
	  else 
	    (s'', b' :: bl')

and abstract_term i s a =
  match a with
    | Var _ -> 
	(s, a)
    | App(f, al) ->
	let j = theory_of f in
	let (s', al') = abstract_args j s al in
	let a' = if Term.eql al al' then a else App(f, al') in
	  if i = U || i <> j then
	    try
	      (s', v s (inv j s a'))
	    with Not_found ->
	      let (x'', s'') = Rule.extend s' a' in
		(s'', x'')
	  else 
	    (s', a')


(*s Processing an atom *)

type 'a status = 
  | Valid 
  | Inconsistent 
  | Satisfiable of 'a

let rec process s a =
  Trace.call "shostak" "Process" a Atom.pp;
  let exitmsg str = 
    Trace.exit "shostak" "Process" str Pretty.string
  in
  let s = {s with ctxt = Atom.Set.add a s.ctxt} in
  let a' = can s a in
  let (s', a') = abstract s a' in
  try
    match a' with
      | Atom.True -> 
	  exitmsg "Valid";
	  Valid
      | Atom.False -> 
	  exitmsg "Inconsistent";
	  Inconsistent
      | Atom.Equal(x,y) -> 
	  let s'' = merge x y s' in
	    exitmsg "Satisfiable";
	    Satisfiable(s'')
      | Atom.Diseq(x,y) -> 
	  let s'' = diseq x y s' in
	    exitmsg "Satisfiable";
	    Satisfiable(s'')
      | Atom.In(i,a) -> 
	  let s'' = add a i s' in
	    exitmsg "Satisfiable";
	    Satisfiable(add a i s')
  with 
      Exc.Inconsistent -> 
	Trace.exit "shostak" "Process" "Inconsistent" Pretty.string;
	Inconsistent

and merge x y s = 
  let e = Fact.mk_equal x y None in
  Rule.close (Rule.merge e s)

and add x i s = 
  let c = Fact.mk_cnstrnt x i None in
  Rule.close (Rule.add c s)

and diseq x y s =
  let d = Fact.mk_diseq x y None in
  Rule.close (Rule.diseq d s)
  

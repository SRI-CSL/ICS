
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
open Theories
open Sym
open Term
open Context
open Mpa
open Three
(*i*)

let nonlinear = ref true

type sign = Pos of Term.t | Neg of Term.t

exception Found of sign

exception Foundterm of Term.t


(*s Canonization of terms. *)

let rec can_t s =
  Trace.func "shostak" "Can" Term.pp Term.pp
    (canterm s)

and canterm s a =
  match a with
    | Var _ ->
	v s a
    | App(f, al) ->
	match f with
	  | Arith(op) -> 
	      canarith s op al
	  | Tuple(op) -> 
	      cantuple s op al
	  | Bv(op) -> 
	      canbv s op al
	  | Builtin(op) -> 
	      canbuiltin a s op al
	  | _ -> 
	      let al' = mapl (canterm s) al in
		lookup s (if al == al' then a else mk_app f al')

and canarith s op l = 
  match op, l with
    | Num(q), [] -> 
	lookup s (Arith.mk_num q)
    | Multq(q), [x] -> 
	lookup s (Arith.mk_multq q (find A s (canterm s x)))
    | Add, [x; y] -> 
	let a' = find A s (canterm s x) 
	and b' = find A s (canterm s y) in
	  lookup s (Arith.mk_add a' b')
    | Add, _ :: _ :: _ -> 
	let interp a = find A s (canterm s a) in
	let l' =  mapl interp l in
	  lookup s (Arith.mk_addl l')
    | _ -> 
	assert false

and cantuple s op al =
  let interp a = find T s (canterm s a) in
  let bl =  mapl interp al in
  lookup s (Tuple.sigma op bl)

and canbv s op al = 
  let interp a = find BV s (canterm s a) in
  let bl =  mapl interp al in
  lookup s (Bitvector.sigma op bl)

and canbuiltin a s op l =
  match op, l with
    | Update, [x; y; z] -> 
	Sig.update s (canterm s x, canterm s y, canterm s z)
    | Select, [x; y] -> 
	Sig.select s (canterm s x, canterm s y)
    | Mult, al -> 
	Sig.multl s (mapl (canterm s) al)
    | Div, [x; y] -> 
	Sig.div s (canterm s x, canterm s y)
    | Expt, [x; y] -> 
	Sig.expt s (canterm s x) (canterm s y)
    | Unsigned, [x] -> 
	Sig.unsigned s (canterm s x)
    | Apply(r), [x; y] ->
        Sig.apply s r (canterm s x) (canterm s y)
    | Lambda(i), [x] ->
	Sig.lambda s i x
    | _ ->
	assert false


(*s Canonical Term Equality. *)

let eq s a b =
  Term.eq (canterm s a) (canterm s b)


(*s Canonization of atoms. *)

let rec can s = 
  Trace.func "shostak" "Can" Atom.pp Atom.pp
    (function 
       | Atom.True -> Atom.True
       | Atom.Equal(e) -> can_equal s e
       | Atom.Diseq(d) -> can_diseq s d
       | Atom.In(c) ->  can_cnstrnt s c
       | Atom.False -> Atom.False)
   
and can_equal s e =
  let (a, b, _) = Fact.d_equal e in
  let x' = canterm s a
  and y' = canterm s b in
  match Context.is_equal s x' y' with
    | Yes ->
	Atom.mk_true()
    | No -> 
	Atom.mk_false()
    | X -> 
	let (x'', y'') = crossmultiply s (x', y') in
	Atom.mk_equal (Fact.mk_equal x'' y'' None)
 
and can_diseq s d =
  let (a, b, _) = Fact.d_diseq d in
  let x' = canterm s a
  and y' = canterm s b in
    match Context.is_equal s x' y' with
      | Yes -> Atom.mk_false()
      | No -> Atom.mk_true()
      | X ->
	  let (x'', y'') = crossmultiply s (x', y') in
	    Atom.mk_diseq (Fact.mk_diseq x'' y'' None)

and crossmultiply s (a, b) =
  let (a', b') = crossmultiply1 s (a, b) in
    if Term.eq a a' && Term.eq b b' then
      (a', b')
    else 
      crossmultiply s (a', b')

and crossmultiply1 s =
  Trace.func "shostak" "Crossmultiply" 
    (Pretty.pair Term.pp Term.pp) 
    (Pretty.pair Term.pp Term.pp)
    (fun (a, b) ->
       try
	 let d = denum s a in
	   (Sig.mult s (a, d), Sig.mult s (b, d))
       with
	   Not_found ->
	     try
	       let d = denum s b in
		 (Sig.mult s (a, d), Sig.mult s (b, d))
	     with
		 Not_found -> (a, b))

and denum s =
  Trace.func "shostak" "Denum" Term.pp Term.pp
    (fun a ->
       try
	 List.iter
	   (fun m ->
	      let (_, pp) = Arith.mono_of m in
		match pp with
		  | App(Builtin(Div), [_; y]) -> raise (Foundterm(y))
		  | _ -> ())
	   (Arith.monomials a);
	 raise Not_found
       with
	   Foundterm(d) -> d)

and can_cnstrnt s c =
  let (a, c, _) = Fact.d_cnstrnt c in
  let mk_in x i =
    let (x', i') = normalize s (x, i) in
      Atom.mk_in (Fact.mk_cnstrnt x i' None)
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
	  Atom.mk_equal (Fact.mk_equal n a' None)
      | Binrel.Overlap(cd) ->
	  mk_in a' cd
  with
      Not_found -> mk_in a' c

and normalize s (a, c)=     (* following still suspicious. *)
  let (a', c') = normalize1 s (a, c) in
    if Term.eq a a' && Cnstrnt.eq c c' then
      (a', c')
    else 
      normalize s (a', c')

and normalize1 s =
  Trace.func "shostak" "Normalize" Term.pp_in Term.pp_in
    (fun (a, c) ->
       let arrange a q ds =    (* compute [a * ds - q * ds]. *)
	 Sig.sub s (Sig.mult s (a, ds)) (Sig.multq s q ds)
       in
       let lower dom alpha = Cnstrnt.mk_lower dom (Mpa.Q.zero, alpha) in
       let upper dom alpha = Cnstrnt.mk_upper dom (alpha, Mpa.Q.zero) in
	 try
	   match Cnstrnt.d_upper c with        (* [a < q] or [a <= q]. *)
	     | Some(dom, q, beta) ->
		 (match signed_denum s a with
		    | Pos(ds) -> (arrange a q ds, lower dom beta)
		    | Neg(ds) -> (arrange a q ds, upper dom beta))
	     | _ ->
		 (match Cnstrnt.d_lower c with  (*s [a > q] or [a >= q]. *)
		    | Some(dom, alpha, q) ->
			(match signed_denum s a with
			   | Pos(ds) -> (arrange a q ds, upper dom alpha)
			   | Neg(ds) -> (arrange a q ds, lower dom alpha))
		    | _ -> (a, c))
	   with
	       Not_found -> (a, c))

and signed_denum s a =
  try
    List.iter
      (fun m ->
	 let (_, pp) = Arith.mono_of m in
	   match pp with
	     | App(Builtin(Div), [_; y]) -> 
		 (try
		   let d = cnstrnt s y in
		     if Cnstrnt.is_pos d then
		       raise (Found(Pos(y)))
		     else if Cnstrnt.is_neg d then
		       raise (Found(Neg(y)))
		     else 
		       ()
		 with
		     Not_found -> ())
	     | _ -> ())
      (Arith.monomials a);
    raise Not_found
  with
      Found(res) -> res



(*s Processing an atom *)

type 'a status = 
  | Valid 
  | Inconsistent 
  | Satisfiable of 'a

let pp_status pp fmt = function
  | Valid -> Format.fprintf fmt ":valid"
  | Inconsistent -> Format.fprintf fmt ":unsat"
  | Satisfiable(x) -> Format.fprintf fmt ":ok "; pp fmt x


let rec process s =
  Trace.func "shostak" "Process" Atom.pp (pp_status Context.pp)
    (fun a ->
       try
	 match can s a with
	   | Atom.True -> 
	       Valid
	   | Atom.False ->
	       Inconsistent
	   | Atom.Equal(e) ->
	       Satisfiable(merge a e s)
	   | Atom.Diseq(d) -> 
	       Satisfiable(diseq a d s)
	   | Atom.In(c) -> 
	       Satisfiable(add a c s)
	 with 
	     Exc.Inconsistent -> Inconsistent)
  
and merge a e =  
  Context.protect
    (fun s  ->
       let s = Context.extend a s in
       let (s', e') = Rule.abstract_equal (s, e) in
       let s'' = Rule.merge e' s' in
	 Rule.close s'')
    
and add a c = 
  Context.protect
    (fun s  ->
       let s = Context.extend a s in
       let (s', c') = Rule.abstract_cnstrnt (s, c) in
       let s'' = Rule.add c' s' in
	 Rule.close s'')

and diseq a d =
  Context.protect
    (fun s  ->  
       let s = Context.extend a s in
       let (s', d') = Rule.abstract_diseq (s, d) in
       let s'' = Rule.diseq d' s' in
	 Rule.close s'')
  



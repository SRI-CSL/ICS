
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


(*s Only interpreted find. *)

let find i s x =
  match i with
    | Sym.U -> x
    | _ -> Context.find i s x

(*s Abstraction. *)

let rec abstract s a = 
  Trace.msg "shostak" "Abstract" a Atom.pp;
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
	  if Term.eq b b' && Term.eql bl bl' then
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
	      let (x'', s'') = extend s' a' in
		(s'', x'')
	  else 
	    (s', a')
	

(* Lookup terms on rhs of solution sets. *)
    
let lookup s a = 
  match a with
    | Var _ ->
	v s a
    | App(f, _) ->
	let i = theory_of f in
	  try v s (inv i s a) with Not_found -> a
    

(*s Canonization of terms. *)

let rec can_t s a =
  match a with
    | Var _ ->
	v s a
    | App(f, al) ->
	match f, al with
	  | Arith(Multq(q)), [x] ->
	      canmultq s q x
	  | Arith(op), _ -> 
	      let al' = canl s A al in
	      let a' = if al == al' then a else Arith.sigma op al' in 
		lookup s a'
	  | Tuple(op), _ -> 
	      let al' = canl s T al in
	      let a' = if al == al' then a else Tuple.sigma op al' in
		lookup s a'
	  | Bv(op), _ -> 
	      let al' = canl s BV al in
	      let a' = if al == al' then a else Bitvector.sigma op al' in
		lookup s a'
	  | Uninterp(op), [x; y; z] when Name.eq Name.update op ->
	      canupdate s (can_t s x, can_t s y, can_t s z)
	  | Uninterp(op), [x; y] when Name.eq Name.select op ->
	      canselect s (can_t s x, can_t s y)
	  | Uninterp(op), [x] when Name.eq Name.floor op ->
	      canfloor s (can_t s x)
	  | Uninterp(op), xl when Name.eq Name.mult op ->
	      let xl' = mapl (can_t s) xl in
	      canmultl s xl'
	  | Uninterp(op), [x; y] when Name.eq Name.div op ->
	      candiv s (can_t s x, can_t s y)
	  | Uninterp(op), [x; y] when Name.eq Name.expt op ->
	      canexpt s (can_t s x) (can_t s y)
	  | Uninterp(op), [x] when Name.eq Name.sin op ->
	      cansin s (can_t s x)
	  | Uninterp(op), [x] when Name.eq Name.cos op ->
	      cancos s (can_t s x)
	  | Uninterp(op), _ -> 
	      let al' = canl s U al in
	      let a' = if al == al' then a else mk_app f al' in
		lookup s a'

and canl s i = 
  mapl (fun a -> find i s (can_t s a))


(*s Sigmatization. [sigma s f l] encodes the following 
 array axioms: [select(update(a,i,x),j)] reduces to [x] if [i = j] 
 and to  [select(a,j)] if [i <> j]. *)

and canselect s (upd, j) = 
  try
    let (yes, a, i, x) = 
      choose s
	(fun upd' ->
	   match Solution.find s.u upd' with
	     | App(Uninterp(op), [a; i; x])
		 when Name.eq Name.update op -> 
		 (match is_equal s i j with
		   | Three.Yes  -> Some(true, a, i, x)
		   | Three.No -> Some(false, a, i, x)
		   | _ -> None)
	     | _ -> None)
	upd
    in
      if yes then      (* [i = j => select(update(a,i,x), j) = x] *)
	x
      else             (* [i <> j => select(update(a,i,x),j) = select(a,j)] *)
	lookup s (mk_app Sym.select [a; j])
  with
     Not_found -> 
       lookup s (mk_app Sym.select [upd; j])


and canupdate s (a, j, y) = 
  try
    let (b, i, x) =
      choose s
	(fun a' ->
	   match Solution.find s.u a' with
	     | App(Uninterp(update), [b; i; x])
		 when Name.eq Name.update update && Term.eq i j ->
		 Some(b, i, x)
	     | _ -> None)
	a
    in
      lookup s (Term.mk_app Sym.update [b; i; y])
  with
      Not_found ->
	lookup s (Term.mk_app Sym.update [a; j; y])


and canfloor s a =
  let a = find A s a in
  let ms = Arith.monomials a in
  let (ints, nonints) = List.partition (is_int s) ms in
  let nonint' = Arith.mk_addl nonints in
  let fl = match Arith.d_num nonint' with
    | Some(q) -> 
	lookup s (Arith.mk_num (Mpa.Q.of_z (Mpa.Q.floor q)))
    | None -> 
	lookup s (Term.mk_app Sym.floor [lookup s nonint']) 
  in
    lookup s (Arith.mk_addl (fl :: ints))

and canmultq s q a =
  let a' = find U s a in
  match a' with
    | App(Uninterp(div), [App(Arith(Num(p)), []); x])
	when Name.eq Name.div div ->
	canmultq s (Q.mult q p) (caninv s x)
    | _ ->
	lookup s (Arith.mk_multq q a')

and canmult s (a, b) =
  let rec multl l1 l2 =             
    match l1, l2 with
      | [], _ -> l2
      | _ , [] -> l1
      | m1 :: l1', m2 :: l2' ->
	  let n1, x1 = expt_of m1 in
	  let n2, x2 = expt_of m2 in
	  let cmp = Term.cmp x1 x2 in
	    if cmp = 0 then
	      let n = Q.add n1 n2 in
		if Q.is_zero n then
		  multl l1' l2'
		else 
		  (canexpt s (Arith.mk_num n) x1) :: (multl l1' l2')
	    else if cmp < 0 then
	      m1 :: multl l1' l2
	    else (* cmp > 0 *)
	      m2 :: multl l1 l2'
  and of_list l =
    match l with
      | [] -> 
	  lookup s (Arith.mk_one)
      | [x] -> x
      | _ -> lookup s (Term.mk_app Sym.mult l)
  and expt_of a =
    match a with
      | App(Uninterp(expt), [App(Arith(Num(q)), []); y]) 
	  when Name.eq expt Name.expt ->
	  (q, y)
      | _ ->
	  (Q.one, a)
  in
    if Term.eq a b then                                (* [ a * a --> a^2] *)
      canexpt s (Arith.mk_num (Q.of_int 2)) a 
    else
      let a' = find A s a and b' = find A s b in
      match a', b' with
	| App(Arith(Num(q)), []), App(Arith(Num(p)), []) ->
	    lookup s (Arith.mk_num (Q.mult q p))
	| _, App(Arith(Num(p)), []) ->
	    canmultq s p a
	| App(Arith(Num(q)), []), _ -> 
	    canmultq s q b
	| App(Arith(Add), xl), _ ->    (* [(x1+...+xn) * b = x1*b+...+xn*b] *)
	    let xl' = mapl (fun x -> canmult s (x, b)) xl in
	    lookup s (Arith.mk_addl xl')
	| _,  App(Arith(Add), yl) ->   (* [a * (y1+...+yn) = y1*a+...+yn*a] *)
	    let yl' = mapl (fun y -> canmult s (y, a)) yl in
	    lookup s (Arith.mk_addl yl')
	| App(Uninterp(mult1), xl), App(Uninterp(mult2), yl)
	    when Name.eq mult1 Name.mult 
	      && Name.eq mult2 Name.mult -> 
	    of_list (multl xl yl)
	| App(Uninterp(mult), xl), _
	    when Name.eq mult Name.mult -> 
	    of_list (multl xl [b'])
	| _, App(Uninterp(mult), yl)
	    when Name.eq mult Name.mult -> 
	    of_list (multl yl [a'])
	| App(Uninterp(expt1), [n; x]), App(Uninterp(expt2), [m; y])
	    when Name.eq expt1 Name.expt
	      && Name.eq expt2 Name.expt
	      && Term.eq x y ->                 (* [ x^n * x^m --> x^(n+m)] *)
            canexpt s (Arith.mk_add n m) x
	| App(Uninterp(expt), [n; x]), _
	    when Name.eq expt Name.expt
	      && Term.eq x b ->                 (* [ x^n * x --> x^(n+1)] *)
	    canexpt s (Arith.mk_incr n) x
	| _, App(Uninterp(expt), [m; y])
	    when Name.eq expt Name.expt
              && Term.eq y a ->                 (* [ y * y^m --> y^(m+1)] *)
	    canexpt s (Arith.mk_incr m) y
	| _ ->
	    let (a, b) = Term.orient (a, b) in
	    lookup s (Term.mk_app Sym.mult [a; b])
  
and canmultl s al =
  match al with
    | [] -> lookup s (Arith.mk_one)
    | [x] -> x
    | [x; y] -> canmult s (x, y)
    | x :: y :: zl -> canmultl s (canmult s (x, y) :: zl)

and canexpt s n a =
  let n' = find A s n and a' = find A s a in
  match n', a' with
    | App(Arith(Num(q)), []), _ when Q.is_zero q ->
	lookup s (Arith.mk_one)
    | App(Arith(Num(q)), []), _ 
	when Q.is_one q ->
	a
    | _, App(Arith(Num(q)), []) ->        (* [q^n]; to be done *)
	lookup s (Term.mk_app Sym.expt [n; a])
    | _, App(Uninterp(expt), [m; x])      (* [(x^m)^n = x^(m+n) ] *)
	when Name.eq expt Name.expt ->
	canexpt s (lookup s (Arith.mk_add n' m)) x
    | _, App(Uninterp(mult), xl)   (* [(x1 * ... * xk)^n = x1^n * ... xk^n] *)
	when Name.eq mult Name.mult ->
	canmultl s (List.map (canexpt s n) xl)
    | _, a ->
	lookup s (Term.mk_app Sym.expt [n; a])


and caninv s a =
  match find A s a with
    | App(Arith(Num(q)), []) when not(Q.is_zero q) ->
	lookup s (Arith.mk_num (Q.inv q))
    | App(Uninterp(div), [a1; a2]) 
	when Name.eq div Name.div ->
	candiv s (a2, a1)
    | _ ->
	lookup s (mk_app Sym.div [lookup s Arith.mk_one; a])

and candiv s (a, b) =
  let rec divm (m, b) =
    if Term.eq m b then                    (* [ a / a = 1] *)
      lookup s (Arith.mk_one)
    else 
      let (q, x) = Arith.mono_of m in            
	if Mpa.Q.is_zero q then            (* [ (q*x) / b = q * (x/ b)] *)
	  lookup s (Arith.mk_zero) 
	else 
	  match x with
	    | App(Arith(Num(p)), []) ->    (* [ (q*p) / b = (q*p) * 1/b] *)
		Arith.mk_multq (Q.mult q p) (caninv s b)
	    | _ -> 
		canmultq s q (cancelled_div (x, b))
  and cancelled_div (a, b) =
    mk_app Sym.div [a; b] (* no lookup, division is cross-multiplied *)
  in
    match find A s a, find A s b with
      | a, App(Arith(Num(q)), []) when not(Q.is_zero q) ->
	  canmultq s (Q.inv q) a
      | App(Uninterp(div), [a1; a2]), b
	  when Name.eq div Name.div ->
	  candiv s (a1, canmult s (a2, b))
      | a, App(Uninterp(div), [b1; b2])
	  when Name.eq div Name.div ->
	  canmult s (a, candiv s (b2, b1))
      | a, b ->
	  let ml = Arith.monomials a in
	  let ml' = List.map (fun m -> divm (m, b)) ml in
	    lookup s (Arith.mk_addl ml')
 

and cansin s a =
  match find A s a with
    | App(Arith(Add), [x; y]) ->
	let a = canmult s (cansin s x, cancos s y) in
	let b = canmult s (cancos s x, cancos s y) in
	  lookup s (Arith.mk_add a b)
    | _ ->
	lookup s (Term.mk_app Sym.sin [a])

and cancos s a =
  lookup s (Term.mk_app Sym.cos [a])


(*s Canonical Term Equality. *)

let eq s a b =
  Term.eq (can_t s a) (can_t s b)


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
  let x' = can_t s a in
  let y' = can_t s b in
  match Context.is_equal s x' y' with
    | Yes -> Atom.mk_true()
    | No -> Atom.mk_false()
    | _ -> Atom.mk_equal x' y'
 
and can_d s (a, b) =
  let x' = can_t s a in
  let y' = can_t s b in
  match Context.is_equal s x' y' with
    | Yes -> Atom.mk_false()
    | No -> Atom.mk_true()
    | X -> Atom.mk_diseq x' y'

and can_c s c a =
  let a' = can_t s a in
  try                 
    let d = cnstrnt s a' in
    match Cnstrnt.cmp c d with
      | Binrel.Sub -> 
	  Atom.mk_in c a'
      | (Binrel.Super | Binrel.Same) ->
	  Atom.mk_true()
      | Binrel.Disjoint ->
	  Atom.mk_false()
      | Binrel.Singleton(q) ->
	  Atom.mk_equal (Arith.mk_num q) a'
      | Binrel.Overlap(cd) ->
	  Atom.mk_in cd a'
  with
      Not_found -> 
	Atom.mk_in c a'


(*s Processing an atom *)

type 'a status = 
  | Valid 
  | Inconsistent 
  | Satisfiable of 'a

let rec process s a =
  Trace.msg "shostak" "Process" a Atom.pp;
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
  Context.close (Context.merge e s)

and add x i s = 
  let c = Fact.mk_cnstrnt x i None in
  Context.close (Context.add c s)

and diseq x y s =
  let d = Fact.mk_diseq x y None in
  Context.close (Context.diseq d s)
  

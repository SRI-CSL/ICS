
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



(*s Unsigned interpretation. *)

let rec unsigned s a =
  let interp_of b =
      Bitv.fold_right 
	(fun x acc -> if x then 2 * acc + 1 else 2 * acc) 
	b 0
  in
    match Solution.find s.bv a with 
      | App(Bv(Const(b)), []) ->
	  num s (Q.of_int (interp_of b))
      | App(Bv(Conc(n, m)), [x; y]) ->
	  let ux = Solution.find s.a (unsigned s x) in
	  let uy = Solution.find s.a (unsigned s y) in
	  let two_expt_m = Q.of_z (Z.expt (Z.of_int 2) m) in
	    add s (multq s two_expt_m ux) uy
      | _ -> 
	  lookup s (Term.mk_app Sym.unsigned [a])


(*s Reducing patterns of the form [select(update(a,i,x), j)]
  according to the equations
     [i = j => select(update(a,i,x), j) = x]
     [i <> j => select(update(a,i,x),j) = select(a,j)] 
 *)

and select s (upd, j) =
  let simplify upd =                     (* match [select(update(a,i,x), j)] *)
    match Solution.find s.u upd with     (* s.t. [update(a,i,x)] is equal to *)
      | App(Builtin(Update), [a; i; x]) -> (* [upd] in [u] and either [i= j] *)
	  (match is_equal s i j with     (* or [i <> j] are known in [s].    *)
	     | Three.Yes  -> Some(x)
	     | Three.No -> Some(mk_app Sym.select [a; j])
	     | Three.X -> None)
      | _ -> 
	  None
  in
    try
      choose s simplify upd
    with
	Not_found -> mk_app Sym.select [upd; j]


and update s (a, j, y) = 
   let simplify a =                  (* search for [update(b, i, x) equal *)
     match Solution.find s.u a with  (* to [a] in [s] and [i = j] in [s]. *)
       | App(Builtin(Update), [b; i; x]) 
	   when Term.eq i j -> Some(Term.mk_app Sym.update [b; i; y])
       | _ -> None
   in
     try
       choose s simplify a
     with
	 Not_found ->
	   Term.mk_app Sym.update [a; j; y]
	
   
and floor s a =
  if is_int s a then
    a
  else 
    match Solution.find s.a a with
      | App(Arith(Num(q)), []) when Q.is_integer q ->
	  a
      | App(Arith(Multq(q)), [x]) when Q.is_integer q && is_int s x ->
	  a
      | App(Arith(Add), l) ->
	  let (ints, nonints) = List.partition (is_int s) l in
          let nonint = 
            match Arith.mk_addl nonints with
              | App(Arith(Num(q)), []) -> 
	          num s (Mpa.Q.of_z (Mpa.Q.floor q))
              | nonint -> 	  
	          Term.mk_app Sym.floor [lookup s nonint]
          in
            Arith.mk_addl (nonint :: ints)
      | _ ->
	  Term.mk_app Sym.floor [a]


and ceiling s a =    (* [ceiling(x) = -floor(-x)] *)
  lookup s 
    (Arith.mk_neg 
       (floor s 
	  (lookup s (Arith.mk_neg a))))

and add s a b =
  let a' = Solution.find s.a a 
  and b' = Solution.find s.a b in
  lookup s (Arith.mk_add a' b')

and sub s a b =
  add s a (multq s Q.negone b)

and addl s = function
  | [] -> zero s
  | [x] -> lookup s x
  | [x; y] -> add s x y
  | x1 :: x2 :: xl -> addl s (add s x1 x2 :: xl)

and num s q =
  lookup s (Arith.mk_num q)

and two s = lookup s Arith.mk_two
and one s = lookup s Arith.mk_one
and zero s = lookup s Arith.mk_zero

and multq s q a =
  Trace.msg "rewrite" "Multq" a Term.pp;
  lookup s (Arith.mk_multq q (Solution.find s.a a))

and mult s (a, b) = 
  Trace.msg "rewrite" "Mult" (a, b) (Pretty.pair Term.pp Term.pp);
  let cmp a b =
    let (n, x) = expt_of a and (m, y) = expt_of b in
      let cmp = Term.cmp x y in
	if cmp = 0 then Q.compare n m else cmp
  in
  let rec multl l1 l2 =             
    match l1, l2 with
      | [], _ -> l2
      | _ , [] -> l1
      | m1 :: l1', m2 :: l2' ->
	  let n1, x1 = expt_of m1 in
	  let n2, x2 = expt_of m2 in
	  let cmp = cmp x1 x2 in
	    if cmp = 0 then
	      let n = Q.add n1 n2 in
		if Q.is_zero n then
		  multl l1' l2'
		else 
		  (expt s (num s n) x1) :: (multl l1' l2')
	    else if cmp < 0 then
	      m1 :: multl l1' l2
	    else (* cmp > 0 *)
	      m2 :: multl l1 l2'
  and of_list l =
    match l with
      | [] -> one s
      | [x] -> lookup s x
      | _ -> lookup s (Term.mk_app Sym.mult l)
  in
    if Term.eq a b then                                (* [ a * a --> a^2] *)
      expt s (num s (Q.of_int 2)) a 
    else
      let a' = Solution.find s.a a and b' = Solution.find s.a b in
	match a' with
	  | App(Builtin(Div), [a1; a2]) ->      (* [(a1 / a2) * b = (a1 * b) / a2] *) 
	      div s (mult s (a1, b), a2)
	  | _ ->
	      (match b with
		 | App(Builtin(Div), [b1; b2]) ->     (* [a * (b1 / b2) = (a * b1) / b2] *)
		     div s (mult s (a, b1), b2)
		 | _ -> 
		     (match a', b' with
			| App(Arith(Num(q)), []), App(Arith(Num(p)), []) ->
			    num s (Q.mult q p)
			| _, App(Arith(Num(p)), []) ->
			    multq s p a
			| App(Arith(Num(q)), []), _ -> 
			    multq s q b
			| App(Arith(Add), xl), _ ->    (* [(x1+...+xn) * b = x1*b+...+xn*b] *)
			    let xl' = mapl (fun x -> Solution.find s.a (mult s (x, b))) xl in
			      lookup s (Arith.mk_addl xl')
			| _,  App(Arith(Add), yl) ->   (* [a * (y1+...+yn) = y1*a+...+yn*a] *)
			    let yl' = mapl (fun y -> Solution.find s.a (mult s (y, a))) yl in
			      lookup s (Arith.mk_addl yl')
			| App(Builtin(Mult), xl), App(Builtin(Mult), yl) ->
			    of_list (multl xl yl)
			| App(Builtin(Mult), xl), _ ->
			    of_list (multl xl [b'])
			| _, App(Builtin(Mult), yl) ->
			    of_list (multl yl [a'])
			| App(Builtin(Expt), [n; x]), App(Builtin(Expt), [m; y])
			    when Term.eq x y ->                 (* [ x^n * x^m --> x^(n+m)] *)
			    expt s (add s n m) x
			| App(Builtin(Expt), [n; x]), _
			    when Term.eq x b ->                 (* [ x^n * x --> x^(n+1)] *)
			    expt s (Arith.mk_incr n) x
			| _, App(Builtin(Expt), [m; y])
			    when Term.eq y a ->                 (* [ y * y^m --> y^(m+1)] *)
			    expt s (Arith.mk_incr (Solution.find s.a m)) y
			| _ ->
			    of_list (multl [a] [b])))
  

and multl s al =
  match al with
    | [] -> one s
    | [x] -> x
    | [x; y] -> mult s (x, y)
    | x :: y :: zl -> multl s (mult s (x, y) :: zl)


and expt s n a =
  Trace.msg "rewrite" "Expt" (n, a) (Pretty.pair Term.pp Term.pp);
  let n' = Solution.find s.a n and a' = Solution.find s.a a in
    match n', a' with
      | App(Arith(Num(q)), []), _ when Q.is_zero q ->  (* [x^0 = 1] *)
	  one s
      | App(Arith(Num(q)), []), _ when Q.is_one q ->   (* [x^1 = x] *)
	  a
      | _, App(Arith(Num(q)), []) ->                       (* [q^n] *)
	  if Q.equal q Q.one then
	       Arith.mk_one
	  else
	    lookup s (Term.mk_app Sym.expt [n; a])
      | _, App(Builtin(Expt), [m; x]) ->               (* [(x^m)^n = x^(m+n) ] *)
	  expt s (lookup s (Arith.mk_add n' (Solution.find s.a m))) x
      | _, App(Builtin(Mult), xl) ->      (* [(x1 *...* xk)^n = x1^n *...xk^n] *)
	  multl s (mapl (expt s n) xl)
      | _, App(Builtin(Div), [x; y]) ->             (* [(x / y)^n = x^n / y^n] *)
	  div s (expt s n x, expt s n y)
      | App(Arith(Num(q)), []), App(Builtin(Sin), [x])  (* [sin^2(x) = 1 - cos^2(x)] *)
	  when Q.equal q Q.two ->
	  sub s (one s) (expt s (two s) (cos s x))
      | _ ->
	  lookup s (Term.mk_app Sym.expt [n; a])


and inv s a = 
  Trace.msg "rewrite" "Inv" a Term.pp;
  match Solution.find s.a a with
    | App(Arith(Num(q)), []) when not(Q.is_zero q) ->
	num s (Q.inv q)
    | App(Builtin(Div), [a1; a2]) ->      
	div s (a2, a1)
    | _ ->
	lookup s (mk_app Sym.div [one s; a])

and expt_of a =
  match a with
    | App(Builtin(Expt), [App(Arith(Num(q)), []); x]) -> (q, x)
    | _ -> (Q.one, a)

and div s (a, b) =
  Trace.msg "rewrite" "Div" (a, b) (Pretty.pair Term.pp Term.pp);
  let rec divm (m, b) =
    if Term.eq m b then                    (* [ a / a = 1] *)
      one s
    else 
      let (q, x) = Arith.mono_of m in            
	if Mpa.Q.is_zero q then            (* [ (q*x) / b = q * (x/ b)] *)
	  zero s
	else if Mpa.Q.is_one q then
	  lookup s (cancelled_div (x, b))
	else 
	  match x with
	    | App(Arith(Num(p)), []) ->    (* [ (q*p) / b = (q*p) * 1/b] *)
		multq s (Q.mult q p) (inv s b)
	    | _ -> 
		multq s q (cancelled_div (x, b))

  and cancelled_div (a, b) =
    let product_of a = match a with
      | App(Builtin(Mult), xl) -> xl
      | _ -> [a]
    in
    let al = product_of a in
    let bl = product_of b in
      let (a', b') = cancel (one s, one s) (al, bl) in
	lookup s (mk_app Sym.div [a'; b'])
	
  and cancel ((acc1, acc2) as acc) =
    function
    | [], [] -> 
	acc
    | [], l2 -> 
	(acc1, multl s l2)
    | l1, [] -> 
	(multl s l1, acc2)
    | ((a :: al) as l1), ((b :: bl) as l2) ->  
	let (n, x) = expt_of a in
	let (m, y) = expt_of b in
	let compare = Term.cmp x y in
	  if compare = 0 then
	    let acc' = match Q.cmp n m with
	      | Q.Equal -> acc 
	      | Q.Less -> 
		  let k = num s (Q.sub m n) in
		    (acc1, mult s (expt s k x, acc2))
	      | Q.Greater -> 
		  let k = num s (Q.sub n m) in
		    (mult s (expt s k x, acc1), acc2)
	    in
	      cancel acc' (al, bl)
	  else if compare > 0 then 
	    cancel (acc1, mult s (b, acc2)) (l1, bl)
	  else (* compare < 0 *)
	    cancel (mult s (a, acc1), acc2) (al, l2)	  
  in  
    match Solution.find s.a a, Solution.find s.a b with
      | a, App(Arith(Num(q)), []) when Q.is_one q ->
	  a
      | a, App(Arith(Num(q)), []) when not(Q.is_zero q) ->
	  multq s (Q.inv q) a
      | App(Builtin(Div), [a1; a2]), b ->
	  div s (a1, mult s (a2, b))
      | a, App(Builtin(Div), [b1; b2]) ->
	  mult s (a, div s (b2, b1))
      | a, b ->
	  let ml = Arith.monomials a in
	    addl s (mapl (fun m -> divm (m, b)) ml)
 

and sin s a =
  Trace.msg "rewrite" "Sin" a Term.pp;
  match Solution.find s.a a with
    | App(Arith(Add), [x; y]) ->
	let a = mult s (sin s x, cos s y) in
	let b = mult s (cos s x, sin s y) in
	  add s a b
    | _ ->
	lookup s (Term.mk_app Sym.sin [a])

and cos s a = 
  Trace.msg "rewrite" "Cos" a Term.pp;
  lookup s (Term.mk_app Sym.cos [a])

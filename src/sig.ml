
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
open Theories
(*i*)

(*s Reducing patterns of the form [select(update(a,i,x), j)]
  according to the equations
     [i = j => select(update(a,i,x), j) = x]
     [i <> j => select(update(a,i,x),j) = select(a,j)] 
 *)

let select s (upd, j) =
  let simplify upd =                     (* match [select(update(a,i,x), j)] *) 
    match find U s upd with               (* s.t. [update(a,i,x)] is equal to *)
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
	Not_found -> mk_app Sym.select [upd; j]    (*s add lookup. *)


let update s (a, j, y) = 
   let simplify a =                  (* search for [update(b, i, x) equal *)
     match find U s a with           (* to [a] in [s] and [i = j] in [s]. *)
       | App(Builtin(Update), [b; i; x]) 
	   when Term.eq i j -> Some(Term.mk_app Sym.update [b; i; y])
       | _ -> None
   in
   lookup s
     (try
	choose s simplify a
      with
	  Not_found -> Term.mk_app Sym.update [a; j; y])
	
 
let rec add s a b =
  let a' = find A s a 
  and b' = find A s b in
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
  Trace.func "rewrite" "multq" (Pretty.infix Mpa.Q.pp "*" Term.pp) Term.pp 
    (multq_ s)
    (q, a)
  
and multq_ s (q, a) =
  lookup s (Arith.mk_multq q (find A s a))

and mult s =
  Trace.func "rewrite" "mult" (Pretty.infix Term.pp "*" Term.pp) Term.pp
    (mult_ s)

and mult_ s (a, b) = 
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
      let a' = finda s a and b' = finda s b in
	match a', b' with
	  | App(Arith(Num(q)), []), App(Arith(Num(p)), []) ->
	      num s (Q.mult q p)
	  | _, App(Arith(Num(p)), []) ->
	      multq s p a'
	  | App(Arith(Num(q)), []), _ -> 
	      multq s q b'
	  | App(Arith(Multq(q)), [x]), _ ->
	      multq s q (mult s (x, b'))
	  | _, App(Arith(Multq(p)), [y]) ->
	      multq s p (mult s (y, a'))
	  | App(Builtin(Div), [a1; a2]), _ ->      (* [(a1 / a2) * b = (a1 * b) / a2] *) 
	      div s (mult s (a1, b), a2)
	  | _, App(Builtin(Div), [b1; b2]) ->     (* [a * (b1 / b2) = (a * b1) / b2] *)
	      div s (mult s (a, b1), b2)
	  | App(Arith(Add), xl), _ ->    (* [(x1+...+xn) * b = x1*b+...+xn*b] *)
	      let xl' = mapl (fun x -> finda s (mult s (x, b))) xl in
		lookup s (Arith.mk_addl xl')
	  | _,  App(Arith(Add), yl) ->   (* [a * (y1+...+yn) = y1*a+...+yn*a] *)
	      let yl' = mapl (fun y -> finda s (mult s (y, a))) yl in
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
	      expt s (Arith.mk_incr (finda s m)) y
	  | _ ->
	      of_list (multl [a] [b])

and finda s a =  (* only lookup in uninterpreted part! *)
  try
    let b = apply U s a in
      match b with
	| App(f, _) when Sym.is_nonlin f -> b
	| _ ->  a
      with
	  Not_found -> a 
 

and multl s =
  Trace.func "rewrite"  "multl" (Pretty.infixl Term.pp "*") Term.pp  
    (multl_ s)

and multl_ s al =
  match al with
    | [] -> one s
    | [x] -> x
    | [x; y] -> mult s (x, y)
    | x :: y :: zl -> multl_ s (mult s (x, y) :: zl)


and expt s n a =
  Trace.func "rewrite" "expt"
    (fun fmt (n, a) -> Pretty.infix Term.pp "^" Term.pp fmt (a, n))
    Term.pp
    (expt_ s)
    (n, a)


and expt_ s (n, a) =
  let n' = finda s n and a' = finda s a in
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
	  expt s (lookup s (Arith.mk_add n' (finda s m))) x
      | _, App(Builtin(Mult), xl) ->      (* [(x1 *...* xk)^n = x1^n *...xk^n] *)
	  multl s (mapl (expt s n) xl)
      | _, App(Builtin(Div), [x; y]) ->             (* [(x / y)^n = x^n / y^n] *)
	  div s (expt s n x, expt s n y)
      | _ ->
	  lookup s (Term.mk_app Sym.expt [n; a])

and inv s =
  Trace.func "rewrite" "inv" Term.pp Term.pp
    (inv_ s)


and inv_ s a = 
  match finda s a with
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

and div s =
  Trace.func "div"  "div" (Pretty.infix Term.pp "/" Term.pp) Term.pp
    (div_ s)


and div_ s (a, b) =
  let rec divm (m, b) =
    if Term.eq m b then                    (* [ a / a = 1] *)
      one s
    else if Arith.is_one b then            (* [ a / 1 = a] *)
      a
    else if Arith.is_zero a then           (* [ 0 / b = 0]. *)
      a
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
      let (a', b') = cancel s (a, b) in
	if Arith.is_one b' then
	  lookup s a'
	else 
	  lookup s (mk_app Sym.div [a'; b'])
  in  
    match finda s a, finda s b with
      | a, App(Arith(Num(q)), []) when Q.is_one q ->    (* [a / 1 = a]. *)
	  a
      | a, App(Arith(Num(q)), []) when not(Q.is_zero q) -> (* [a/q = 1/q * a]. *) 
	  multq s (Q.inv q) a
      | App(Builtin(Div), [a1; a2]), b ->    (* [(a1/a2) / b = a1 / (a2 * b)]. *)
	  div s (a1, mult s (a2, b))
      | a, App(Builtin(Div), [b1; b2]) ->    (* [a / (b1 / b2) = a * b2 / b1] *)
	  mult s (a, div s (b2, b1))
      | a, App(Arith(Multq(p)), [y]) ->      (* [a / (p * y)] = 1/p * (a / y)]. *)
	  if Q.is_zero p then
	    lookup s (Term.mk_app Sym.div [a; zero s])
	  else 
	    multq s (Q.inv p) (div s (a, y))
      | App(Arith(Multq(q)), [x]), b ->      (* [(q*x) / b = q * (x / b)]. *)
	  multq s q (div s (x, b))
      | a, b ->                              (* [(q1*x1 + ... qn*xn) / b = *)
	  let ml = Arith.monomials a in      (* q1* (x1/b) + ... + qn* (xn/b)] *)
	    addl s (mapl (fun m -> divm (m, b)) ml)
 

and cancel s = 
  Trace.func "cancel" "Cancel"
    (Pretty.pair Term.pp Term.pp)
    (Pretty.pair Term.pp Term.pp)
    (fun (a, b) ->
       let product_of = function
	 | App(Builtin(Mult), xl) -> xl
	 | a -> [a]
       in
       let rec loop ((acc1, acc2) as acc) =
	 function
	   | [], [] -> 
	       acc
	   | [], l2 -> 
	       (acc1, multl s (acc2 :: l2))
	   | l1, [] -> 
	       (multl s (acc1 :: l1), acc2)
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
		     loop acc' (al, bl)
		 else if compare > 0 then 
		   loop (acc1, mult s (b, acc2)) (l1, bl)
		 else (* compare < 0 *)
		   loop (mult s (a, acc1), acc2) (al, l2)
       in
	 loop (one s, one s) (product_of a, product_of b))


(*s Unsigned interpretation. *)

let rec unsigned s a =
  let interp_of b =
      Bitv.fold_right 
	(fun x acc -> if x then 2 * acc + 1 else 2 * acc) 
	b 0
  in
    match find BV s a with 
      | App(Bv(Const(b)), []) ->
	  num s (Q.of_int (interp_of b))
      | App(Bv(Conc(n, m)), [x; y]) ->
	  let ux = find A s (unsigned s x) in
	  let uy = find A s (unsigned s y) in
	  let two_expt_m = Q.of_z (Z.expt (Z.of_int 2) m) in
	    add s (multq s two_expt_m ux) uy
      | _ -> 
	  lookup s (Term.mk_app Sym.unsigned [a])



let apply s r a b =
  lookup s (mk_app (Sym.apply r) [a; b])

let lambda s i a =
  lookup s (mk_app (Sym.Builtin(Sym.Lambda(i))) [a])


let sigma s f l =
  match f, l with
    | Unsigned, [x] -> unsigned s x
    | Select, [x; y] -> select s (x, y)
    | Update, [x; y; z] -> update s (x, y, z)
    | Mult, xl -> multl s xl
    | Div, [x; y] -> div s (x, y)
    | Expt, [x; y] -> expt s x y
    | Apply(r), [x; y]  -> apply s r x y
    | Lambda(i), [y] -> lambda s i y
    | _ -> mk_app (Builtin(f)) l

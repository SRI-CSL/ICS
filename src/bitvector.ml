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

open Mpa

(** {6 Recognizers} *)

let is_pure = Term.is_pure Th.bv

let is_interp = function
  | Term.App(sym, _, _) -> Sym.theory_of sym = Th.bv
  | _ -> false


(** {6 Destructors} *)

let d_interp = function
  | Term.App(sym, l, _) -> (Sym.Bv.get sym, l)
  | _ -> raise Not_found

let d_const a =
  match d_interp a with
    | Sym.Const(c), [] -> c
    | _ -> raise Not_found

let d_conc a = 
  match d_interp a with
    | Sym.Conc(n, m), [x; y] -> (n, m, x, y)
    | _ -> raise Not_found

let d_sub a = 
  match d_interp a with
    | Sym.Sub(n, i, j), [x] -> (n, i, j , x)
    | _ -> raise Not_found

let to_option f a =
  try let b = f a in Some(b) with Not_found -> None

let width a =
  try
    let op, _ = d_interp a in
      Some(Sym.Bv.width op)
  with
      Not_found -> None


(** {6 Constant bitvectors} *)

let mk_const c = Term.App.mk_const(Sym.Bv.const c)

let mk_zero n = mk_const(Bitv.create n false)
let mk_one n = mk_const(Bitv.create n true)
let mk_eps = mk_const(Bitv.from_string "")

let is_const c a =
  try Bitv.equal (d_const a) c with Not_found -> false

let is_eps a =
  try Bitv.length (d_const a) = 0 with Not_found -> false

let is_zero a =
  try Bitv.all_zeros (d_const a) with Not_found -> false
 
let is_one a =
  try Bitv.all_ones (d_const a) with Not_found -> false


(** Creating fresh bitvector variables for solver. 
 The index variable are always reset to the current value
 when solver is called. *)

let mk_fresh n =
  assert (n >= 0);
  if n = 0 then mk_eps else 
    Term.Var.mk_fresh Th.bv None None


(** {6 Iterators} *)

let rec iter f a =
  try
    (match d_interp a with
      | Sym.Const(_), [] -> ()
      | Sym.Sub(_, _, _), [x] -> iter f x
      | Sym.Conc(n, m), [x; y] -> iter f x; iter f y
      | _ -> f a)
  with
      Not_found -> f a

let rec fold f a e =
  try
    (match d_interp a with
       | Sym.Const(_), [] -> e
       | Sym.Sub(_,_,_), [x] -> fold f x e
       | Sym.Conc(_,_), [x; y] -> fold f x (fold f y e)
       | _ -> assert false)
  with
      Not_found -> f a e

let rec exists p a =
  try
    (match d_interp a with
       | Sym.Sub _, [b] -> exists p b
       | Sym.Conc(n,m), [b1; b2] -> (exists p b1) || (exists p b2)
       | _ ->  false)
  with 
      Not_found -> p a


(** Term constructors. *)

let rec mk_sub n i j a =
  assert (0 <= i && j < n && n >= 0);
  if i = 0 && j = n - 1 then 
    a 
  else if j < i then
    mk_eps
  else 
    try
      (match d_interp a with
	 | Sym.Const(b), [] -> 
	     mk_const (Bitv.sub b i (j - i + 1))
	 | Sym.Sub(m,k,l), [x] ->
	     mk_sub m (k + i) (k + j) x
	 | Sym.Conc(n,m), [x; y] ->
	     if j < n then
	       mk_sub n i j x
	     else if n <= i then
	       mk_sub m (i - n) (j - n) y
	     else 
	       (assert(i < n && n <= j);
		let t = mk_sub n i (n - 1) x in
		let u = mk_sub m 0 (j - n) y in
		  mk_conc (n - i) (j - n + 1) t u)
	 | _ ->
	     failwith "Bv.mk_sub: ill-formed expression")
    with
	Not_found -> 
	  Term.App.mk_app (Sym.Bv.sub n i j) [a]
	
and mk_conc n m a b =
  assert (0 <= n && 0 <= m);
  match n = 0, m = 0 with
    | true, true -> mk_eps
    | true, false -> b
    | false, true -> a
    | false, false ->
	(try
	   merge n m a b
	 with
	     Not_found -> Term.App.mk_app (Sym.Bv.conc n m) [a; b])

and merge n m a b =
  match to_option d_interp a, to_option d_interp b with
    | _, Some(Sym.Conc(m1,m2), [b1;b2]) -> 
	mk_conc (n + m1) m2 (mk_conc n m1 a b1) b2
    | Some(Sym.Const(c),[]), Some(Sym.Const(d),[]) -> 
	mk_const (Bitv.append c d)
    | Some(Sym.Sub(n,i,j),[x]), Some(Sym.Sub(m,j',k), [y])
	when j' = j + 1 && Term.eq x y ->
	assert(n = m);
	  mk_sub n i k x
    | Some(Sym.Conc(m1, m2), [b1; b2]), Some(Sym.Const(d), []) ->
	let c = d_const b2 in
	let n = Bitv.length d in
	  mk_conc m1 (m2 + n) b1 (mk_const (Bitv.append c d))
    | _ -> 
	raise Not_found
 
and mk_conc3 n m k a b c =
  mk_conc n (m + k) a (mk_conc m k b c)

and cut n i a =
  (mk_sub n 0 (i - 1) a, mk_sub n i (n - 1) a)


(** Mapping over bitvector terms. *)

let map f =
  let rec loop a =
    try
      (match d_interp a with
	 | Sym.Const(_), [] ->
	     a
	 | Sym.Sub(n,i,j), [x] -> 
	     let x' = loop x in
	       if x == x' then a else mk_sub n i j x'
	 | Sym.Conc(n,m), [x;y] -> 
	     let x' = loop x and y' = loop y in
	       if x == x' && y == y' then a else mk_conc n m x' y'
	 | _ -> 
	     assert false)
    with
	Not_found -> f a
  in
  loop


let apply (x, b) = 
  map (fun y -> if Term.eq x y then b else y)



(** Sigmatizing an expression. *)

let sigma op l =
  match op, l with
    | Sym.Const(c), [] -> mk_const c
    | Sym.Sub(n,i,j), [x] -> mk_sub n i j x
    | Sym.Conc(n,m), [x;y] ->  mk_conc n m x y
    | _ -> failwith "Bv.sigma: ill-formed expression"

 
(* n-ary concatenation of some concatenation normal form *)

let rec mk_iterate n b = function
  | 0 -> mk_eps 
  | 1 -> b
  | k -> mk_conc n (n * (k-1)) b (mk_iterate n b (k-1))


(* Flattening out concatenations. The result is a list of equivalent
 equalities not containing any concatenations. *)

let decompose e =
  let rec loop acc = function
    | [] -> acc
    | (a,b) :: el when Term.eq a b ->
	loop acc el
    | (a,b) :: el -> 
	let (acc',el') = 
	  match to_option d_conc a, to_option d_conc b  with   
	    | Some(n1,m1,x1,y1), Some(n2,m2,x2,y2) ->
		if n1 = n2 then
		  (acc, ((x1,x2) :: (y1,y2) :: el))
		else if n1 < n2 then
		  let (x21,x22) = cut n2 n1 x2 in
		  let e1 = (x1,x21) in
		  let e2 = (y1, mk_conc (n2-n1) m2 x22 y2) in
		  (acc, (e1 :: (e2 :: el)))
		else (* n1 > n2 *)
		  let (x11,x12) = cut n1 n2 x1 in
		  let e1 = (x11,x2) in
		  let e2 = (mk_conc (n1-n2) m1 x12 y1, y2) in
		  (acc, (e1 :: e2 :: el))
	    | Some(n,m,x,y), None ->
		let e1 = (mk_sub (n+m) 0 (n-1) b, x) in
		let e2 = (mk_sub (n+m) n (n+m-1) b, y) in
		(acc, (e1 :: e2 :: el))
	    | None, Some(n,m,x,y) ->
		let e1 = (mk_sub (n+m) 0 (n-1) a, x) in
		let e2 = (mk_sub (n+m) n (n+m-1) a, y) in
		(acc, (e1 :: e2 :: el))
	    | None, None ->
		(((a, b) :: acc), el)
	in
	loop acc' el'
  in
  loop [] [e] 


(** Adding a solved pair [a |-> b] to the list of solved forms [sl],
 and propagating this new binding to the unsolved equalities [el] and 
 the rhs of [sl]. It also makes sure that fresh variables [a] are never
 added to [sl] but only propagated. *)

let rec add a b (el, sl) =
  assert(not(is_interp a));
  if Term.eq a b then 
    (el, sl)
  else if inconsistent a b then
    raise Exc.Inconsistent
  else 
    match is_fresh_bv_var a, is_fresh_bv_var b with 
      | false, false ->
	  (inste el a b, (a, b) :: insts sl a b)
      | true, true -> 
	  (inste el a b, insts sl a b)
      | true, false -> 
	  if is_interp b then
	    (inste el a b, insts sl a b)
	  else
	    (inste el b a, insts sl b a)
      | false, true ->
	  (inste el b a, insts sl b a) 

and is_fresh_bv_var _ = false    
     
and inste el a b = 
  List.map (fun (x,y) -> (apply1 x a b, apply1 y a b)) el

and insts sl a b =
  List.map (fun (x,y) -> (x, apply1 y a b)) sl 
  
and inconsistent a b =
  match width a, width b with
    | Some(n), Some(m) -> n <> m
    | _ -> false

and apply1 a x b =      (* substitute [x] by [b] in [a]. *)
  map (fun y -> if Term.eq x y then b else y) a


(** Toplevel solver. *)

let rec solve e  =
  solvel ([e], [])
  
and solvel (el,sl) =
  match el with
    | [] -> sl
    | (a, b) :: el when Term.eq a b ->
	solvel (el,sl)
    | (a, b) :: el ->
	(match to_option d_interp a, to_option d_interp b  with   
	   | None, Some _ when not(Term.subterm a b) ->  (* Check if solved. *)
	       solvel (add a b (el, sl))
	   | Some _, None  when not(Term.subterm b a) ->
	       solvel (add b a (el, sl))
	   | Some(Sym.Conc _, _), _                    (* Decomposition of [conc] *)
	   | _, Some(Sym.Conc _, _) ->
	       solvel (decompose (a,b) @ el, sl)
	   | Some(Sym.Const(c), []), Some(Sym.Const(d), []) ->
	       if Pervasives.compare c d = 0 then 
		 solvel (el,sl)
	       else 
		 raise Exc.Inconsistent
	   | Some(Sym.Sub(n,i,j),[x]), Some(Sym.Sub(m,k,l),[y]) when Term.eq x y ->
	       assert(n = m);
	       let (x, b) = solve_sub_sub x n i j k l in
		 solvel (add x b (el,sl))
	   | Some(Sym.Sub(n,i,j),[x]), _ ->
	       assert(n-j-1 >= 0);
	       assert(i >= 0);
	       let b' = 
		 mk_conc3 i (j-i+1) (n-j-1) 
		   (mk_fresh i) b (mk_fresh (n-j-1)) 
	       in
		 solvel (add x b' (el,sl))
	   | _, Some(Sym.Sub(n,i,j), [x]) -> 
               assert(n-j-1 >= 0); 
	       assert(i >= 0);
	       let b' = mk_conc3 i (j-i+1) (n-j-1) (mk_fresh i) 
			  b (mk_fresh (n-j-1)) in
	       solvel (add x b' (el,sl))
	   | _ ->
	       let a, b = Term.orient(a,b) in
		 solvel (add a b (el,sl)))

(* Solving [xn[i:j] = xn[k:l]] *)
and solve_sub_sub x n i j k l =      
  assert (n >= 0 && i < k && j-i = l-k);
  let lhs = 
    mk_sub n i l x 
  in
  let rhs =
    if (l-i+1) mod (k-i) = 0 then
      let a = mk_fresh (k-i) in
      mk_iterate (k-i) a ((l-i+1)/(k-i))
    else
      let h = (l-i+1) mod (k-i) in
      let h' = k-i-h in
      let a = mk_fresh h in
      let b = mk_fresh h' in
      let nc = (l-i-h+1)/(k-i) in
      let c = mk_iterate (h + h') (mk_conc h' h b a) nc in
      mk_conc h (nc * (h' + h)) a c
  in
  (lhs, rhs)

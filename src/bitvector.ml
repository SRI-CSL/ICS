
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

(*i*)
open Sym
open Term
open Format
open Mpa
(*i*)


let is_bvsym f =
  match Sym.destruct f with
    | Sym.Interp(Sym.Bv _) -> true
    | _ -> false

let d_bvsym f =
  match Sym.destruct f with
    | Sym.Interp(Sym.Bv(op)) -> Some(op)
    | _ -> None

let is_interp a =
  not (Term.is_var a) &&
  is_bvsym (Term.sym_of a)

let d_interp a =
  if is_var a then None else
  match d_bvsym (Term.sym_of a) with
    | Some(op) -> Some(op, Term.args_of a)
    | _ -> None

(* Constant bitvectors *)

let mk_const c = 
  let f = Sym.mk_bv_const c in
  Term.mk_const f

let mk_eps = 
  mk_const(Bitv.from_string "")

let is_eps a =
  match d_interp a with
    | Some(Const b, []) -> Bitv.length b = 0
    | _ -> false

let mk_zero n = 
  assert(n > 0);
  mk_const(Bitv.create n false)

let is_zero a =
  match d_interp a with
    | Some(Const b, []) -> Bitv.all_zeros b
    | _ -> false

let mk_one n =
  mk_const(Bitv.create n true)

let is_one a =
  match d_interp a with
    | Some(Const b, []) -> Bitv.all_ones b
    | _ -> false


(*s Creating fresh bitvector variables. *)

let freshvars = ref Term.Map.empty
let _ = Tools.add_at_reset (fun () -> freshvars := Term.Map.empty)

let mk_fresh =
  let name = Name.of_string "bv" in
  fun n ->
    assert (n >= 0);
    if n = 0 then 
      mk_eps
    else 
      let x = Term.mk_fresh_var name None in
      freshvars := Term.Map.add x n !freshvars;
      x

let is_fresh a = Term.Map.mem a !freshvars

(*s Bitvector symbols *)

let width a =
  try
    Some(Term.Map.find a !freshvars)
  with
      Not_found ->
	if Term.is_var a then None else
	  Sym.width (Term.sym_of a)


let iter f a =
  match d_interp a with
    | Some(Sym.Const(_), []) -> ()
    | Some(Sym.Sub(_,_,_), [x]) -> iter f x
    | Some(Sym.Conc(n,m), [x;y]) -> iter f x; iter f y
    | Some(Sym.Bitwise(n), [x;y;z]) -> iter f x; iter f y; iter f z
    | _ -> f a


(*s Fold functional. *)

let rec fold f a e =
  match d_interp a with
    | Some(Sym.Const(_), []) -> e
    | Some(Sym.Sub(_,_,_), [x]) -> fold f x e
    | Some(Sym.Conc(_,_), [x;y]) -> fold f x (fold f y e)
    | Some(Sym.Bitwise(_), [x;y;z]) -> fold f x (fold f y (fold f z e))
    | _ -> f a e


let is_bitwise a =
  match d_interp a with
    | Some(Bitwise _, [_;_;_]) -> true
    | _ -> false

let d_bitwise a =
  match d_interp a with
    | Some(Bitwise(n),[x;y;z]) ->
	Some(n,x,y,z)
    | _ -> 
	None

let d_conc a = 
  match d_interp a with
    | Some(Conc(n,m), [x;y]) -> Some(n,m,x,y)
    | _ -> None

let d_const a = 
  match d_interp a with
    | Some(Const(c), []) -> Some(c)
    | _ -> None

let d_sub a = 
  match d_interp a with
    | Some(Sub(n,i,j),[x]) -> Some(n,i,j,x)
    | _ -> None


(*s Building up Bitvector BDDs *)

let is_bvbdd a =
  is_zero a || is_one a || is_bitwise a

let cofactors x a =
  match d_interp a with
    | Some(Bitwise _, [y;pos;neg]) 
	when Term.eq x y -> 
	  (pos,neg)
    | _ -> 
	(a,a)
   
let topvar x s2 s3 =
  let max x y = if (x <<< y) then y else x in
  match d_bitwise s2, d_bitwise s3 with
    | Some(_,y,_,_), Some(_,z,_,_) -> max x (max y z)
    | Some(_,y,_,_), None -> max x y
    | None, Some(_,z,_,_) -> max x z
    | None, None -> x
    
module H3 = Hashtbl.Make(
  struct
    type t = Term.t * Term.t * Term.t
    let equal (a1,a2,a3) (b1,b2,b3) = 
      Term.eq a1 b1 && Term.eq a2 b2 && Term.eq a3 b3
    let hash = Hashtbl.hash
  end)
 
let ht = H3.create 17

let rec build n s3 =
  try
    H3.find ht s3
  with Not_found ->
    let b = build_fun n s3 in
    H3.add ht s3 b; b 

and build_fun n (s1,s2,s3) =
  if Term.eq s2 s3 then s2
  else if is_one s2 && is_zero s3 then s1
  else if is_one s1 then s2
  else if is_zero s1 then s3
  else match d_bitwise s1 with
    | Some(_,y,_,_) ->
	let x = topvar y s2 s3 in
	let (pos1,neg1) = cofactors x s1 in
	let (pos2,neg2) = cofactors x s2 in
	let (pos3,neg3) = cofactors x s3 in
	let pos = build n (pos1,pos2,pos3) in
	let neg = build n (neg1,neg2,neg3) in
	if Term.eq pos neg then pos else Term.mk_app (Sym.mk_bv_bitwise n) [x;pos;neg]
    | None ->
	Term.mk_app (Sym.mk_bv_bitwise n) [s1;s2;s3]

(*s Term constructors. *)

let rec mk_sub n i j a =
  assert (0 <= i && j < n && n >= 0);
  if i = 0 && j = n-1 then 
    a 
  else if j < i then
    mk_eps
  else 
    match d_interp a with
      | Some(Sym.Const(b), []) -> 
	  mk_const(Bitv.sub b i (j-i+1))
      | Some(Sym.Sub(m,k,l), [x]) ->
	  mk_sub m (k+i) (k+j) x
      | Some(Sym.Conc(n,m), [x;y]) ->
	  if j < n then
	    mk_sub n i j x
	  else if n <= i then
	    mk_sub m (i-n) (j-n) y
	  else 
	    (assert(i < n && n <= j);
            let t = mk_sub n i (n-1) x in
	    let u = mk_sub m 0 (j-n) y in
	    mk_conc (n-i) (j-n+1) t u)
      | Some(Sym.Bitwise(n), [x;y;z]) ->
	  mk_bitwise (j-i+1) (mk_sub n i j x) (mk_sub n i j y) (mk_sub n i j z) 
      | None ->
	  Term.mk_app (Sym.mk_bv_sub n i j) [a]
      | Some _ ->
	  failwith "Bv.mk_sub: ill-formed expression"
	
and mk_conc n m a b =
  assert (0 <= n && 0 <= m);
  match n = 0, m = 0 with
    | true, true -> mk_eps
    | true, false -> b
    | false, true -> a
    | false, false ->
	(match merge n m a b with
	   | None -> Term.mk_app (Sym.mk_bv_conc n m) [a;b]
	   | Some(c) -> c)

and merge n m a b =
  match d_interp a, d_interp b with
    | _, Some(Conc(m1,m2), [b1;b2]) -> 
	Some(mk_conc (n + m1) m2 (mk_conc n m1 a b1) b2)
    | Some(Const(c),[]), Some(Const(d),[]) -> 
	Some(mk_const (Bitv.append c d))
    | Some(Sub(n,i,j),[x]), Some(Sub(m,j',k), [y])
	when j' = j + 1 && Term.eq x y ->
	  assert(n = m);
	  Some(mk_sub n i k x)
    | Some(Bitwise(n1),[x1;y1;z1]), Some(Bitwise(n2),[x2;y2;z2]) ->
	(match merge n1 n2 x1 x2 with
	   | None -> None
	   | Some(x) ->
	       (match merge n1 n2 y1 y2 with
		  | None -> None
		  | Some(y) ->
		      (match merge n1 n2 z1 z2 with
			 | None -> None
			 | Some(z) -> Some(mk_bitwise (n1+n2) x y z))))
    | _ -> 
	None
 

and mk_conc3 n m k a b c =
  mk_conc n (m + k) a (mk_conc m k b c)

and mk_bitwise n a b c =
  assert (n >= 0);
  if n = 0 then 
    mk_eps
  else 
    match d_const a, d_const b, d_const c with
      | Some(c1), Some(c2), Some(c3) ->
	  mk_const (Bitv.bw_or (Bitv.bw_and c1 c2) (Bitv.bw_and (Bitv.bw_not c1) c3))
      | _ ->
	  (match d_conc a, d_conc b, d_conc c with
	     | Some(n1,n2,a1,a2), _, _ ->
		 assert(n = n1 + n2);
		 let b1,b2 = cut n n1 b in
		 let c1,c2 = cut n n1 c in 
		 mk_conc n1 n2 (mk_bitwise n1 a1 b1 c1) (mk_bitwise n2 a2 b2 c2)
	     | _, Some(n1,n2,b1,b2), _ ->
		 assert(n = n1 + n2);
		 let a1,a2 = cut n n1 a in
		 let c1,c2 = cut n n1 c in 
		 mk_conc n1 n2 (mk_bitwise n1 a1 b1 c1) (mk_bitwise n2 a2 b2 c2)
	     | _, _, Some(n1,n2,c1,c2) ->
		 assert(n = n1 + n2);
		 let a1,a2 = cut n n1 a in
		 let b1,b2 = cut n n1 b in 
		 mk_conc n1 n2 (mk_bitwise n1 a1 b1 c1) (mk_bitwise n2 a2 b2 c2)
	     | _ ->
		 drop (build n (lift n a, lift n b, lift n c)))

and lift n a =
  if is_bvbdd a then 
    a 
  else
    let f = Sym.mk_bv_bitwise n in
    Term.mk_app f [a;mk_one n;mk_zero n]

and drop a =
 match d_bitwise a with
   | Some(_,b1,b2,b3) when is_one b2 && is_zero b3 -> b1
   | _ -> a

and cut n i a =
  (mk_sub n 0 (i - 1) a, mk_sub n i (n - 1) a)


(*s Derived bitwise constructors. *)

let mk_bwconj n a b = mk_bitwise n a b (mk_zero n)
let mk_bwdisj n a b = mk_bitwise n a (mk_one n) b
let mk_bwneg n a = mk_bitwise n a (mk_zero n) (mk_one n)
let mk_bwimp n a1 a2 = mk_bitwise n a1 a2 (mk_one n)
let mk_bwiff n a1 a2 = mk_bitwise n a1 a2 (mk_bwneg n a2)

(*s Mapping over bitvector terms. *)

let map f =
  let rec loop a =
    match d_interp a with
      | Some(Sym.Const(_), []) ->
	  a
      | Some(Sym.Sub(n,i,j), [x]) -> 
	  mk_sub n i j (loop x)
      | Some(Sym.Conc(n,m), [x;y]) -> 
	  mk_conc n m (loop x) (loop y)
      | Some(Sym.Bitwise(n), [x;y;z]) -> 
	  mk_bitwise n (loop x) (loop y) (loop z)
      | None -> 
	  f a
      | Some _ -> 
	  assert false
  in
  loop

(*s Does term [a] occur interpreted in [b]. *)

let rec occurs a b =
  let rec loop x =
    (Term.eq x a)
    || (match d_interp x with
	  | Some(op,l) ->
	      (match op, l with
		 | Sym.Const(_), [] -> false
		 | Sym.Sub _, [x] -> loop x
		 | Sym.Conc(n,m), [x;y] -> (loop x) || (loop y)
		 | Sym.Bitwise(n), [x;y;z] -> (loop x) || (loop y) || (loop z)
		 | _ ->  failwith "Bv.map: ill-formed expression")
	  | None -> false)
  in
  loop b

(*s Sigmatizing an expression. *)

let sigma op l =
  match op, l with
    | Sym.Const(c), [] -> mk_const c
    | Sym.Sub(n,i,j), [x] -> mk_sub n i j x
    | Sym.Conc(n,m), [x;y] ->  mk_conc n m x y
    | Sym.Bitwise(n), [x;y;z] -> mk_bitwise n x y z
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
	  match d_conc a, d_conc b  with   
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


(*s Solving is based on the equation
 [ite(x,p,n) = (p or n) and exists delta. x = (p and (n => delta))] *)

and solve_bitwise n (a,b) =
  assert(n >= 0);
  let s = mk_bwiff n a b in
  let rec triangular_solve s e =
    match d_bitwise s with
      | Some (_,x,pos,neg) ->
	  if is_one pos && is_zero neg then          (* poslit *)
	    (x, pos) :: e
	  else if is_zero pos && is_one neg then     (* neglit *)
	    (x, pos) :: e
	  else
	    let t' = mk_bwconj n pos (mk_bwimp n neg (mk_fresh n)) in
	    let e' = (x,t') :: e in
	    let s' = mk_bwdisj n pos neg in
	    if is_zero s' then
	      raise Exc.Inconsistent
	    else if is_one s' then
	      e'
	    else if is_bitwise s' then
	      triangular_solve s' e'
	    else
	      (s', mk_one n) :: e'
      | None ->
	  (s, mk_one n) :: e
  in
  if is_zero s then
    raise Exc.Inconsistent
  else if is_one s then
    []
  else
    triangular_solve s []


(*s Adding a solved pair [a |-> b] to the list of solved forms [sl],
 and propagating this new binding to the unsolved equalities [el] and 
 the rhs of [sl]. It also makes sure that fresh variables [a] are never
 added to [sl] but only propagated. *)

let rec add a b (el,sl) =
  assert(not(is_interp a));
  if Term.eq a b then 
    (el,sl)
  else if inconsistent a b then
    raise Exc.Inconsistent
  else 
    match is_fresh a, is_fresh b with 
      | false, false ->
	  (inste el a b, (a,b) :: insts sl a b)
      | true, true -> 
	  (inste el a b, insts sl a b)
      | true, false -> 
	  if is_interp b then
	    (inste el a b, insts sl a b)
	  else
	    (inste el b a, insts sl b a)
      | false, true ->
	  (inste el b a, insts sl b a)  
     
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


(*s Toplevel solver. *)

let rec solve e =
  solvel ([e], [])
  
and solvel (el,sl) =
  match el with
    | [] -> sl
    | (a,b) :: el when Term.eq a b ->
	solvel (el,sl)
    | (a,b) :: el ->
	(match d_interp a, d_interp b  with   
	   | None, Some _ when not(occurs a b) ->      (* Check if solved. *)
	       solvel (add a b (el,sl))
	   | Some _, None  when not(occurs b a) ->
	       solvel (add b a (el,sl))
	   | Some(Conc _, _), _                  (* Decomposition of conc. *)
	   | _, Some(Conc _, _) ->
	       solvel (decompose (a,b) @ el, sl)
	   | Some(Bitwise(n), _), _ ->            (* Solve bitwise ops. *)
	       solvel (solve_bitwise n (a,b) @ el, sl)
	   | (None | Some(Const _, _)), Some(Bitwise(m), _) ->
	       solvel (solve_bitwise m (a,b) @ el, sl)
	   | Some(Const(c), []), Some(Const(d), []) ->
	       if Pervasives.compare c d = 0 then 
		 solvel (el,sl)
	       else 
		 raise Exc.Inconsistent
	   | Some(Sub(n,i,j),[x]), Some(Sub(m,k,l),[y]) when Term.eq x y ->
	       assert(n = m);
	       let (x,b) = solve_sub_sub x n i j k l in
	       solvel (add x b (el,sl))
	   | Some(Sub(n,i,j),[x]), _ ->
	       assert(n-j-1 >= 0);
	       assert(i >= 0);
	       let b' = mk_conc3 i (j-i+1) (n-j-1) 
			  (mk_fresh i) b (mk_fresh (n-j-1)) in
	       solvel (add x b' (el,sl))
	   | _, Some(Sub(n,i,j), [x]) -> 
               assert(n-j-1 >= 0); 
	       assert(i >= 0);
	       let b' = mk_conc3 i (j-i+1) (n-j-1) (mk_fresh i) 
			  b (mk_fresh (n-j-1)) in
	       solvel (add x b' (el,sl))
	   | _ ->
	       let a,b = Term.orient(a,b) in
	       solvel (add a b (el,sl)))

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

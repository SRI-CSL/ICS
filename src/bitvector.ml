(*
 * The contents of this file are subject to the ICS(TM) Community Research
 * License Version 2.0 (the ``License''); you may not use this file except in
 * compliance with the License. You may obtain a copy of the License at
 * http://www.icansolve.com/license.html.  Software distributed under the
 * License is distributed on an ``AS IS'' basis, WITHOUT WARRANTY OF ANY
 * KIND, either express or implied. See the License for the specific language
 * governing rights and limitations under the License.  The Licensed Software
 * is Copyright (c) SRI International 2003, 2004.  All rights reserved.
 * ``ICS'' is a trademark of SRI International, a California nonprofit public
 * benefit corporation.
 *)

(** Operations on bitvectors. *)

open Mpa

let bitv2nat b =
  Bitv.fold_right 
    (fun x acc -> 
       if x then 2 * acc + 1 else 2 * acc) 
    b 0

(** Unsigned bitvector of length [n] for integer [i]. Hashed. *)
let rec nat2bitv n i =
  let ht = Hashtbl.create 17 in
  let _ = Tools.add_at_reset (fun () -> Hashtbl.clear ht) in
    try
      Hashtbl.find ht (n, i)
    with
	Not_found -> 
	  let b = nat2bitv_rec n i in
	    Hashtbl.add ht (n, i) b; b
  
and nat2bitv_rec n i =
  assert(i >= 0);
  let rec loop acc i =
    if i <= 0 then acc else
      let acc' = (if i mod 2 = 0 then "0" else "1") ^ acc in
	loop acc' (i / 2)
  in
  let str = loop "" i in
  let m = String.length str in
    assert(n - m >= 0);
    let patch = String.make (n - m) '0' in
    let str' = patch ^ str in
      assert(String.length str' = n);
      Bitv.from_string str'

let is_pure = Term.is_pure Th.bv

let d_interp = function
  | Term.App(sym, l, _) -> (Sym.Bv.get sym, l)
  | _ -> raise Not_found

let is_interp a =
  try
    (match d_interp a with
       | Sym.Const _, [] -> true
       | Sym.Sub _, [_] -> true
       | Sym.Conc _, [_; _] -> true
       | _ -> false)
  with
      Not_found -> false

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
      Not_found ->
	(try Some(Term.Var.width_of a) with Not_found -> None)


(** Constant bitvectors *)
let mk_const c = Term.App.mk_const(Sym.Bv.mk_const c)

let mk_zero n = mk_const(Bitv.create n false)
let mk_one n = mk_const(Bitv.create n true)
let mk_eps () = mk_const(Bitv.from_string "")

let is_const c a =
  try Bitv.equal (d_const a) c with Not_found -> false

let is_eps a =
  try Bitv.length (d_const a) = 0 with Not_found -> false

let is_zero a =
  try Bitv.all_zeros (d_const a) with Not_found -> false
 
let is_one a =
  try Bitv.all_ones (d_const a) with Not_found -> false



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


let rec mk_sub n i j a =
  assert (0 <= i && j < n && n >= 0);
  if i = 0 && j = n - 1 then 
    a 
  else if j < i then
    mk_eps()
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
	  Term.App.mk_app (Sym.Bv.mk_sub n i j) [a]
	
and mk_conc n m a b =
  assert (0 <= n && 0 <= m);
  match n = 0, m = 0 with
    | true, true -> mk_eps()
    | true, false -> b
    | false, true -> a
    | false, false ->
	(try
	   merge n m a b
	 with
	     Not_found -> Term.App.mk_app (Sym.Bv.mk_conc n m) [a; b])

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

let sigma op l =
  match op, l with
    | Sym.Const(c), [] -> mk_const c
    | Sym.Sub(n,i,j), [x] -> mk_sub n i j x
    | Sym.Conc(n,m), [x;y] ->  mk_conc n m x y
    | _ -> failwith "Bv.sigma: ill-formed expression"

 
(** [n]-ary concatenation of some concatenation normal form *)
let rec mk_iterate n b = function
  | 0 -> mk_eps ()
  | 1 -> b
  | k -> mk_conc n (n * (k-1)) b (mk_iterate n b (k-1))


(** Flattening out concatenations. The result is a list of equivalent
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


let fresh = ref []

(** Creating fresh bitvector variables for solver. 
 The index variable are always reset to the current value
 when solver is called. *)
let mk_fresh n =
  assert (n >= 0);
  if n = 0 then mk_eps () else 
    let x = Term.Var.mk_fresh Th.bv None (Var.Cnstrnt.mk_bitvector(n)) in
      fresh := x :: !fresh;
      x

let is_fresh x =
  let eqx = Term.eq x in
    List.exists eqx !fresh


let rec solve ((a, b) as e) =
  match width a, width b with
    | Some(n), Some(m) when n <> m -> 
	raise Exc.Inconsistent
    | _ -> 
	fresh := [];         (* initialize fresh variables. *)
	solvel ([e], [])
  
and solvel (el, sl) =
  Trace.msg "bv'" "Solve" (el, sl) (Pretty.pair Term.Subst.pp Term.Subst.pp);
  match el with
    | [] -> sl
    | (a, b) :: el when Term.eq a b ->
	solvel (el,sl)
    | (a, b) :: el ->
	(match to_option d_interp a, to_option d_interp b  with   
	   | None, Some _ when not(occurs a b) ->      (* Check if solved. *)
	       solvel (add a b (el, sl))
	   | Some _, None  when not(occurs b a) ->
	       solvel (add b a (el, sl))
	   | Some(Sym.Conc _, _), _                    (* Decomposition of [conc] *)
	   | _, Some(Sym.Conc _, _) ->
	       solvel (decompose (a, b) @ el, sl)
	   | Some(Sym.Const(c), []), Some(Sym.Const(d), []) ->
	       if Pervasives.compare c d = 0 then 
		 solvel (el,sl)
	       else 
		 raise Exc.Inconsistent
	   | Some(Sym.Sub(n,i,j),[x]), Some(Sym.Sub(m,k,l),[y]) when Term.eq x y ->
	       assert(n = m);
	       let (x, b) = solve_sub_sub x n i j k l in
		 solvel (add x b (el,sl))
	   | Some(Sym.Sub(n,i,j),[x]),  Some(Sym.Const _, []) ->
	       assert(n-j-1 >= 0);
	       assert(i >= 0);
	       let b' = 
		 mk_conc3 i (j-i+1) (n-j-1) 
		   (mk_fresh i) b (mk_fresh (n-j-1)) 
	       in
		 solvel (add x b' (el,sl))
	   | Some(Sym.Const _, []), Some(Sym.Sub(n,i,j), [x]) -> 
               assert(n-j-1 >= 0); 
	       assert(i >= 0);
	       let a' = mk_conc3 i (j-i+1) (n-j-1) (mk_fresh i) 
			  a (mk_fresh (n-j-1)) in
	       solvel (add x a' (el,sl))
	   | _ ->
	       assert(not(is_interp a));
	       assert(not(is_interp b));
	       let a, b = Term.orient(a, b) in
		 solvel (add a b (el,sl)))

(* Solving [xn[i:j] = xn[k:l]] *)
and solve_sub_sub x n i j k l =  
  Trace.msg "bv'" "Solve(sub)" (mk_sub n i j x, mk_sub n k l x) (Pretty.pair Term.pp Term.pp);
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
    Trace.msg "bv'" "Solve(sub)" (lhs, rhs) (Pretty.pair Term.pp Term.pp);
    (lhs, rhs)


and occurs x a =
  assert(not(is_interp x));
  try
    let (_, bl) = d_interp a in
      List.exists (occurs x) bl
  with
      Not_found -> Term.eq x a


(** Adding a solved pair [a |-> b] to the list of solved forms [sl],
 and propagating this new binding to the unsolved equalities [el] and 
 the rhs of [sl]. It also makes sure that fresh variables [a] are never
 added to [sl] but only propagated. *)
and add x b (el, sl) =
  assert(not(is_interp x));
  if Term.eq x b then 
    (el, sl)
  else if inconsistent x b then
    raise Exc.Inconsistent
  else 
    let (x, b) = orient (x, b) in
    let el' = subst el (x, b) 
    and sl' = compose sl (x, b) in
      (el', sl')
		
(** Ensure [y = x] with [y] fresh and [x] not fresh can not happen. *)
and orient (a, b) =
  if Term.is_var a && Term.is_var b then
    if is_fresh a && not(is_fresh b) then
      (b, a)
    else 
      Term.orient (a, b)
  else
    (a, b)
     
and subst el (x, a) = 
  let apply_to_eq (b1, b2) =
    (apply (x, a) b1, apply (x, a) b2)
  in
    List.map apply_to_eq el

and compose sl (x, a) =
  if is_fresh x && is_fresh a then   (* equality between fresh variables *)
    Term.Subst.fuse apply (x, a) sl  (* can be dropped. *)
  else 
    Term.Subst.compose apply (x, a) sl

and inconsistent a b =
  match width a, width b with
    | Some(n), Some(m) -> n <> m
    | _ -> false


(** Two terms are disequal iff there is at least one
  position on which bitconstants differ. *)
let is_diseq a b =
  try
    let sl = solve (a, b) in
      sl <> []
  with
      Exc.Inconsistent -> true


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
(*i*)

(*s Symbols. *)

let mult = Sym.Pp(Sym.Mult)
let expt n = Sym.Pp(Sym.Expt(n))

(*s Constructors. *)

let mk_one = 
  mk_app mult []

let is_one = function
  | App(Pp(Mult), []) -> true
  | _ -> false

let rec mk_expt n a = 
  Trace.msg "pp" "Expt" (a, n) (Pretty.pair Term.pp Pretty.number); 
  if n = 0 then                     (* [a^0 = 1] *)
    mk_one
  else if n = 1 then                (* [a^1 = a] *)
    a
  else 
    match a with
      | App(Pp(Expt(m)), [x]) ->    (* [x^m^n = x^(m * n)] *)
	  mk_expt (m * n) x
      | App(Pp(Mult), []) ->        (* [1^n = 1] *)
	  mk_one
      | App(Pp(Mult), [x]) -> 
	  mk_app (expt n) [x]
      | App(Pp(Mult), xl) ->        (* [(x1 * ... * xk)^n = x1^n * ... * ...xk^n] *)
	  mk_multl (mapl (mk_expt n) xl)
      | _ ->
	  mk_app (expt n) [a]
      
and mk_multl al =
  List.fold_left mk_mult mk_one al

and mk_mult a b =
  Trace.msg "pp" "Mult" (a, b) (Pretty.pair Term.pp Term.pp);
  match a with
    | App(Pp(Expt(n)), [x]) ->
	mk_mult_with_expt x n b
    | App(Pp(Mult), []) ->
	b
    | App(Pp(Mult), xl) ->
	mk_mult_with_pp xl b
    | _ ->
	mk_mult_with_expt a 1 b

and mk_mult_with_expt x n b =
  match b with
    | App(Pp(Expt(m)), [y]) when Term.eq x y ->  (* [x^n * x*m = x^(n + m)] *)
	mk_expt (n + m) x
    | App(Pp(Mult), []) ->           (* [x^n * 1 = x^n] *)
	mk_expt n x
    | App(Pp(Mult), yl) ->           (* [x^n * (y1 * ... * yk) = (y1 * ... x^n ... * yk)] *)
	insert x n yl
    | _ ->
	insert x n [b]

and mk_mult_with_pp xl b =
  match b with
    | App(Pp(Expt(m)), [y]) ->       (* [(x1 * ... * xk) * y^m = (x1 * ...y^m * ... * xk)] *)
	insert y m xl
    | App(Pp(Mult), yl) ->
	merge yl xl 
    | _ ->
	insert b 1 xl

and cmp1 (x, n) (y, m) =
  let res = Term.cmp x y in
    if res = 0 then Pervasives.compare n m else res

and destruct a =
  match a with
    | App(Pp(Expt(n)), [x]) -> (x, n)
    | _ -> (a, 1)


and insert x n bl =
  merge [mk_expt n x] bl

and insert1 x = insert x 1

and merge al bl =
  Trace.msg "pp" "Merge" (al, bl) (Pretty.pair (Pretty.list Term.pp) (Pretty.list Term.pp));
  let compare a b = cmp1 (destruct a) (destruct b) in
  let rec loop acc al bl = 
    match al, bl with
      | [], [] -> acc
      | _, [] -> List.sort compare (acc @ al)
      | [], _ -> List.sort compare (acc @ bl)
      | a :: al', b :: bl' ->
	  let (x, n) = destruct a 
	  and (y, m) = destruct b in
	  let cmp = Term.cmp x y in
	    if cmp = 0 then
	      if n + m = 0 then
		loop acc al'  bl'
	      else 
		loop (mk_expt (n + m) x :: acc) al' bl'
	    else if cmp < 0 then
	      loop (a :: acc)  al' bl
	    else 
	      loop (b :: acc) al bl'
  in
  match loop [] al bl with
    | [] -> mk_one
    | [c] -> c
    | cl -> mk_app mult cl

let mk_inv a = mk_expt (-1) a
	

(*s Sigma normal forms. *)

let sigma op l =
  match op, l with
    | Expt(n), [x] -> mk_expt n x
    | Mult, xl -> mk_multl xl
    | _ -> assert false



let rec map f a =   
  match a with
    | App(Pp(Expt(n)), [x]) -> 
	let x' = map f x in 
	  if x == x' then a else 
	    mk_expt n x'
    | App(Pp(Mult), xl) ->
	let xl' = mapl (map f) xl in
	  if xl' == xl then a else 
	    mk_multl xl'
    | _ ->
	f a

(*s Constraint. *)

let tau ctxt op l =
  Trace.msg "pp" "tau" l (Pretty.list Term.pp);
  try
    match op, l with
      | Expt(n), [x] -> 
	  Cnstrnt.expt n (ctxt x)
      | Mult, [] -> 
	  Cnstrnt.mk_one
      | Mult, _ -> 
	  Cnstrnt.multl (List.map ctxt l)
      | _ ->
	  assert false
    with
	Not_found -> Cnstrnt.mk_real

(*s Normalize a power product to a list. *)

let to_list a =
  match a with
    | App(Pp(Mult), xl) -> xl
    | _ -> [a]

    
(*s Ordering relation. *)

let cmp a b =
  let rec loop al bl =
    match al, bl with
      | [], [] -> 0
      | [], _ -> -1
      | _, [] -> 1
      | x :: xl', y :: yl' ->
	  let res = cmp1 (destruct x) (destruct y) in
	    if res = 0 then loop xl' yl' else res
  in
    loop (to_list a) (to_list b)

let min a b = if cmp a b <= 0 then a else b

let max a b = if cmp a b <= 0 then b else a


(*s Greatest common divisor of two power products. For example,
  [gcd 'x^2 * y' 'x * y^2] returns the triple [(y, x, x * y)].
  [x * y] is the gcd of these power products. *)

let gcd a b =
  let rec gcdloop ((pl, ql, gcd) as acc) (al, bl) =
    match al, bl with
      | [], [] -> 
	  acc
      | [], bl ->
	  (bl @ pl , ql, gcd)
      | al, [] ->
	  (pl, al @ ql, gcd)
      | a :: al', b :: bl' ->
	  let (x, n) = destruct a 
	  and (y, m) = destruct b in
	  let res = Term.cmp x y in
	    if res = 0 then
	      let acc' = 
		if n = m then 
		  (pl, ql, mk_expt n x :: gcd)
		else if n < m then
		  (mk_expt (m - n) x :: pl, ql, mk_expt n x :: gcd)
		else (* n > m *)
		  (pl, mk_expt (n - m) x :: ql, mk_expt m x :: gcd)
	      in
		gcdloop acc' (al', bl')
	    else if res > 0 then
	      let x_pow_n =  mk_expt n x in
		gcdloop (x_pow_n :: pl, ql, x_pow_n :: gcd) (al', bl)
	    else (* res < 0 *)
	      let y_pow_m =  mk_expt m y in
		gcdloop (y_pow_m :: pl, ql, y_pow_m :: gcd) (al, bl')  
  in
  let (pl, ql, gcd) = gcdloop ([], [], []) (to_list a, to_list b) in
    (mk_multl pl, mk_multl ql, mk_multl gcd)


let split a =
  let (numerator, denumerator) =
    List.partition
      (fun m ->
	 let (_, n) = destruct m in
	   n >= 0)
      (to_list a)
  in
    (mk_multl numerator, mk_inv (mk_multl denumerator))

let numerator a = fst(split a)
let denumerator a = snd(split a)


(*s Least common multiple *)

let lcm qq pp =
  let rec loop acc al bl =
    match al, bl with
      | [], [] -> 
	  acc
      | [], bl ->
	  mk_mult acc (mk_multl bl)
      | al, [] ->
	  mk_mult acc (mk_multl al)
      | a :: al', b :: bl' ->
	  let (x, n) = destruct a 
	  and (y, m) = destruct b in
	  let acc' =
	    if Term.eq x y then
	      mk_mult acc (mk_expt (Pervasives.min n m) x)
	    else 
	      mk_mult a (mk_mult b acc)
	  in
	    loop acc' al' bl'
  in
    loop mk_one (to_list pp) (to_list qq)


(*s Divisibility. *)

let div =
  Trace.func "foo" "div" (Pretty.pair Term.pp Term.pp)  (Pretty.option Term.pp)
    (fun (pp, qq) ->
       try
	 let rec loop acc al bl =
	   match al, bl with
	     | [], [] -> 
		 acc
	     | [], bl ->
		 mk_mult acc (mk_multl bl)
	     | al, [] ->
		 raise Not_found
	     | a :: al', b :: bl' ->
		 let (x, n) = destruct a 
		 and (y, m) = destruct b in
		 let cmp = Term.cmp x y in
		   if cmp = 0 then
		     if n = m then
		       loop acc al' bl'
		     else if n < m then
		       loop (mk_mult acc (mk_expt (m - n) x)) al' bl'
		     else 
		       raise Not_found
		   else if cmp > 0 then
		     loop (mk_mult b acc) al bl'
		   else 
		     raise Not_found
	 in
	   Some(loop mk_one (to_list pp) (to_list qq))
       with
	   Not_found -> None)

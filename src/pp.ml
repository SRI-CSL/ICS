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
 * 
 * Author: Harald Ruess
 *)

open Sym
open Term

(** {6 Symbols.} *)

let mult = Sym.Pp(Sym.Mult)
let expt n = Sym.Pp(Sym.Expt(n))


(** {6 Recognizers.} *)

let rec is_interp a =
  is_expt a || is_mult a
  
and is_expt = function
  | App(Pp(Expt(_)), [x]) -> is_var x
  | _ -> false

and is_mult = function
  | App(Pp(Mult), xl) ->
      (List.for_all (fun x -> is_expt x || is_var x) xl)
  | _ -> false

let rec is_diophantine = function
  | App(Pp(Mult), xl) ->
      List.for_all is_diophantine xl
  | App(Pp(Expt(n)), [x]) when n >= 0 ->
      is_diophantine x
  | a -> 
      is_intvar a

(** {6 Iterators} *)

let rec fold f a e =
  match a with
    | App(Pp(Mult), xl) ->
	List.fold_right (fold f) xl e
    | App(Pp(Expt(n)), [x]) ->
	f x n e
    | _ ->
	f a 1 e


(** {6 Constructors.} *)

let mk_one = 
  mk_app mult []

let is_one = function
  | App(Pp(Mult), []) -> true
  | _ -> false

let rec mk_expt n a = 
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
      | App(Pp(Mult), xl) ->        (* [(x1*...*xk)^n = x1^n*...*...xk^n] *)
	  mk_multl (mapl (mk_expt n) xl)
      | _ ->
	  mk_app (expt n) [a]
      
and mk_multl al =
  List.fold_left mk_mult mk_one al

and mk_mult a b =
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
    | App(Pp(Expt(m)), [y]) 
	when Term.eq x y ->  (* [x^n * x*m = x^(n + m)] *)
	mk_expt (n + m) x
    | App(Pp(Mult), []) ->   (* [x^n * 1 = x^n] *)
	mk_expt n x
    | App(Pp(Mult), yl) -> (* [x^n * (y1 * ... * yk) = (y1*...x^n...*yk)] *)
	insert x n yl
    | _ ->
	insert x n [b]

and mk_mult_with_pp xl b =
  match b with
    | App(Pp(Expt(m)), [y]) -> (* [(x1*...*xk) * y^m = (x1*...y^m*...*xk)] *)
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
  let compare a b = cmp1 (destruct a) (destruct b) in
  let rec loop acc al bl = 
    match al, bl with
      | [], [] -> acc
      | _, [] -> acc @ al   (* needs to be sorted. *)
      | [], _ -> acc @ bl
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
    | cl -> mk_app mult (List.sort compare cl)

let mk_inv a = mk_expt (-1) a
	

(** {6 Sigma normal forms.} *)

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


(** {6 Normal forms.} *)


(** Normalize a power product to a list. *)
let to_list a =
  match a with
    | App(Pp(Mult), xl) -> xl
    | _ -> [a]


let of_list pl =
  List.fold_right
    (fun (y, n) ->
       mk_mult (mk_expt n y))
    pl
    mk_one

    
(** {6 Ordering relation.} *)

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


(** Greatest common divisor of two power products. For example,
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


(** {6 Least common multiple.} *)

let lcm (pp, qq) =
  let rec lcmloop ((pl, ql, lcm) as acc) (al, bl) =
    match al, bl with
      | [], [] -> 
	  acc
      | [], bl ->
	  (bl @ pl, ql, bl @ lcm)
      | al, [] ->
	  (pl, al @ ql, al @ lcm)
      | a :: al', b :: bl' ->
	  let (x, n) = destruct a 
	  and (y, m) = destruct b in
	  let res = Term.cmp x y in
	    if res = 0 then
	      let acc' = 
		let res = Pervasives.compare n m in
		  if res = 0 then       (* [n = m] *)
		    (pl, ql, a :: lcm)
		  else if res < 0 then  (* [n < m] *)
		    (mk_expt (m - n) x :: pl, ql, mk_expt m x :: lcm)
		  else                  (* [n > m]. *)
		    (pl, mk_expt (n - m) x :: ql, mk_expt n x :: lcm)
	      in
		lcmloop acc' (al', bl')
	    else if res > 0 then        (* [y] does not occur in [al] anymore. *)
	      lcmloop (b :: pl, ql, b :: lcm) (al, bl')
	    else 
	      lcmloop (pl, a :: ql, a :: lcm) (al', bl)
  in
    let (pl, ql, lcm) = lcmloop ([], [], []) (to_list pp, to_list qq) in
      (mk_multl pl, mk_multl ql, mk_multl lcm)

let lcm =
  Trace.func "foo7" "lcm"
    (Pretty.pair Term.pp Term.pp)
    (Pretty.triple Term.pp Term.pp Term.pp)
    lcm


(**  {6 Divisibility.} *)

let div (pp, qq) =
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
      Not_found -> None


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


let d_interp = function
  | Term.App(sym, al, _) -> 
      (Sym.Pprod.get sym, al)
  | _ -> 
      raise Not_found


let d_expt = function
  | Term.App(sym, [a], _) -> 
      let n = Sym.Pprod.d_expt sym in
	(n, a)
  | _ ->
      raise Not_found

let d_mult = function
  | Term.App(sym, al, _) 
      when Sym.Pprod.is_mult sym -> al
  | _ -> 
      raise Not_found


(** {6 Recognizers.} *)

let is_interp a =
  try Sym.theory_of (Term.App.sym_of a) = Th.nl with Not_found -> false

let rec is_diophantine a =
  try
    match d_interp a with
      | Mult, xl ->
	  List.for_all is_diophantine xl
      | Expt(n), [x] when n >= 0 ->
	  is_diophantine x
      | _ -> 
	  Term.Var.is_int a
    with
	Not_found -> Term.Var.is_int a

(** {6 Iterators} *)

let rec fold f a e =
  try
    match d_interp a with
      | Mult, xl ->
	  List.fold_right (fold f) xl e
      | Expt(n), [x] ->
	  f x n e
      | _ ->
	  f a 1 e
    with
	Not_found -> f a 1 e

let rec iter f a =
  try
    match d_interp a with
      | Mult, xl ->
	  List.iter (iter f) xl
      | Expt(n), [x] ->
	  f x n
      | _ ->
	  f a 1
    with
	Not_found -> f a 1



(** {6 Constructors.} *)

let mk_one = 
  Term.App.mk_app Sym.Pprod.mk_mult []

let is_one = function
  | Term.App(sym, [], _) 
      when Sym.Pprod.is_mult sym -> true
  | _ -> false

let rec mk_expt n a = 
  if n = 0 then                     (* [a^0 = 1] *)
    mk_one
  else if n = 1 then                (* [a^1 = a] *)
    a
  else 
    try
      match d_interp a with
	| Expt(m), [x] ->    (* [x^m^n = x^(m * n)] *)
	    mk_expt (m * n) x
	| Mult, [] ->        (* [1^n = 1] *)
	    mk_one
	| Mult, [x] -> 
	    Term.App.mk_app (Sym.Pprod.mk_expt n) [x]
	| Mult, xl ->        (* [(x1*...*xk)^n = x1^n*...*...xk^n] *)
	    mk_multl (Term.mapl (mk_expt n) xl)
	| _ ->
	    Term.App.mk_app (Sym.Pprod.mk_expt n) [a]
      with
	  Not_found ->  
	    Term.App.mk_app (Sym.Pprod.mk_expt n) [a]
      
and mk_multl al =
  List.fold_left mk_mult mk_one al

and mk_mult a b =
  try
    match d_interp a with
      | Expt(n), [x] ->
	  mk_mult_with_expt x n b
      | Mult, [] ->
	  b
      | Mult, xl ->
	  mk_mult_with_pp xl b
      | _ ->
	  mk_mult_with_expt a 1 b
    with
	Not_found -> mk_mult_with_expt a 1 b

and mk_mult_with_expt x n b =
  try
    match d_interp b with
      | Expt(m), [y]
	  when Term.eq x y ->  (* [x^n * x*m = x^(n + m)] *)
	  mk_expt (n + m) x
      | Mult, [] ->            (* [x^n * 1 = x^n] *)
	  mk_expt n x
      | Mult, yl ->            (* [x^n * (y1 * ... * yk) = (y1*...x^n...*yk)] *)
	  insert x n yl
      | _ ->
	  insert x n [b]
    with
	Not_found -> insert x n [b]

and mk_mult_with_pp xl b =
  try
    match d_interp b with
      | Expt(m), [y] -> (* [(x1*...*xk) * y^m = (x1*...y^m*...*xk)] *)
	  insert y m xl
      | Mult, yl ->
	  merge yl xl 
      | _ ->
	  insert b 1 xl
    with
	Not_found ->  insert b 1 xl

and cmp1 (x, n) (y, m) =
  let res = Term.cmp x y in
    if res = 0 then Pervasives.compare n m else res

and destruct a =
  try
    let (n, x) = d_expt a in
      (x, n)
  with
      Not_found -> (a, 1)
 

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
    | cl -> Term.App.mk_app Sym.Pprod.mk_mult (List.sort compare cl)

let mk_inv a = mk_expt (-1) a
	

(** {6 Sigma normal forms.} *)

let sigma op l =
  match op, l with
    | Expt(n), [x] -> mk_expt n x
    | Mult, xl -> mk_multl xl
    | _ -> assert false


let rec map f a = 
  try
    match d_interp a with
      | Expt(n), [x] -> 
	  let x' = map f x in 
	    if x == x' then a else 
	      mk_expt n x'
      | Mult, xl ->
	  let xl' = Term.mapl (map f) xl in
	    if xl' == xl then a else 
	      mk_multl xl'
      | _ ->
	  f a
    with
	Not_found -> f a


(** Replacing a variable with a term. *)
let apply (x, b) = 
  map (fun y -> if Term.eq x y then b else y)


(** {6 Normal forms.} *)


(** Normalize a power product to a list. *)
let to_list a =
  try d_mult a with Not_found -> [a]

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


(** {7 Sign partition} *)

let partition p a = 
  let (nonneg, uncnstrnt) =
    fold
      (fun y n (nonneg, uncnstrnt) ->
	 if p y n then
	   ((y, n) :: nonneg, uncnstrnt)
	 else 
	   (nonneg, (y, n) :: uncnstrnt))
      a
      ([], [])
  in
    (of_list nonneg, of_list uncnstrnt)


(** {6 Abstract constraint interpretatiosn} *)

let dom lookup op al =
  let product a =
    try
      let (n, x) = d_expt a in
	Dom.expt n (lookup x)
    with
	Not_found -> lookup a
  in
  try
    (match op, al with
       | Expt(n), [a] -> Dom.expt n (lookup a)
       | Mult, [] -> Dom.of_q Mpa.Q.one
       | Mult, [a] -> product a
       | Mult, [a; b] -> Dom.mult (product a) (product b)
       | Mult, _ -> Dom.multl (List.map product al)
       | _ -> assert false)
  with
      Not_found -> Dom.Real

let rec dom_of = function
  | Term.Var(x, _) -> 
      Var.dom_of x
  | Term.App(sym, al, _) ->
      let op = Sym.Pprod.get sym in
	dom dom_of op al


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

let is_interp = function
  | App(Product _, _) -> true
  | _ -> false

(*s Destructors. *)

let d_tuple = function
  | App(Product(Tuple), xl) -> Some(xl)
  | _ -> None

let d_proj = function
  | App(Product(Proj(i, n)), [x]) -> Some(i, n, x)
  | _ -> None


(*s Fold iterator  *)

let rec fold f a e = 
  match a with
    | App(Product(Tuple), xl) ->
	List.fold_right (fold f) xl e
    | App(Product(Proj _), [x]) -> 
	fold f x e
    | _ ->
	f a e
 

(*s Constructors for tuples and projections. *)

let mk_tuple = 
  let product = Product(Tuple) in
    function
      | [x] -> x
      | ([x; y] as xl) ->
	  (match x, y with
	     | App(Product(Proj(0,2)), [z1]), 
	       App(Product(Proj(1,2)), [z2]) when Term.eq z1 z2 ->
		 z1
	     | _ ->
		 Term.mk_app product xl)
      | xl -> 
	  Term.mk_app product xl

let mk_proj i n a =
  match a with
    | App(Product(Tuple), xl) ->
	List.nth xl i
    | _ -> 
	Term.mk_app (Product(Proj(i, n))) [a]


(*s Apply term transformer [f] at uninterpreted positions. *)

let rec map f a =
  match a with
    | App(Product(Tuple), xl) ->
	let xl' = Term.mapl (map f) xl in
	  if xl == xl' then a else 
	    mk_tuple xl'
    | App(Product(Proj(i, n)), [x]) ->
	let x' = map f x in
	  if x == x' then a else 
	    mk_proj i n x'
    | _ -> 
	f a


(*s Sigmatizing. *)

let sigma op l =
  match op, l with
    | Tuple, _ -> 
	mk_tuple l
    | Proj(i, n), [x] -> 
	mk_proj i n x
    | _ -> 
	assert false


(*s Fresh variables. *)

let mk_fresh =
  let name = Name.of_string "t" in
    fun () -> Var(Var.mk_fresh name None)

(*s Solving tuples. *) 

let rec solve e =
  let (a, b, _) = Fact.d_equal e in
    solvel [(a, b)] []

and solvel el sl =
  match el with
    | [] -> sl
    | (a, b) :: el1 ->
	solve1 (a, b) el1 sl 

and solve1 (a, b) el sl = 
  if Term.eq a b then 
    solvel el sl
  else if Term.is_var b then
    solvevar (b, a) el sl
  else match a with
    | App(Product(Proj(i, n)), [x]) -> 
	let e' = proj_solve i n x b in
	solvel (e' :: el) sl
    | App(Product(Tuple), xl) ->
	solvel (tuple_solve xl b el) sl 
    | _ -> 
	solvevar (a, b) el sl

and solvevar (x, b) el sl =
  if is_var b then
    solvel el (add (Term.orient (x, b)) sl)
  else if Term.occurs x b then
    raise Exc.Inconsistent
  else 
    solvel el (add (x, b) sl)
	
(*s [(a0,...,a{n-1}) = b] iff [a0 = proj{0,n}(b)] and ... 
 and [a{n-1} = proj{n-1, n}(b)] *)

and tuple_solve al b acc = 
  let n = List.length al in
  let rec loop i al acc =
    match al with
      | [] -> acc
      | a :: al' ->
	  let b' = mk_proj i n b in
          let acc' = (a, b') :: acc in
	    loop (i + 1) al' acc'
  in
  loop 0 al acc

(*s [solve (proj i n s, t) = (s, \list{c0,...,t,...cn-1})]
     where [ci] are fresh, [s] at [i]-th position. *)

and proj_solve i n s t =
  let rec args j acc =
    if j = -1 then acc
    else
      let a = 
	if i = j then 
	  t 
	else 
	    mk_fresh()
      in  (* fresh var equals [mk_proj j n s] *)
	args (j - 1) (a :: acc)
  in
    (s, mk_tuple (args (n - 1) []))

and add (a, b) sl =
  if Term.eq a b then 
    sl
  else
    let e = Fact.mk_equal a b None in  (* does not swap terms *)
      e :: (substl a b sl)             (* since [a] and [b] are oriented *)

and substl a b = 
  List.map 
    (fun e -> 
       let (x, y, _) = Fact.d_equal e in
	 Fact.mk_equal x (subst1 y a b) None)

and subst1 a x b =      (* substitute [x] by [b] in [a]. *)
  map (fun y -> if Term.eq x y then b else y) a

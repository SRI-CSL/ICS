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

open Sym
open Term

let cons = Pair(Cons)
let car = Pair(Car)
let cdr = Pair(Cdr)


let is_interp = function
  | App(Pair _, _) -> true
  | _ -> false

let is_cons = function
  | App(Pair(Cons), [_;_]) -> true
  | _ -> false

let is_car = function
  | App(Pair(Car), [_]) -> true
  | _ -> false

let is_cdr = function
  | App(Pair(Cdr), [_]) -> true
  | _ -> false
 

(** [cons(car(x), cdr(x))] reduces to [x]. *)
let mk_cons a b = 
  match a, b with
  (*  | App(Pair(Car), [x]), App(Pair(Cdr), [x'])  
	when Term.eq x x' -> x *)
    | _ ->
	Term.mk_app cons [a;b]

(** [car(cons(a, _))] reduces to [a]. *)
let mk_car = function
  | App(Pair(Cons), [a; _]) -> a
  | a -> Term.mk_app car [a]

(** [cdr(cons(_, b))] reduces to [b]. *)
let mk_cdr = function
  | App(Pair(Cons), [_; b]) -> b
  | a -> Term.mk_app cdr [a]

(** Derived constructors. *)

let rec mk_tuple = function
  | [a] -> a
  | [a; b] -> mk_cons a b
  | a :: al -> mk_cons a (mk_tuple al)
  | [] -> assert false

let rec mk_proj i a =
  if i <= 0 then 
    mk_car a 
  else if i = 1 then 
    mk_cdr a
  else 
    mk_cdr (mk_proj (i - 1) a)


(** Apply term transformer [f] at uninterpreted positions. *)
let rec map f a =
  match a with
    | App(Pair(op), al) ->
	(match op, al with
	   | Cons, [b1;b2] ->
	       let b1' = map f b1
	       and b2' = map f b2 in
		 if b1 == b1' && b2 == b2' then a else
		   mk_cons b1' b2'
	   | Car, [b] ->
	       let b' = map f b in
		 if b == b' then a else  mk_car b'
	   | Cdr, [b] ->
	       let b' = map f b in
		 if b == b' then a else  mk_cdr b'
	   | _ ->
	       f a)
    | _ ->
	f a

(** Canonization. *)
let sigma op l =
  match op, l with
    | Cons, [a; b] -> mk_cons a b
    | Car, [a] -> mk_car a
    | Cdr, [b] -> mk_cdr b 
    | _ -> Term.mk_app (Pair(op)) l


(** Fresh variables. *)
let mk_fresh =
  let name = Name.of_string "t" in
    fun () -> Term.mk_fresh name None None

(** Solving tuples. *) 
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
  else 
    match a, b with
      | App(Pair(Car), [s]), (App(Pair _, _) as n) -> 
	  solvel ((s, mk_cons n (mk_fresh())) :: el) sl
      | (App(Pair _, _) as n), App(Pair(Car), [s]) -> 
	  solvel ((s, mk_cons n (mk_fresh())) :: el) sl
      | App(Pair(Cdr), [s]), (App(Pair _, _) as n) -> 
	  solvel ((s, mk_cons (mk_fresh()) n) :: el) sl
      | (App(Pair _, _) as n), App(Pair(Cdr), [s]) -> 
	  solvel ((s, mk_cons (mk_fresh()) n) :: el) sl
      | App(Pair(Cons), [s; t]), App(Pair(Cons), [u;v]) ->
	  solvel ((s, u) :: (t, v) :: el) sl
      | x, t when not(is_interp x) && not(occurs x t) ->
	  solvel el (add (x, t) sl)
      | t, x when not(is_interp x) && not(occurs x t) ->
	  solvel el (add (x, t) sl)
      | x, t when not(is_cons t) || occurs_as_arg_of_a_cons x t ->
	  raise Exc.Inconsistent
      | t, x when not(is_cons t) || occurs_as_arg_of_a_cons x t ->
	  raise Exc.Inconsistent
      | x, (App(Pair(Cons), [s1; s2]) as t)
	  when not(is_interp x) && occurs_as_arg_of_a_proj x t ->
	  let z1 = mk_fresh() and z2 = mk_fresh() in
	  let rho = [mk_car(x), z1; mk_cdr(x), z2] in
	  let s1' = replace_proj s1 x z1 z2 
	  and s2' = replace_proj s2 x z1 z2 in
	    solvel ((z1, s1') :: (z2, s2') :: el) (add (x, mk_cons s1' s2') sl)
      | _ ->
	  raise Exc.Incomplete

and replace_proj a x z1 z2 =    (* replace [car(x)] by [z1] and [cdr(x)] by [z2] in [a]. *)
  let rec loop a = 
    match a with
      | App(Pair(Cons), [a1; a2]) ->
	  let a1' = loop a1 and a2' = loop a2 in
	    if a1' == a1 && a2' == a2 then a else mk_cons a1' a2'
      | App(Pair(Car), [b]) ->
	  if Term.eq b x then z1 else
	    let b' = loop b in
	      if b == b' then a else mk_car b'
      | App(Pair(Cdr), [b]) ->
	  if Term.eq b x then z2 else
	    let b' = loop b in
	      if b == b' then a else mk_cdr b'
      | _ ->
	  a
  in
    loop a

and occurs_as_arg_of_a_proj x = function
  | App(Pair(Car), [y]) -> Term.eq x y
  | App(Pair(Cdr), [y]) -> Term.eq x y
  | App(Pair(Cons), [s; t]) -> (occurs_as_arg_of_a_proj x s || occurs_as_arg_of_a_proj x t)
  | _ -> false

and occurs_as_arg_of_a_cons x = function
  | App(Pair(Cons), [s; t]) ->
      Term.eq x s ||
      Term.eq x t ||
      occurs_as_arg_of_a_cons x s || 
      occurs_as_arg_of_a_cons x t
  | _ -> false  

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



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
  | App(Coproduct _, _) -> true
  | _ -> false


(*s Fold iterator  *)

let rec fold f a e = 
  match a with
    | App(Coproduct(_), [x]) -> fold f x e
    | _ -> f a e


(*s Constructors for tuples and projections. *)

let mk_inl =
  let sym = Coproduct(InL) in
    function
      | App(Coproduct(OutL), [x]) -> x
      | x -> mk_app sym [x]

let mk_inr =
  let sym = Coproduct(InR) in
    function
      | App(Coproduct(OutR), [x]) -> x
      | x -> mk_app sym [x]

let mk_outr =
  let sym = Coproduct(OutR) in
    function
      | App(Coproduct(InR), [x]) -> x
      | x -> mk_app sym [x]

let mk_outl =
  let sym = Coproduct(OutL) in
    function
      | App(Coproduct(InL), [x]) -> x
      | x -> mk_app sym [x]

let rec mk_inj i x = 
  if i <= 0 then
    mk_inl x
  else if i = 1 then
    mk_inr x
  else 
    mk_inr (mk_inj (i - 1) x)

let rec mk_out i x = 
  if i <= 0 then
    mk_outl x
  else if i = 1 then
    mk_outr x
  else 
    mk_outr (mk_out (i - 1) x)


(*s Sigmatizing. *)

let sigma op l =
  match op, l with
    | InL, [x] -> mk_inl x 
    | InR, [x] -> mk_inr x 
    | OutL, [x] -> mk_outl x 
    | OutR, [x] -> mk_outr x 
    | _ -> assert false

 
(*s Apply term transformer [f] at uninterpreted positions. *)

let rec map f a =
  match a with
    | App(Coproduct(op), [x]) ->
	let x' = map f x in
	  if x == x' then a else sigma op [x']
    | _ -> 
	f a



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
    | App(Coproduct(op), [x]) ->  (*s solve [inY(x) = b] is [x = outY(b)]. *)
	let rhs' = match op with  (*s solve [outY(x) = b] is [x = inY(b)]. *)
	  | InL -> mk_outl b
	  | InR -> mk_outr b
	  | OutL -> mk_inl b
	  | OutR -> mk_inr b
	in 
	  solvel ((x, rhs') :: el) sl
    | _ -> 
	solvevar (a, b) el sl
	
and solvevar (x, b) el sl =
  if is_var b then
    solvel el (add (Term.orient (x, b)) sl)
  else if Term.occurs x b then
    raise Exc.Inconsistent
  else 
    solvel el (add (x, b) sl)

and add (a, b) sl =
  if Term.eq a b then  sl else
    let e = Fact.mk_equal a b None in  (* does not swap terms *)
      e :: (substl a b sl)             (* since [a] and [b] are oriented *)

and substl a b = 
  List.map 
    (fun e -> 
       let (x, y, _) = Fact.d_equal e in
	 Fact.mk_equal x (subst1 y a b) None)

and subst1 a x b =      (* substitute [x] by [b] in [a]. *)
  map (fun y -> if Term.eq x y then b else y) a

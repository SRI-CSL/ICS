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

(** Theory definition. *)
let theory = Theory.create "p"

let is_theory = Theory.eq theory 

let _ = 
  Theory.Description.add theory
    "Theory of pairs and projections."

(** Signature for products. *)
module Sig = struct
  let th = theory
  type t = Cons | Car | Cdr
  let name = 
    let cons = Name.of_string "cons"
    and car = Name.of_string "car"
    and cdr = Name.of_string "cdr" in
      function 
	| Cons -> cons
	| Car -> car
	| Cdr -> cdr
end

module Op = Funsym.Make(Sig)

let op t = Op.out (Term.sym_of t)
let args t = Term.Args.to_list (Term.args_of t)

let is_interp a =
  try Op.is_interp (Term.sym_of a) with Not_found -> false

let rec is_pure a =
  try
    (match op a, args a with
       | Sig.Cons, [b1; b2] -> is_pure b1 && is_pure b2
       | Sig.Car, [b] -> is_pure b
       | Sig.Cdr, [b] -> is_pure b
       | _ -> false)
  with
      Not_found -> Term.is_var a

let is_car a =
  try
    (match op a, args a with
       | Sig.Car, [_] -> true
       | _ -> false)
  with
      Not_found -> false

let is_cdr a =
  try
    (match op a, args a with
       | Sig.Cdr, [_] -> true
       | _ -> false)
  with
      Not_found -> false

let is_cons a =
  try
    (match op a, args a with
       | Sig.Cons, [_; _] -> true
       | _ -> false)
  with
      Not_found -> false

let d_car a =
  match op a, args a with
    | Sig.Car, [b] -> b
    | _ -> raise Not_found

let d_cdr a =
  match op a, args a with
    | Sig.Cdr, [b] -> b
    | _ -> raise Not_found

let d_cons a =
  match op a, args a with
    | Sig.Cons, [b1; b2] -> b1, b2
    | _ -> raise Not_found

let fst a =
  match op a, args a with
    | Sig.Cons, [b1; _] -> b1
    | _ -> raise Not_found

let snd a =
  match op a, args a with
    | Sig.Cons, [_; b2] -> b2
    | _ -> raise Not_found


(** [cons(car(x), cdr(x))] reduces to [x]. *)
let mk_cons = 
  let f = Op.inj(Sig.Cons) in
  let cons a b = Term.mk_binary f a b in
    fun a b -> 
      try
	let x = d_car a and y = d_cdr b in
	  if Term.eq x y then x else cons a b
      with
	  Not_found -> cons a b

(** [car(cons(b, _))] reduces to [b]. *) 
let mk_car =
  let f = Op.inj(Sig.Car) in
  let car a = Term.mk_unary f a in
    fun a -> 
      try
	(match op a, args a with
	   | Sig.Cons, [b; _] -> b
	   | _  -> car a)
      with
	  Not_found -> car a

(** [cdr(cons(_, b))] reduces to [b]. *)
let mk_cdr =
  let f = Op.inj(Sig.Cdr) in 
  let cdr a = Term.mk_unary f a in
    fun a -> 
      try
	(match op a, args a with
	   | Sig.Cons, [_; b] -> b
	   | _  -> cdr a)
      with
	  Not_found -> cdr a

(** Canonizer. *)
let sigma f l =
  assert(Op.is_interp f);
  match Op.out f, Term.Args.to_list l with
    | Sig.Cons, [a; b] -> mk_cons a b
    | Sig.Car, [a] -> mk_car a
    | Sig.Cdr, [b] -> mk_cdr b
    | _ -> invalid_arg "Product.sigma: uninterpreted"


(** Disequality test. *)
let rec is_diseq a b =
  try
    let f = op a and al = args a in
      (try
	 is_diseq_interp f al (op b) (args b)
       with
	   Not_found -> is_diseq_uninterp f al b)
  with
      Not_found -> 
	(try 
	   is_diseq_uninterp (op b) (args b) a
	 with
	     Not_found -> false)

and is_diseq_interp f al g bl =
  match f, al, g, bl with
    | Sig.Cons, [a1; a2], Sig.Cons, [b1; b2] ->
	is_diseq a1 b1 || is_diseq a2 b2
    | Sig.Car, [a1], Sig.Car, [b1] ->
	is_diseq a1 b1
    | Sig.Cdr, [a1], Sig.Cdr, [b1] ->
	is_diseq a1 b1
    | _ ->
	false

and is_diseq_uninterp f al b =
 let rec arg_of_iterated_destructor a =
   try
     (match op a, args a with
	| (Sig.Car | Sig.Cdr), [b] -> arg_of_iterated_destructor b
	| _ -> a)
   with
       Not_found -> a
 in
   match f, al with
     | Sig.Cons, [a1; a2] -> Term.eq a1 b ||  Term.eq a2 b
     | (Sig.Car | Sig.Cdr), [a1] -> Term.eq (arg_of_iterated_destructor a1) b
     | _ -> false
 
let _ = 
  let m = Term.Methods.empty() in
    m.Term.Methods.can <- Some(sigma);
    m.Term.Methods.is_diseq <- Some(is_diseq);
    Term.Methods.register theory m
      

(** Tuple constructors *)
let rec mk_tuple = function
  | [a] -> a
  | [a; b] -> mk_cons a b
  | a :: al -> mk_cons a (mk_tuple al)
  | [] -> invalid_arg "Product.tuple: empty argument"

(** Generalized projection *)
let rec mk_proj i a =
  if i <= 0 then 
    mk_car a 
  else if i = 1 then 
    mk_cdr a
  else 
    mk_cdr (mk_proj (i - 1) a)


(** Apply term transformer [f] at uninterpreted positions. *)
let map f =
  let rec mapf a = 
    try
      (match op a, args a with
	 | Sig.Cons, [b1; b2] ->
	     let b1' = mapf b1 and b2' = mapf b2 in
	       if b1 == b1' && b2 == b2' then a else mk_cons b1' b2'
	 | Sig.Car, [b] -> 
	     let b' = mapf b in
	       if b == b' then a else mk_car b'
	 | Sig.Cdr, [b] -> 
	     let b' = mapf b in
	       if b == b' then a else mk_cdr b'
	 | _ -> 
	     f a)
    with
	Not_found -> f a
  in
    mapf

let norm (x, a) = 
  assert(Term.is_var x);
  assert(not(Term.occurs x a));
  let lookup z = if z == x then a else z in
    map lookup

(** Solver. *)

let rec is_constructor_term a =
  try
    (match op a with
       | Sig.Cons -> List.for_all is_constructor_term (args a)
       | _ -> false)
  with
      Not_found -> Term.is_var a

let rec solve a b = 
  assert(is_pure a && is_pure b);
  let rec solvel ks el sl = 
    match el with
      | [] -> ks, Term.Subst.of_list sl
      | e :: el' -> solve1 ks e el' sl 
  and solve1 ks ((a, b) as e) el sl =
    if Term.eq a b then 
      solvel ks el sl
    else
      try
	let x = d_proj a in
	  proj x ks (e :: el) sl
      with
	  Not_found ->
	    (try
	       let y = d_proj b in
		 proj y ks (e :: el) sl
	     with
		 Not_found ->
		   assert(is_constructor_term a && is_constructor_term b);
		   (match is_cons a, is_cons b with
		      | true, true -> 
			  let (a1, a2) = d_cons a
			  and (b1, b2) = d_cons b in
			    decompose (a1, b1) (a2, b2) ks el sl
		      | false, false -> 
			  rename (a, b) ks el sl
		      | false, true -> 
			  constructor (a, b) ks el sl
		      | true, false ->
			  constructor (b, a) ks el sl))
  and constructor ((x, b) as e) ks el sl =
    assert(Term.is_var x);
    assert(not(Term.eq x b));
    assert(is_constructor_term b);
    if Term.is_var_of x b then
      raise Exc.Inconsistent
    else 
      let el' = replace e el 
      and sl' = if Term.Set.mem x ks then fuse sl e else compose sl e in
	solvel ks el' sl'
  and proj x ks el sl =
    assert(Term.is_var x);
    assert(not(Term.Set.mem x ks));
    let k1 = Term.mk_fresh_var "k"
    and k2 = Term.mk_fresh_var "k" in
    let e' = (x, mk_cons k1 k2) in
    let ks' = Term.Set.add k1 ks; Term.Set.add k2 ks; ks 
    and el' = replace e' el 
    and sl' = compose sl e' in
      solvel ks' el' sl'
  and rename ((x, y) as e) ks el sl = 
    assert(Term.is_var x && Term.is_var y);
    let el' = replace e el
    and sl' = replace e sl in
      solvel ks el' sl'
  and decompose e1 e2 ks el sl =
    solvel ks (e1 :: e2 :: el) sl
  in
    solve1 (Term.Set.empty()) (a, b) [] []

and fuse sl e = 
  let norm_right (y, b) = 
    let b' = norm e b in
      if b == b' then e else (y, b')
  in 
    List.map norm_right sl


and compose sl ((x, a) as e) =
  assert(Term.is_var x);
  assert(not(Term.occurs x a));
  assert(not(List.exists (fun (y, _) -> Term.eq x y) sl));
  let sl' = fuse sl e in
    e :: sl'

and replace e el =
  let norm1 ((a, b) as eq) = 
    let a' = norm e a and b' = norm e b in
      if a == a' && b == b' then eq else (a', b')
  in
    List.map norm1 el
  
and d_proj a =
  assert(is_pure a);
  match op a, args a with
    | (Sig.Car | Sig.Cdr), [a] ->
	if Term.is_var a then a else d_proj a
    | Sig.Cons, [a1; a2] -> 
	(try d_proj a1 with Not_found -> d_proj a2)
    | _ -> 
	assert false
  


(** {6 Inference System} *)

(** Description of the theory of products as a convex Shostak theory. *)
module T = struct
  let th = theory
  let can = sigma
  let map = map
  let solve = solve
  let disjunction _ = raise Not_found
end

(** Inference system for products as an 
  instance of a Shostak inference system. *)
module Component = Shostak.Make(T)
module P = E.Equal(Component)


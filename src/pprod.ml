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


let d_mult = function
  | Term.App(sym, [a; b], _) 
      when Sym.Pprod.is_mult sym 
	-> (a, b)
  | _ -> 
      raise Not_found

let is_interp = function
  | Term.App(sym, _, _) when Sym.Pprod.is_mult sym  -> true
  | _ -> false

let d_interp = d_mult

let is_mult =
  function
    | Term.App(sym, [a; b], _) 
	when Sym.Pprod.is_mult sym 
	  -> true
    | _ -> 
	false

let rec is_diophantine a =
  try
    let (a1, a2) = d_mult a in
      is_diophantine a1 || is_diophantine a2
  with
      Not_found -> Term.Var.is_int a


let rec is_nonneg a =
  try
    let (a1, a2) = d_mult a in
      is_nonneg a1 || is_nonneg a2
  with
      Not_found -> Term.Var.is_slack a


(** Number of occurrences of [x] in [a]. *)  
let multiplicity x =
  let rec scan acc a =
    try
      let (y, b) = d_mult a in
      let cmp = Term.cmp x y in
	if cmp < 0 then        (* [x << y] *)
	  scan acc b
	else if cmp = 0 then   (* [x = y] *)
	  scan (1 + acc) b
	else                   (* [x >> y] *)
	  acc
    with
	Not_found -> 
	  if Term.eq x a then (1 + acc) else acc
  in
    scan 0

(** Decompose [x*x*...*x*y*....] into [(x, n)] with [n] the
  multiplicity of [x] in [a] and [y*...], or raise [Not_found]
  if input term is not a multiplication. *)
let decompose a =
  try
    let (x, _) = d_mult a in  
    let rec scan acc post =
      try
	let (y, b) = d_mult post in
	  if Term.eq x y then
	    scan (acc + 1) b
	  else 
	    (acc, Some(post))
      with
	  Not_found -> 
	    if Term.eq x post then (acc + 1, None) else  (acc, None)  
    in
    let (n, b) = scan 0 a in
      ((x, n), b)
  with
      Not_found -> ((a, 1), None)


let rec fold f a acc =
  let ((x, n), b) = decompose a in
  let acc' = f x n acc in
    match b with
      | None -> acc'
      | Some(b') -> fold f b' acc'


let rec iter f a =
  let ((x, n), b) = decompose a in
    f x n;
    match b with
      | None -> ()
      | Some(b') -> iter f b'
 

(** Ordered right-associative power products. *)
let rec mk_mult a b =
  try
    let (a1, a2) = d_mult a in
      mk_mult a1 (mk_mult a2 b)
  with
      Not_found -> 
	assert(not(is_mult a));
	try
	  let (b1, b2) = d_mult b in
	  let cmp = Term.cmp a b1 in
	    if Term.cmp a b1 <= 0 then   (* case [a <= b1] *)
	      mult a b
	    else                         (* case [a > b1] *)
	      mk_mult b1 (mk_mult a b2)
	with
	    Not_found -> 
	      assert(not(is_mult b));
	      if Term.cmp a b <= 0 then
		mult a b
	      else
		mult b a
		
and mult a b =
  Term.App.mk_app Sym.Pprod.mk_mult [a; b]

and mk_expt a n =
  assert(n >= 1);
  if n = 1 then a else mk_mult a (mk_expt a (n - 1))

let rec of_list = function
  | [(a, n)] -> mk_expt a n
  | (a, n) :: al -> mk_mult (mk_expt a n) (of_list al)
  | [] -> invalid_arg "Pprod.of_list: empty list"
 

(** Sigma normal forms. *)
let sigma _ = function
  | [a; b] -> mk_mult a b
  | al -> Term.App.mk_app Sym.Pprod.mk_mult al

 
(** Apply [f] to uninterpreted positions. *)
let rec map f a = 
  try
    let (a1, a2) = d_mult a in
    let b1 = map f a1 and b2 = map f a2 in
      if a1 == b1 && a2 == b2 then a else mk_mult b1 b2
  with
      Not_found -> f a


(** Replacing a variable with a term. *)
let apply (x, b) = 
  map (fun y -> if Term.eq x y then b else y)

(** Divide [a] of the form [x * ... y^m ... *z] by [y^n], where [n <= m]. *)
let rec divide a (y, n) =
  let ((y', m), post) = decompose a in
  let cmp = Term.cmp y y' in
    if cmp < 0 then       (* [y << y'] *)
      (match post with
	 | Some(b') -> mk_mult (mk_expt y' m) (divide b' (y, n))
	 | None -> invalid_arg "Pprod.divide: denumerator not found")
    else if cmp = 0 then 
      (match post with
	 | Some(b') -> 
	     assert(n <= m);
	     mk_mult (mk_expt y' (m - n)) b'
	 | None -> 
	     mk_expt y' (m - n))
    else    
      invalid_arg "Pprod.divide: denumerator not found"
   

(** Abstract constraint interpretation *)
let rec dom_of a =
  try
    let (a1, a2) = d_mult a in
      Dom.mult (dom_of a1) (dom_of a2)
  with
      Not_found -> Term.Var.dom_of a
      
let dom lookup op al =
  let dom_of x = 
    try
      let d1 = Term.Var.dom_of x in
	(try 
	  let d2 = lookup x in
	    Dom.inter d1 d2
	with
	    Not_found -> d1)
    with
	Not_found -> 
	  try lookup x with Not_found -> Dom.Real
  in
    Dom.multl (List.map dom_of al)
  

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


let is_interp = function
  | Term.App(op, _, _) -> Sym.Propset.is op
  | _ -> false

let d_interp = function
  | Term.App(op, l, _) -> (Sym.Propset.get op, l)
  | _ -> raise Not_found

let is_empty a =
  try
    (match d_interp a with
      | Sym.Empty, [] -> true
      | _ -> false)
  with
      Not_found -> false

let is_full a =
  try
    (match d_interp a with
       | Sym.Full, [] -> true
       | _ -> false)
  with
      Not_found -> false

 let is_diseq a b =
   (is_empty a && is_full b) || (is_full a && is_empty b)

let is_const a = is_empty a || is_full a

let mk_empty () = Term.App.mk_const (Sym.Propset.mk_empty)
let mk_full () = Term.App.mk_const (Sym.Propset.mk_full)

let d_ite a = 
  match d_interp a with
    | Sym.Ite, [a; b; c] -> (a, b, c)
    | _ -> raise Not_found


(** Constructing BDDs for conditional set constructor. *)
let mk_ite a b c = 
  let cofactors x a =
    try
      let (y, pos, neg) = d_ite a in
	if Term.eq x y then (pos, neg) else (a, a)
    with
	Not_found -> (a, a)
  in
  let topvar x s2 s3 =
    let to_option f s = try Some(f s) with Not_found -> None in
    let max x y = if Term.(<<<) x y then x else y in
      match to_option d_ite s2, to_option d_ite s3 with
	| Some(y,_,_), Some(z,_,_) -> max x (max y z)
	| Some(y,_,_), None -> max x y
	| None, Some(z,_,_) -> max x z
	| None, None -> x
  in
  let rec build s3 =
    let module H3 = Hashtbl.Make(
      struct
	type t = Term.t * Term.t * Term.t
	let equal (a1, a2, a3) (b1, b2, b3) = 
	  Term.eq a1 b1 && Term.eq a2 b2 && Term.eq a3 b3
	let hash = Hashtbl.hash
      end)
    in
    let ht =  H3.create 17 in
    let _ = Tools.add_at_reset (fun () -> H3.clear ht) in
      try
	H3.find ht s3
      with 
	  Not_found ->
	    let b = build_fun s3 in
	      H3.add ht s3 b; b 
  and build_fun (s1, s2, s3) =
    if Term.eq s2 s3 then s2
    else if is_full s2 && is_empty s3 then s1
    else if is_full s1 then s2
    else if is_empty s1 then s3
    else 
      try
	let (y, _, _) = d_ite s1 in
	let x = topvar y s2 s3 in
	let (pos1, neg1) = cofactors x s1 in
	let (pos2, neg2) = cofactors x s2 in
	let (pos3, neg3) = cofactors x s3 in
	let pos = build (pos1, pos2, pos3) in
	let neg = build (neg1, neg2, neg3) in
	  if Term.eq pos neg then pos else 
	    Term.App.mk_app Sym.Propset.mk_ite [x; pos; neg]
      with
	  Not_found -> 
	    Term.App.mk_app Sym.Propset.mk_ite [s1; s2; s3]
  in
  let lift a =
    if is_interp a then a else
      Term.App.mk_app Sym.Propset.mk_ite [a; mk_full(); mk_empty()]
  in
  let  drop a =
    try
      let (b1, b2, b3) = d_ite a in
	if is_full b2 && is_empty b3 then b1
	else 
	  a
    with
	Not_found -> a
  in	
    try
      drop (build (lift a, lift b, lift c))
    with
	Not_found -> invalid_arg "ite"
      

let mk_inter a b = mk_ite a b (mk_empty())
let mk_union a b = mk_ite a (mk_full()) b
let mk_compl a = mk_ite a (mk_empty()) (mk_full())
let mk_imp a b = mk_ite a b (mk_full())
let mk_iff a b = mk_ite a b (mk_compl b)


(** Mapping over bitvector terms. *)
let rec map f a =
  try
    (match d_interp a with
       | Sym.Empty, [] -> a
       | Sym.Full, [] -> a
       | Sym.Ite, [b1; b2; b3] ->
	   mk_ite (map f b1) (map f b2) (map f b3)
       | _ -> 
	   f a)
  with
      Not_found -> f a


(** Replacing a variable with a term. *)
let apply (x, b) =
  let lookup y = if Term.eq x y then b else y in
    map lookup


(** Canonized set expressions. *)
let sigma op l =
  match op, l with
    | Sym.Empty, [] -> mk_empty()
    | Sym.Full, [] -> mk_full()
    | Sym.Ite, [a; b; c] -> mk_ite a b c
    | _ ->  failwith "Propset.sigma: ill-formed expression"

 
(** Creating fresh bitvector variables for solver. 
 The index variable are always reset to the current value
 when solver is called. *)
let mk_fresh () = 
  Term.Var.mk_fresh Th.set None Var.Cnstrnt.Unconstrained
 

(** Solving equations over propositional set terms based on
  the equality of [ite(x, p, n)] and 
     [(p union n) & exists delta. x = (p inter (n implies delta))] *)
let rec solve (a, b) =
  solve1 (mk_iff a b) []

and solve1 a sl =
  try
    (match d_interp a with
      | Sym.Empty, [] -> raise Exc.Inconsistent
      | Sym.Full, [] -> sl
      | Sym.Ite, [x; pos; neg] -> 
	  let a' = mk_union pos neg in 
	    assert(not(Term.subterm x a'));
	    let sl' = compose (x, mk_inter pos (mk_imp neg (mk_fresh()))) sl in
	      solve1 a' sl'
      | _ -> failwith "Ill-formed propositional set term")
  with
      Not_found -> 
	compose (a, mk_full()) sl

and compose (x, b) sl =
  Term.Subst.compose apply (x, b) sl


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

open Sym


let is_interp = function
  | Term.App(sym, _, _) -> Sym.Coproduct.is sym
  | _ -> false

let is_pure = Term.is_pure Th.cop

let d_interp = function
  | Term.App(sym, [a], _) -> (Sym.Coproduct.get sym, a)
  | _ -> raise Not_found

let destruct a = 
 try Some(d_interp a) with Not_found -> None

let d_outl a =
  match d_interp a with
    | Sym.Out(Left), b -> b
    | _ -> raise Not_found

let d_outr a =
  match d_interp a with
    | Sym.Out(Right), b -> b
    | _ -> raise Not_found

let d_inr a =
  match d_interp a with
    | Sym.In(Right), b -> b
    | _ -> raise Not_found

let d_inl a =
  match d_interp a with
    | Sym.In(Left), b -> b
    | _ -> raise Not_found

let d_in a =
  match d_interp a with
    | Sym.In(d), b -> (d, b)
    | _ -> raise Not_found

let d_out a =
  match d_interp a with
    | Sym.Out(d), b -> (d, b)
    | _ -> raise Not_found


let mk_inl a =
  try 
    d_outl a 
  with 
      Not_found ->
	Term.App.mk_app Sym.Coproduct.mk_inl [a]

let mk_inr a =
  try 
    d_outr a 
  with 
      Not_found -> 
	Term.App.mk_app Sym.Coproduct.mk_inr [a]


let mk_outr a =
  try
    d_inr a 
  with
      Not_found -> 
	Term.App.mk_app Sym.Coproduct.mk_outr [a]

let mk_outl a =
  try
    d_inl a 
  with
      Not_found -> 
	Term.App.mk_app Sym.Coproduct.mk_outl [a]

let mk_inX = function Sym.Left -> mk_inl | Sym.Right -> mk_inr
let mk_outX = function Sym.Left -> mk_outl | Sym.Right -> mk_outr

(** Generalize injection. *)
let rec mk_inj i a = 
  if i <= 0 then
    mk_inl a
  else if i = 1 then
    mk_inr a
  else 
    mk_inr (mk_inj (i - 1) a)

(** Generalized coinjection. *)
let rec mk_out i a = 
  if i <= 0 then
    mk_outl a
  else if i = 1 then
    mk_outr a
  else 
    mk_outr (mk_out (i - 1) a)



(** Canonical forms *)
let sigma op l =
  match op, l with
    | Sym.In(Left), [x] -> mk_inl x 
    | Sym.In(Right), [x] -> mk_inr x 
    | Sym.Out(Left), [x] -> mk_outl x 
    | Sym.Out(Right), [x] -> mk_outr x 
    | _ -> assert false

 
(** Apply term transformer [f] at uninterpreted positions. *)
let rec map f a =
  try
    let op, x = d_interp a in
    let x' = map f x in
      if x == x' then a else sigma op [x']
  with
      Not_found -> f a


(** Replacing a variable with a term. *)
let apply (x, b) =
  let lookup y = if Term.eq x y then b else y in
    map lookup


(** Solving equalities. A configuration [(el, sl)] consists
  of unsolved equalities [el] and a partial solution [sl].
  Starting with the argument equality in canonical form
  and the empty solution set, the following rules are applied,
  whereby it is understood that all terms are kept in canonical form.
  - Triv : [a = a; S] ==> S
  - Inj= : [inX(a) = inX(b); S] ==> [a = b; S]
  - Inj/=: [inX(a) = inY(b); S] ==> [bot, if X/=Y].
  - Slv  : [inX(a) = b; S] ==> [a = outX(b); S]
  - Out= : [outX(a) = outX(b); S] ==> [a = b; S]
  - Out/=: [outX(a) = outY(b); S] ==> a = inX(outY(b)), if X/=Y.
  - Subst: [x = a; S] ==> [S o {x = a}] if x not in vars(a).
  - Bot  : [x = a; S] ==> bot if x in vars(a).
  With this, if one ends up with [x = e[x]], then it is still inconsistent
  since the destructor pattern on [x] is incompatible with the constructor
  pattern.
*)

let rec solve e =
  solvel ([e], [])

and solvel (el, sl) =
  match el with
    | [] -> sl
    | (a, b) :: el1 ->
	solve1 (a, b) (el1, sl)

and solve1 (a, b) (el, sl) = 
  if Term.eq a b then                              (* [Triv] *)
    solvel (el, sl)
  else 
    match destruct a, destruct b with
      | Some(Sym.In(x), a'), Some(Sym.In(y), b') -> 
	  if x = y then                            (* [Inj=] *)
	    solvel ((a', b') :: el, sl)          
	  else                                     (* [Inj/=] *)
	    raise Exc.Inconsistent              
      | Some(Sym.In(x), a'), Some _ ->             (* [Slv] left *)
	  solvel ((a', mk_outX x b) :: el, sl)
      | Some _, Some(Sym.In(x), b') ->             (* [Slv] right *)
	  solvel ((b', mk_outX x a) :: el, sl)
      | Some(Sym.Out(x), a'), Some(Sym.Out(y), b') -> 
	  if x = y then                            (* [Out=] *)
	    solvel ((a', b') :: el, sl)           
	  else                                     (* [Out/=] *) 
	    solvel ((a', mk_inX x b) :: el, sl)
      | None, Some _ -> 
	  if occurs a b then                       (* Bot *) 
	    raise Exc.Inconsistent
	  else 
	    solvel (install (a, b) (el, sl))
      | Some _, None -> 
	  if occurs b a then                       (* Bot *) 
	    raise Exc.Inconsistent
	  else 
	    solvel (install (b, a) (el, sl))
      | None, None -> 
	  solvel (install (Term.orient (a, b)) (el, sl))

and install (x, b) (el, sl) =
  assert(not(occurs x b));
  let el' = substitute (x, b) el                  (* Subst *)
  and sl' = Term.Subst.compose apply (x, b) sl in
    (el', sl')

and occurs x a =
  assert(not(is_interp x));
  try
    let (_, b) = d_interp a in
      occurs x b
  with
      Not_found -> Term.eq x a

and substitute (x, b) el =
  let apply2 (a1, a2) =
    (apply (x, b) a1, apply (x, b) a2)
  in
    List.map apply2 el


(** Check for disequalities. *)
let is_diseq a b =
  try
    let _ = solve (a, b) in
      false
  with
      Exc.Inconsistent -> true
 

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
  | Term.App(sym, _, _) -> (Sym.theory_of sym) = Th.cop
  | _ -> false

let is_pure = Term.is_pure Th.cop

let d_interp = function
  | Term.App(sym, [a], _) -> (Sym.Coproduct.get sym, a)
  | _ -> raise Not_found

let d_outl a =
  match d_interp a with
    | Sym.OutL, b -> b
    | _ -> raise Not_found

let d_outr a =
  match d_interp a with
    | Sym.OutR, b -> b
    | _ -> raise Not_found

let d_inr a =
  match d_interp a with
    | Sym.InR, b -> b
    | _ -> raise Not_found

let d_inl a =
  match d_interp a with
    | Sym.InL, b -> b
    | _ -> raise Not_found


let mk_inl a =
  try 
    d_outl a 
  with 
      Not_found -> Term.App.mk_app Sym.Coproduct.inl [a]

let mk_inr a =
  try 
    d_outr a 
  with 
      Not_found -> Term.App.mk_app Sym.Coproduct.inr [a]

let mk_outr a =
  try
    d_inr a 
  with
      Not_found -> Term.App.mk_app Sym.Coproduct.outr [a]

let mk_outl a =
  try
    d_inl a 
  with
      Not_found -> Term.App.mk_app Sym.Coproduct.outl [a]


(** Generalize injection. *)
let rec mk_inj i x = 
  if i <= 0 then
    mk_inl x
  else if i = 1 then
    mk_inr x
  else 
    mk_inr (mk_inj (i - 1) x)

(** Generalized coinjection. *)
let rec mk_out i x = 
  if i <= 0 then
    mk_outl x
  else if i = 1 then
    mk_outr x
  else 
    mk_outr (mk_out (i - 1) x)


(** Two chains of injections are disequal if they differ. *)
let rec is_diseq a b =
  try
    let (f, x) = d_interp a
    and (g, y) = d_interp b in
      (match f, g with
         | Sym.InL, Sym.InL -> is_diseq x y
         | Sym.InR, Sym.InR -> is_diseq x y
         | Sym.InL, Sym.InR -> true
         | Sym.InR, Sym.InL -> true
         | _ -> false)
  with
      Not_found -> false
 

(** Canonical forms *)
let sigma op l =
  match op, l with
    | Sym.InL, [x] -> mk_inl x 
    | Sym.InR, [x] -> mk_inr x 
    | Sym.OutL, [x] -> mk_outl x 
    | Sym.OutR, [x] -> mk_outr x 
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
  map (fun y -> if Term.eq x y then b else y)


(** Solving equalities. A configuration [(el, sl)] consists
  of unsolved equalities [el] and a partial solution [sl].
  Starting with the argument equality in canonical form
  and the empty solution set, the following rules are applied.
  - Triv   : [a = a, el; sl] ==> [el; sl]
  - In     : [inY(a) = b, el; sl] ==> [a = outY(b), el; sl] 
                for [Y in {L, R}].
  - Out    : [outY(a) = b, el; sl] ==> [a = inY(b), el; sl] 
                for [Y in {L, R}].
  - Orient : [a = y, el; sl] ==> [y = a, el; sl] 
                if [a] is not a variable.
  - Var    : [x = y, el; sl] ==> [el[x/y]; {x = y} |> sl ]
  - Bot    : [x = b, el; sl] ==> [bot] 
                if variable [x] is a proper subterm of [b].
  - Subst  : [x = b, el; sl] ==> [el[x/b]; {x = b} o sl] 
                if variable [x] does not occur in the nonvariable term [b].
*)

let rec solve e =
  solvel [e] []

and solvel el sl =
  match el with
    | [] -> sl
    | (a, b) :: el1 ->
	solve1 (a, b) el1 sl

and solve1 (a, b) el sl = 
  if Term.eq a b then            (* Triv *)
    solvel el sl
  else if Term.is_var b then   
    if Term.is_var a then        (* Var *)
      let el' = substitute (a, b) el in
      let sl' = fuse (a, b) el in
	solvel el' sl'
    else                          (* Orient *)
      solvevar (b, a) el sl
  else 
    try
      let (op, c) = d_interp a in 
      let rhs' =                 
	match op with 
	  | Sym.InL -> mk_outl b  (* In *)
	  | Sym.InR -> mk_outr b
	  | Sym.OutL -> mk_inl b  (* Out *)
	  | Sym.OutR -> mk_inr b
      in 
	solvel ((c, rhs') :: el) sl
    with
	Not_found -> 
	  solvevar (a, b) el sl
	  
and solvevar (x, b) el sl =
  assert(Term.is_var x);
  assert(not(Term.is_var b));
  if Term.occurs x b then          (* Bot *) 
    raise Exc.Inconsistent
  else 
    let el' = substitute (x, b) el (* Subst *)
    and sl' = compose (x, b) sl in
      solvel el' sl'
      
and compose (x, b) sl =
  Term.Subst.compose apply (x, b) sl

and fuse (x, b) sl =
  Term.Subst.fuse apply (x, b) sl

and substitute (x, b) el =
  let subst1 (a1, a2) =
    (apply (x, b) a1, apply (x, b) a2)
  in
    List.map subst1 el

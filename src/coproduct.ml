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
let theory = Theory.create "cop"

let is_theory = Theory.eq theory

let _ = 
  Theory.Description.add theory
    "Theory of coproducts (or direct sums).
     Signature: 
        inl, inr, outl, inl
     Axioms:
        inl(outl(t)) = t
        inr(outr(t)) = t
        outr(inr(t)) = t
        outl(inl(t)) = t
        inl(t) <> inr(t')
        inl(t) <> t
        inr(t) <> t"

type direction = Left | Right

(** Signature. *)
module Sig = struct
  let th = theory
  type t =
    | In of direction 
    | Out of direction
  let name = 
    let inl = Name.of_string "inl"
    and inr = Name.of_string "inr"
    and outl = Name.of_string "outl"
    and outr = Name.of_string "outr" in
      function 
	| In(Left) -> inl
	| In(Right) -> inr
	| Out(Left) -> outl
	| Out(Right) -> outr
end

module Op = Funsym.Make(Sig)

let op t = Op.out (Term.sym_of t)

let arg t = 
  if Term.is_unary t then
    Term.Args.get (Term.args_of t) 0
  else
    raise Not_found

let is_interp t =
  try 
    Op.is_interp (Term.sym_of t) &&
    Term.is_unary t
  with 
      Not_found -> true

let rec is_pure t =
  try is_interp t && is_pure (arg t) with Not_found -> Term.is_var t

let destruct t = 
 try Some(op t, arg t) with Not_found -> None

let d_out x t =
  match op t with 
    | Sig.Out(y) when x = y -> arg t 
    | _ -> raise Not_found

let d_in x t =
  match op t with 
    | Sig.In(y) when x = y -> arg t
    | _ -> raise Not_found

(** [in x (out x t) = t]. *)
let mk_in x t = 
  try d_out x t with Not_found -> 
    Term.mk_unary (Op.inj(Sig.In(x))) t

(** [out x (in x t) = t]. *)
let mk_out x t = 
  try d_in x t with Not_found -> 
    Term.mk_unary (Op.inj(Sig.Out(x))) t
				  

(** Iterated injection. *)
let rec mk_iterated_inj i t = 
  assert(i >= 0);
  if i <= 0 then
    mk_in Left t
  else if i = 1 then
    mk_in Right t
  else 
    mk_in Right (mk_iterated_inj (i - 1) t)


(** Iterated coinjection. *)
let rec mk_iterated_out i t =
  if i <= 0 then
    mk_out Left t
  else if i = 1 then
    mk_out Right t
  else 
    mk_out Right (mk_iterated_out (i - 1) t)


(** Canonical forms *)
let sigma op a =
  assert(Op.is_interp op);
  match Op.out op with
    | Sig.In(x) -> mk_in x (Term.Args.get a 0)
    | Sig.Out(x) -> mk_out x (Term.Args.get a 0)
 
(** Apply term transformer [f] at uninterpreted positions. *)
let rec map f a =
  try
    let op = op a and b = arg a in
    let b' = map f b in
      if b == b' then a else sigma (Op.inj op) (Term.Args.make1 b')
  with
      Not_found -> f a


(** Replacing a variable with a term. *)
let apply (x, b) =
  let lookup y = if Term.eq x y then b else y in
    map lookup

(** Test if unintepreted [x] occurs at an interpreted position of [t].
  For example, [occurs x 'inl(x)'] succeeds whereas [occurs x 'inl(f(x))'] fails. *)
let occurs x =
  assert(not(is_interp x));
  let rec occ t = 
    try occ (arg t) with Not_found -> Term.eq x t
  in
    occ

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

let solve = 
  let el = Stacks.create ()
  and sl = Term.Subst.empty () in
  let push e = Stacks.push e el in
  let add x t = Term.Subst.compose sl x t in
  let solve1 a b =
    if Term.eq a b then () else                             (* [Triv] *)
      match is_interp a, is_interp b with
	| false, false ->                                   (* [Var]. *)
	    if Term.compare a b <= 0 then add a b else add b a
	| false, true ->                                 (* Subst/Bot *)
	    if occurs a b then raise Exc.Inconsistent else add a b
	| true, false ->                                 (* Subst/Bot *)
	    if occurs b a then raise Exc.Inconsistent else add b a
	| true, true -> 
	    (match op a, op b with
	       | Sig.In(x), Sig.In(y) ->
		   if x = y then push (arg a, arg b) else   (* [Inj=] *)
		     raise Exc.Inconsistent                (* [Inj/=] *)
	       | Sig.In(x), Sig.Out _ -> 
		   push (arg a, mk_out x b)             (* [Slv] left *)
	       | Sig.Out _, Sig.In(x) ->                     
		   push (arg b, mk_out x a)            (* [Slv] right *)
	       | Sig.Out(x), Sig.Out(y) -> 
		   if x = y then push (arg a, arg b) else   (* [Out=] *)
		     push (arg a, mk_in x b))              (* [Out/=] *) 
  in
    fun t1 t2 -> 
      Stacks.clear el;
      Stacks.push (t1, t2) el;
      while not(Stacks.is_empty el) do
	let (t1, t2) = Stacks.pop el in
	  solve1 (Term.Subst.apply sl t1) (Term.Subst.apply sl t2)
      done;
      sl

(** Check for disequalities. *)
let rec is_diseq t1 t2 =
  try
    let _ = solve t1 t2 in
      false
  with
      Exc.Inconsistent -> true
 

(** Encoding of the theory of coproducts as a Shostak theory. *)
module T: Shostak.T = struct
  let th = theory
  let can = sigma
  let solve t1 t2 = Term.Set.empty(), solve t1 t2
  let disjunction _ = raise Not_found
end

(** Inference system for coproducts as an instance of a Shostak inference system. *)
module Component = Shostak.Make(T)




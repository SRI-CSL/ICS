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


(** Default normalization of application of constants. *)
let simplify = ref Term.App.mk_app

(** Apply [f] to [a] in a context where normalization [sgm]
  is used. *)
let with_simplifier sgm f a =
  let save = !simplify in
    try
      simplify := sgm;
      let b = f a in
	simplify := save;
	b
    with
	exc ->
	  simplify := save;
	  raise exc

let d_interp a =
  match a with
   | Term.App(sym, al, _) -> (Sym.Fun.get sym, al)
   | _ -> raise Not_found

let destruct a =
  try Some(d_interp a) with Not_found -> None

let d_abs a =
  match d_interp a with
    | Sym.Abs, [a1] -> a1
    | _ -> raise Not_found

let d_apply a =
  match d_interp a with
    | Sym.Apply, [a; b] -> (a, b)
    | _ -> raise Not_found

(** Test if [x] occurs at toplevel uninterpreted position. *)
let rec occurs x a =
  try
    let (_, bl) = d_interp a in
      List.exists (occurs x) bl
  with
      Not_found -> Term.eq x a

let is_pure = Term.is_pure Th.app



let mk_abs a = 
  Term.App.mk_app Sym.Fun.abs [a]

let rec mk_apply a b =
  try
    let x = d_abs a in
      byValue (subst x b 0)
  with
      Not_found ->
	Term.App.mk_app (Sym.Fun.apply) [a; b]


(** evaluation, not affecting function bodies *)
and eval a =
  try 
    let (x, y) = d_apply a in
    let x' = eval x in
      (try 
	 let z = d_abs x' in
	   eval (subst z (eval y) 0)
       with 
	   Not_found -> 
	     let y' = eval y in
	       if x' == x && y' == y then a else 
		      Term.App.mk_app Sym.Fun.apply [x'; y'])
  with
      Not_found -> a

(** normalization using call-by-value*)
and byValue a = 
  let rec bodies a =
    try
      match d_interp a with
	| Sym.Abs, [x] -> 
	    Term.App.mk_app Sym.Fun.abs [byValue x]
	| Sym.Apply, xl -> 
	    Term.App.mk_app Sym.Fun.apply (Term.mapl bodies xl)
	| _ -> a
      with
	  Not_found -> a
  in
    bodies (eval a)


(* Head normal form. *)

and hnf a =
  try
    match d_interp a with
      | Sym.Abs, [x] ->
	  let x' = hnf x in
	    if x == x' then a else 
	      Term.App.mk_app Sym.Fun.abs [x']
      | Sym.Apply, [x1; x2] ->
	  let z = hnf x1 in
	    (try
	       let y = d_abs z in
		 hnf (subst y x2 0)
	     with
		 Not_found -> 
		   if z == x1 then a else 
		     Term.App.mk_app Sym.Fun.apply [z; x2])
      | _ -> 
	  a
    with
	Not_found -> a

(* Normalization using call-by-name. *)

and byName a =
  let rec args a =
    try
      (match d_interp a with
	| Sym.Abs, [x] ->
	    let x' = args x in
	      if x == x' then a else 
		Term.App.mk_app Sym.Fun.abs [x']
	| Sym.Apply, x :: xl ->
	    let x' = args x 
	    and xl' = Term.mapl byName xl in
	      if x == x' && xl == xl' then a else 
		Term.App.mk_app (Sym.Fun.apply) (x' :: xl')
	| _ -> 
	    a)
    with
	Not_found -> a
  in
    args (hnf a)

and subst a s k =
  match a with
    | Term.Var(x, _) when Var.is_free x -> 
        let i = Var.d_free x in
          if k < i then 
            Term.Var.mk_free(i - 1)
          else if i = k then
            s
          else 
            Term.Var.mk_free i
    | Term.Var _ -> 
	a
    | Term.App(sym, [x], _) when Sym.Fun.is_abs sym ->
        mk_abs (subst x (lift s 0) (k + 1))
    | Term.App(f, xl, _) ->
	!simplify f (substl xl s k)

and substl al s k =
  Term.mapl (fun x -> subst x s k) al

and lift a k =
  match a with
    | Term.Var(x, _) when Var.is_free x ->
	let i = Var.d_free x in
	  if i < k then a else Term.Var.mk_free (i + 1)
    | Term.Var _ -> 
	a
    | Term.App(sym, [x], _) when Sym.Fun.is_abs sym ->
	mk_abs (lift x (k + 1))
    | Term.App(f, xl, _) ->
	Term.App.mk_app f (liftl xl k)

and liftl al k =
  Term.mapl (fun a -> lift a k) al

let sigma op al =
  match op, al with
    | Sym.Apply, [x; y] -> 
	mk_apply x y
    | Sym.Abs, [x] -> 
	mk_abs x
    | _ -> 
	assert false

let rec map f a =
  try
    (match d_interp a with
       | Sym.Apply, [x; y] ->
	   let x' = map f x and y' = map f y in
	     if x == x' && y == y' then a else
	       mk_apply x' y'
       | Sym.Abs, [x] ->
	   let x' = map f x in
	     if x == x' then a else mk_abs x'
       | _ ->
	   f a)
  with
      Not_found -> f a

(** Replacing a variable with a term. *)
let apply (x, b) = 
  map (fun y -> if Term.eq x y then b else y)


(** Solving equalities [a = b] for second-order variables [F] by applying 
  the following rules on configurations [(el; sl)] with [el] a set of equalities 
  and [sl] a substitution.  The starting configuration
  is [(a = b; [])] where [[]] represents the identity 
  substitution. All terms are considered to be in normal form.
  - Triv:   [a = a, el; sl] ==> [el; sl]
  - Bot:    [a = b, el; sl] ==> bot     
               if [a], [b] are pure lambda terms
  - Subst:  [F(a1,...,an) = b, el; sl] ==> [{theta}[el]; sl o {theta}]
               with [G] fresh, [theta := F = G[(a1,...,an) := b]]
  - Ext:    [lam(a) = lam(b), el; sl] ==> [lam(a) $ x = lam(b) $ x, el; sl]
               with [x] fresh
*)


let rec solve e =
  raise Exc.Incomplete              (* following is rather experimental *)
  (* solvel ([e], []) *)

and solvel (el, sl) =
  match el with
    | [] -> sl
    | (a, b) :: el1 ->
	solve1 (a, b) (el1, sl)

and solve1 (a, b) (el, sl) = 
  if Term.eq a b then                                   (* [Triv] *)
    solvel (el, sl)
  else if is_pure a && is_pure b then     
    raise Exc.Inconsistent                              (* [Bot] *)
  else if Term.is_var a && Term.is_var b  then          (* [Slv] *)
    solvel (install (Term.orient (a, b)) (el, sl))
  else if Term.is_var a && not(occurs a b) then
    solvel (install (a, b) (el, sl))
  else if Term.is_var b && not(occurs b a) then
    solvel (install (b, a) (el, sl))
  else 
    match destruct a, destruct b with
      | Some(Sym.Apply,[a; b]), Some(Sym.Apply,[c; d])-> (* [AppExt] *)
	  solvel ((a, c) :: (b, d) :: el, sl)
      | Some(Sym.Abs, [a]), Some(Sym.Abs, [b]) ->        (* [LamExt] *)
	  let x = mk_fresh () in
	    solvel (install (mk_apply a x, mk_apply b x) (el, sl))
      | Some(Sym.Apply, [x; a]), _ when Term.is_var x -> (* [Flex] left *)
	  let bnd0 = Term.Var.mk_free 0 in         
	  let y = mk_fresh () in
	  let theta = (x, failwith "to do") in
	    solvel (install theta (el, sl))
      | _, Some(Sym.Apply, [x; b]) when Term.is_var x -> (* [Flex] right *)
	  let bnd0 = Term.Var.mk_free 0 in         
	  let y = mk_fresh () in
	  let theta = (x, mk_abs (failwith "to do")) in
	    solvel (install theta (el, sl))
      | _ -> 
	  raise Exc.Incomplete


and install (x, a) (el, sl) =
  (substitute (x, a) el, compose (x, a) sl)
      
and compose (x, b) sl =
  Term.Subst.compose apply (x, b) sl

and substitute (x, b) el =
  let subst1 (a1, a2) =
    (apply (x, b) a1, apply (x, b) a2)
  in
    List.map subst1 el

and mk_fresh () =
  Term.Var.mk_fresh Th.app None Var.Cnstrnt.Unconstrained

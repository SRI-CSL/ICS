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

(** Operations on combinatory logic terms. *)

let d_interp a =
  match a with
   | Term.App(sym, al, _) -> (Sym.Cl.get sym, al)
   | _ -> raise Not_found

let is_interp = function
  | Term.App(sym, _, _) when Sym.Cl.is sym -> true
  | _ -> false

let destruct a =
  try Some(d_interp a) with Not_found -> None

let d_apply a =
  try
    match d_interp a with
      | Sym.Apply, op :: al -> (op, al)
      | _ -> (a, [])
    with
	Not_found -> (a, [])

let d_reify a =
  match d_interp a with
    | Sym.Reify(f, n), [] -> (f, n)
    | _ -> raise Not_found

let is_pure = Term.is_pure Th.app

let mk_s () = Term.App.mk_const Sym.Cl.s
let mk_k () = Term.App.mk_const Sym.Cl.k
let mk_i () = Term.App.mk_const Sym.Cl.i
let mk_c () = Term.App.mk_const Sym.Cl.c
let mk_reify (f, n) = Term.App.mk_const (Sym.Cl.reify (f, n))

let is_s = function
  | Term.App(sym, [], _) when Sym.Cl.is_s sym -> true
  | _ -> false

let is_k = function
  | Term.App(sym, [], _) when Sym.Cl.is_k sym -> true
  | _ -> false

let is_i = function
  | Term.App(sym, [], _) when Sym.Cl.is_i sym -> true
  | _ -> false

let is_c = function
  | Term.App(sym, [], _) when Sym.Cl.is_c sym -> true
  | _ -> false

let is_redex_atom = function
  | Term.App(sym, [], _) 
      when Sym.Cl.is_s sym
	|| Sym.Cl.is_k sym 
	|| Sym.Cl.is_i sym 
	|| Sym.Cl.is_c sym -> true
  | _ -> false

let is_reify a n =
  match a with
    | Term.App(sym, [], _) ->
	(try
	   let (_, m) = Sym.Cl.d_reify sym in
	     n = m
	 with
	     Not_found -> false)
    | _ -> 
	false

let reflection_enabled = ref true

(** Reductions
  - [I a = a],
  - [K b a = b]
  - [S c b a = (c $ a) $ (b $ a)]
  - [C d c b a = b] when [d = c]
  - [C d c b a = b] when [d <> c]
  - [C d c (z b') (z a')  = z a'] when [d = a'], [c = b']
  - [C d c b a = a] if [d = a], [c = b]
  - [C d c b a = c] if [a = b]
  - reflection, if enabled *)
let rec mk_apply op a =
  if is_i op then a else 
    match d_apply op with
      | k', [b] 
	  when is_k k' -> 
	  b
      | s', [c; b] 
	  when is_s s' ->
	  mk_apply (mk_apply c a) (mk_apply b a)
      | c', [d; c; b] 
	  when is_c c' ->
	  if Term.eq a b then a
	  else (match Term.is_equal c d with
	    | Three.Yes -> b
	    | Three.No -> a
	    | Three.X -> 
		if Term.eq d a && Term.eq c b then a
		else 
		  (match d_apply b, d_apply a with
		     | (z, [a']), (z', [b'])
			 when Term.eq z z'
			   && Term.eq d a'
			   && Term.eq c b' -> mk_apply z a'
		     | _ ->
			 Term.App.mk_app Sym.Cl.apply [c'; d; c; b; a]))    
      | f', bl ->
	  (try
	     if not(!reflection_enabled) then
	       raise Not_found;
	     let (op', n') = d_reify f' in
	       if n' = List.length bl + 1 then
		 reflect op' (bl @ [a])
	       else
		 raise Not_found
	   with
	       Not_found -> 
		 Term.App.mk_app Sym.Cl.apply ([f'] @ bl @ [a]))
	

and mk_apply2 op a1 a2 =
  mk_apply (mk_apply op a1) a2

and mk_apply3 op a1 a2 a3 =
  mk_apply (mk_apply (mk_apply op a1) a2) a3 


and mk_apply_star op = function
  | [] -> op
  | a :: al -> 
      let op' = mk_apply op a in
	mk_apply_star op' al

and mk_cases a b c d =
  mk_apply (mk_apply (mk_apply (mk_apply (mk_c()) a) b) c) d


and sigma op al =
  Trace.msg "foo32" "Cl.sigma" (op, al) (Sym.Cl.pp Term.pp);
  match op, al with
    | Sym.Apply, [x; y] -> mk_apply x y
    | Sym.S, [] -> mk_s ()
    | Sym.K, [] -> mk_k ()
    | Sym.I, [] -> mk_i ()
    | Sym.C, [] -> mk_c ()
    | Sym.Reify(f, n), [] -> mk_reify (f, n)
    | Sym.C, [a; b; c; d] -> mk_cases a b c d
    | Sym.S, [a; b; c] -> mk_apply3 (mk_s()) a b c
    | Sym.K, [a; b] -> mk_apply2 (mk_k()) a b
    | Sym.I, [a] -> mk_apply (mk_i()) a
    | Sym.Apply, [c; a1; a2; a3; a4] when is_c(c) -> mk_cases a1 a2 a3 a4
    | Sym.Apply, [s; a1; a2; a3] when is_s(s) -> mk_apply3 (mk_s()) a1 a2 a3
    | Sym.Apply, [k; a1; a2] when is_k(k) -> mk_apply2 (mk_k()) a1 a2
    | _ ->
	assert false

(** Build a first-order application of function symbol [f]. *)
and reflect f al =  
  match Sym.get f with
    | Sym.Arith(op) -> Arith.sigma op al
    | Sym.Product(op) -> Product.sigma op al
    | Sym.Bv(op) -> Bitvector.sigma op al
    | Sym.Coproduct(op) -> Coproduct.sigma op al
    | Sym.Propset(op) -> Propset.sigma op al
    | Sym.Cl(op) -> sigma op al
    | Sym.Pp(op) -> Pprod.sigma op al
    | Sym.Uninterp _ -> Term.App.mk_app f al
    | Sym.Arrays(op) -> Funarr.sigma Term.is_equal op al


(** Build a higher-order application of function symbol [f].
  Thus reflection must be disabled here. *)
and reify f al = 
  let n = List.length al in
  let op = mk_reify (f, n) in
    try
      reflection_enabled := false;
      let b = mk_apply_star op al in
	reflection_enabled := true;
	b
    with
	exc -> 
	  reflection_enabled := true;
	  raise exc


let d_binary_apply a =
  let rec app acc = function
    | [] -> raise Not_found
    | [c] -> (acc, c)
    | c :: cl -> 
	let acc' = mk_apply acc c in
	  app acc' cl
  in
  let (b, cl) = d_apply a in
    app b cl


let abstract x = 
  let is_binding a =
    try
      (match Term.Var.d_external a with
	 | y, Var.Cnstrnt.Unconstrained when Name.eq x y -> true
	 | _ -> false)
    with
	Not_found -> false
  in
  let rec occurs_bound a =        (* check also for 'uninterpreted' occurrences. *)
    is_binding a ||
    try
      let args = Term.App.args_of a in
	List.exists occurs_bound args
    with
	Not_found -> false
  in
  let rec abst a =
    Trace.msg "bar" "Abst(x)" a Term.pp; 
    if is_binding a then                (* [[x]x = I] *)
      mk_i()
    else if not(occurs_bound a) then    (* [[x]a = K a] if [x] notin [a]. *)
      mk_apply (mk_k()) a
    else 
      try
	let (a1, a2) = d_binary_apply a in
	  if is_binding a2 && not(occurs_bound a1) then
	    a1                          (* [[x]a1 x = a1] if [x] notin [a1]. *)
	  else 
	    mk_apply2 (mk_s()) (abst a1) (abst a2)
      with                              (* [[x]a1 a2 = S ([x]a1) ([x]a2)] *)
	  Not_found -> 
	    assert(Term.is_app a);            (* reify first-order terms *)
	    let (f, al) = Term.App.destruct a in
	      abst (reify f al)
  in
    abst

let abstract = Trace.func2 "bar" "Abstract" Name.pp Term.pp Term.pp abstract

(** Propagate a disequalities [x <> y]. *)
let disapply (x, y) a =
  try
    (match d_interp a with
       | Sym.Apply, [c; a1; a2; _; b] when is_c c ->
	   if Term.eq x a1 && Term.eq y a2
	     || Term.eq x a2 && Term.eq y a1 
	   then
	     b
	   else 
	     a
       | _ ->
	   a)
  with
      Not_found -> a
	     
	       


let rec map f a =
  try
    (match d_interp a with
       | Sym.Apply, [x; y] ->
	   let x' = map f x and y' = map f y in
	     if x == x' && y == y' then a else
	       mk_apply x' y'
       | Sym.Apply, op :: al ->
	   let op' = map f op in
	   let al' = Term.mapl (map f) al in
	     if op == op' && al == al' then a else 
	       mk_apply_star op' al'
       | _ , [] ->
	   a
       | _ ->
	   f a)
  with
      Not_found -> f a

(** Replacing a variable with a term. *)
let apply (x, b) = 
  map (fun y -> if Term.eq x y then b else y)


(** Terms of the following form are said to be {i functional}.
  - [I], 
  - [K], [K $ a], 
  - [S], [S $ a], [S $ b $ a],
  - [C], [C $ a], [C $ b $ a], [C $ c $ b $ a], and
  - [((('fn' $ a1) $ a2) $ ... $ am)] with [m < n] and ['fn'] a
    reified constant of arity [n]  *)
let rec is_functional a =
  let (op, al) = d_apply a in
    try
      let n = arity op
      and m = List.length al in
	m < n
    with
	Not_found -> false
   
and arity op =
  match d_interp op with
    | Sym.S, [] -> 3
    | Sym.K, [] -> 2
    | Sym.I, [] -> 1
    | Sym.C, [] -> 4
    | Sym.Reify(_, n), [] -> n
    | _ -> raise Not_found
    


let fresh = ref []

let mk_fresh () =
  let x = Term.Var.mk_fresh Th.app None Var.Cnstrnt.Unconstrained in
    fresh := x :: !fresh; x

let is_fresh x = 
  let eqx y = Term.eq x y in
    List.exists eqx !fresh

(** Test if [a] consists only of combinators and fresh variables. *)
let rec is_pure a =
  try
    let (_, al) = d_interp a in
      List.for_all is_pure al
  with
      Not_found -> is_fresh a

let is_fo_app = function 
  | Term.App(f, _, _) when not(Sym.Cl.is_apply f) -> true
  | _ -> false


let z() = Name.of_string  "____@@@z###$____"
let mk_bound () = 
  Term.Var.mk_var (z()) Var.Cnstrnt.Unconstrained

(** Ordinary variables and reified non-constants are {i flexible}. *)
let is_flexible x =
  Term.is_var x ||
  try
    let (_, n) = d_reify x in
      n > 0
  with
      Not_found -> false
      

(** Solving equalities [a = b] for second-order variables [F] by applying 
  the following rules on configurations [(el; sl)] with [el] a set of equalities 
  and [sl] a substitution.  The starting configuration
  is [(a = b; [])] where [[]] represents the identity 
  substitution. All terms are considered to be in normal form.
  - Triv:   [a = a, el; sl] ==> [el; sl]
  - ...
*)


let rec solve e =
  fresh := [];
  solvel ([e], [])

and solvel (el, sl) =
  match el with
    | [] -> sl
    | (a, b) :: el1 -> solve1 (a, b) (el1, sl)

and solve1 (a, b) (el, sl) = 
  Trace.msg "l" "Solve" (a, b) Term.Equal.pp;
  if Term.eq a b then                        (* [Triv] *)
    solvel (el, sl)
  else if is_functional a || is_functional b then
    let d = mk_fresh () in                   (* [Ext] *)
    let e = (mk_apply a d, mk_apply b d) in
      solvel (e :: el, sl)
  else if is_fo_app a || is_fo_app b then
    let sl' = solve_fo (a, b) in
      solvel (List.fold_right install sl' (el, sl))
  else 
    let (f, al) = d_apply a
    and (g, bl) = d_apply b in               (* [Decompose] *)
      if Term.eq f g && 
	is_redex_atom f && 
	List.length al = List.length bl 
      then
	let el' = List.combine al bl in
	  solvel (el' @ el, sl)
      else if is_pure a && is_pure b then
	raise Exc.Inconsistent
      else if not(Term.occurs f b) then
	solvel (install (imitate f al b) (el, sl))
      else if not(Term.occurs g a) then
	solvel (install (imitate g bl a) (el, sl))
      else 
	raise Exc.Incomplete

and solve_fo ((a, b) as e) = 
  let shostak_theory_of a =
    match Term.App.theory_of a with
      | Th.Shostak(i) -> i
      | _ -> raise Not_found
  in
    try
      let i = shostak_theory_of a in
	solve_interp i (a, b)
    with
	Not_found -> 
	  let j = shostak_theory_of b in
	    solve_interp j (a, b)
		
and solve_interp i e =
    match i with
      | Th.LA -> Arith.solve e
      | Th.BV -> Bitvector.solve e
      | Th.P -> Product.solve e
      | Th.COP -> Coproduct.solve e
      | Th.SET -> Propset.solve e
      | _ -> invalid_arg "no solving for CL terms"

and imitate x al b =
  assert(not(Term.occurs x b));
  match al with
    | [] -> (x, b)
    | [a] -> 
	let y = mk_fresh () in
	let zb = mk_bound () in
	  (x, abstract (z()) (mk_cases zb a b (mk_apply y zb)))
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
 

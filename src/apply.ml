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
let theory = Theory.create "cl" 

let _ = 
  Theory.Description.add theory
    "Theory of combinatory logic.
     Signature:
       a $ b : application of 'a' to 'b'
       I     : identity combinator
       K     : first projection combinator
       S     : 'S' combinator
       C     : conditional combinator
       'f'   : Family of reification combinators indexed by function symbols
     Axioms: (a $ b) abbreviated as juxtaposition 'a b'
                     I x = x
                   K x y = x
                 S x y z = (x z) (y z)
               C x x a b = a
               C x y a b = b    if x <> y
               C x y a a = a
       C x y (z y) (z x) = z x
               C x y y x = x   
   "

module Combinator = struct

  type t =  S | K | I | C | Reify of Funsym.t * int

  let name = 
    let s = Name.of_string "S"
    and k = Name.of_string "K"
    and i = Name.of_string "I"
    and c = Name.of_string "C" in
      function
	| S -> s
	| K -> k
	| I -> i
	| C -> c
	| Reify(f, _) ->
	    Name.of_string (Format.sprintf "'%s'" (Funsym.to_string f))

  let arity = function
    | S -> 3
    | K -> 2
    | I -> 1
    | C -> 4
    | Reify(_, n) -> n

  let maxarity = 4
	
end


(** Interpreted function symbols in theory [l]. *)
module Sig = struct

  let th = theory

  type t = 
    | Apply
    | Combinator of Combinator.t

  let name = 
    let app = Name.of_string "$" in
      function
	| Apply -> app 
	| Combinator(c) -> Combinator.name c

end

module Op = struct

  module O = Funsym.Make(Sig)

  let inj = O.inj
  let out = O.out
  let is_interp = O.is_interp
	    
  let mk_apply = O.inj(Sig.Apply)

  let mk_s = O.inj(Sig.Combinator(Combinator.S))
  let mk_k = O.inj(Sig.Combinator(Combinator.K))
  let mk_i = O.inj(Sig.Combinator(Combinator.I))
  let mk_c = O.inj(Sig.Combinator(Combinator.C))
  let mk_reify f n = O.inj(Sig.Combinator(Combinator.Reify(f, n)))

  let mk_combinator = function
    | Combinator.S -> mk_s
    | Combinator.K -> mk_k
    | Combinator.I -> mk_i
    | Combinator.C -> mk_c
    | Combinator.Reify(f, n) -> mk_reify f n

  let rec arity = function
    | Sig.Apply -> 2
    | Sig.Combinator(c) -> Combinator.arity c

end 

let op t = Op.out (Term.sym_of t)

let combinator t = 
  match op t with
    | Sig.Combinator(c) -> c
    | Sig.Apply -> raise Not_found
	
let args = Term.args_of
	     
let is_apply t =
  try
    (match op t with
       | Sig.Apply -> Term.is_binary t
       | _ -> false)
  with
      Not_found -> false

let arg1 t =
  assert(is_apply t);
  Term.Args.get (Term.args_of t) 0

let arg2 t =
  assert(is_apply t);
  Term.Args.get (Term.args_of t) 1

let d_arg1 t = 
  match op t with
    | Sig.Apply -> arg1 t
    | _ -> raise Not_found

let d_arg2 t = 
  match op t with
    | Sig.Apply -> arg2 t
    | _ -> raise Not_found

let is_interp t =
  try
    (match op t with
       | Sig.Combinator _ -> Term.is_const t
       | Sig.Apply -> Term.is_binary t)
  with
      Not_found -> false
	
let rec is_pure t =
  try
    (match op t with
       | Sig.Combinator _ -> true
       | Sig.Apply -> is_pure (arg1 t) && is_pure (arg2 t))
  with
      Not_found -> false

let is_combinator c t = 
  try
    (match op t with
       | Sig.Combinator(d) when c = d -> true
       | _ -> false)
  with
      Not_found -> false

let is_s = is_combinator Combinator.S
let is_k = is_combinator Combinator.K
let is_i = is_combinator Combinator.I
let is_c = is_combinator Combinator.C

let is_reify t n =
  try
    (match combinator t with
       | Combinator.Reify(_, m) when n == m -> true
       | _ -> false)
  with
      Not_found -> false

let pp fmt f a = 
  assert(Op.is_interp f);
  match Op.out f with
    | Sig.Apply -> 
        Format.fprintf fmt "@[(";
	Term.pp fmt (Term.Args.get a 0);
	Format.fprintf fmt " $ ";
	Term.pp fmt (Term.Args.get a 1);
        Format.fprintf fmt ")@]";
    | Sig.Combinator(c) ->
	Format.fprintf fmt "%s" (Name.to_string (Combinator.name c))

let destruct t =
  if is_interp t then Some(op t) else None

let d_reify t =
  match combinator t with
    | Combinator.Reify(f, n) -> (f, n)
    | _ -> raise Not_found

let mk_combinator c = 
  Term.mk_const (Op.mk_combinator c)

let mk_s () = Term.mk_const Op.mk_s
let mk_k () = Term.mk_const Op.mk_k
let mk_i () = Term.mk_const Op.mk_i
let mk_c () = Term.mk_const Op.mk_c
let mk_reify (f, n) = Term.mk_const (Op.mk_reify f n)

(** [num_of args '(...((f $ a1) $ a2) $ ...) $ an'] equal [n]. *)
let num_of_args = 
  let rec loop n t =
    try loop (n + 1) (d_arg1 t) with Not_found -> n
  in
    loop 0

(** Reductions
  - [I $ x = x],
  - [(K $ x) $ y = x]
  - [((S $ x) $ y) $ z = (x $ z) $ (y $ z)]
  - [C $ x $ x $ a $ b = a]
  - [C $ x $ y $ a $ b = b] if [x <> y]
  - [C $ x $ y $ (z $ y) $ (z $ x) = z $ x]
  - [C $ x $ y $ y $ x = x]   
  - [C $ x $ y $ a $ a = a]
  - reflection, if enabled *)
let rec mk_apply = 
  let args = Stacks.create () in
  let rec pattern n t = 
    if n > Combinator.maxarity then raise Not_found else
      match op t with
	| Sig.Apply -> 
	    Stacks.push (arg2 t) args;
	    pattern (n + 1) (arg1 t)
	| Sig.Combinator(c) -> 
	    if Combinator.arity c = n then c else raise Not_found
  in
    (fun t1 t2 -> 
      Stacks.clear args;
      try
	let c = pattern 0 t1 in
	  assert(Combinator.arity c = Stacks.length args);
	  (match c with
	    | Combinator.I -> 
		Stacks.pop args
	    | Combinator.K -> 
		Stacks.pop args
	    | Combinator.S -> 
		let x = Stacks.pop args in
		let y = Stacks.pop args in
		let z = Stacks.pop args in
		  mk_apply (mk_apply x z) (mk_apply y z)
	    | Combinator.C -> 
		let x = Stacks.pop args in
		let y = Stacks.pop args in
		let a = Stacks.pop args in
		let b = Stacks.pop args in
		  if Term.eq a b then a else
		    (match Term.is_equal x y with
		       | Three.Yes -> a
		       | Three.No -> b
		       | Three.X -> 
			   Term.mk_binary Op.mk_apply t1 t2)
	    | Combinator.Reify(f, n) -> 
		Term.sigma f (Term.Args.of_stack n args))
      with
	  Not_found -> 
	    Term.mk_binary Op.mk_apply t1 t2)

let is_redex_atom a = 
  is_s a || is_k a || is_i a || is_c a

let mk_cases a b c d =
  mk_apply (mk_apply (mk_apply (mk_apply (mk_c()) a) b) c) d

let sigma f a =
  assert(Op.is_interp f);
  assert(Term.Args.length a = Op.arity (Op.out f));
  match Op.out f with
    | Sig.Apply -> mk_apply (Term.Args.get a 0) (Term.Args.get a 1)
    | Sig.Combinator _ -> Term.mk_const f

(** Build a higher-order application for 
  `first-order' function symbol [f]. *)
let reify f a = 
  assert(not(Op.is_interp f));
  let n = Term.Args.length a in
  let rec apply i acc =
    if i = 0 then acc else
      let i' = i - 1 in
      let ai = Term.Args.get a i' in
      let acc' = Term.mk_binary Op.mk_apply acc ai in
	apply i' acc'
  in
    apply n (mk_reify(f, n))

let abstract x = 
  assert(Term.is_var x);
  let rec abst t =
    if Term.eq x t then                (* [[x]x = I] *)
      mk_i()
    else if not(Term.occurs x t) then  (* [[x]t = K t] if [x] notin [t]. *)
      mk_apply (mk_k()) t
    else 
      try
	let t1 = d_arg1 t and t2 = d_arg2 t in
	  if Term.eq x t2 && not(Term.occurs x t1) then
	    t1                         (* [[x]t1 x = t1] if [x] notin [t1]. *)
	  else 
	    mk_apply (mk_apply (mk_s()) (abst t1)) (abst t2)
      with                             (* [[x]t1 t2 = S ([x]t1) ([x]t2)] *)
	  Not_found -> 
	    assert(Term.is_app t);     (* reify first-order terms *)
	    abst (reify (Term.sym_of t) (Term.args_of t))
  in
    abst
	         
let map f =
  let rec mapf t = 
    try
      (match op t with
	 | Sig.Apply ->
	     let x = d_arg1 t and y = d_arg2 t in
	     let x' = mapf x and y' = mapf y in
	       if x == x' && y == y' then t else mk_apply x' y'
	 | Sig.Combinator _ -> 
	     t)
    with
	Not_found -> f t
  in
    mapf 

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
let rec is_functional t =
  try 
    let f = op t in
      num_of_args t < Op.arity f
  with
      Not_found -> false


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
  - Triv:  [t = t, el; sl] ==> [el; sl]
  - Dec :  [c $ x1 $ ... $ xn = c $ y1 $ ... $ yn, el; sl] ==> [x1 = y1,...,xn = yn, el; sl]
  - Ext :  [t1 = t2, el; sl] ==> [t1 $ z = t2 $ z] if [t1] is functional ([z] fresh)
  - Imit:  [t1 $ t2 = s, el; sl] ==> [t1 = lam[z]C $ z $ s $ t $ (y $ z), el; sl]  
               if [t1] not in [subterms[t]], [y] fresh.
*)

let solve =
  let el = Stacks.create () in
  let push e = Stacks.push e el in
    (fun t1 t2 -> 
       let sl = Term.Subst.empty () in
       let fresh = Term.Set.empty () in
       let rec solve1 t1 t2 =
	 if Term.eq t1 t2 then () else
	   try ext t1 t2 with Not_found -> 
	     try first_order t1 t2 with Not_found -> 
	       try decompose t1 t2 with Not_found -> 
		 if is_pure t1 && is_pure t2 then raise Exc.Inconsistent else
		   try imitate t1 t2 with Not_found -> 
		     try imitate t2 t1 with Not_found -> 
		       raise Exc.Incomplete
       and ext t1 t2 = 
	 if not(is_functional t1) && not(is_functional t2) then raise Not_found else
	   let x = Term.mk_fresh_var "e" in  
	     Term.Set.add x fresh;
	     push (mk_apply t1 x, mk_apply t2 x)
       and first_order t1 t2 = 
	 let is_fo_app t = failwith "to do" in
           if not(is_fo_app t1) && not(is_fo_app t2) then raise Not_found else
	     try Term.Subst.iter (fun x a -> push (x, a)) (Term.solve t1 t2) with
	       | Not_found -> raise Exc.Incomplete
	       | Exc.Valid -> ()
       and decompose t1 t2 =
	 let rec dec acc t1 t2 =
	   try
	     let acc' = (d_arg2 t1, d_arg2 t2) :: acc in
	       dec acc' (d_arg1 t1) (d_arg1 t2)
	   with
	       Not_found ->
		 if Term.eq t1 t2 && is_redex_atom t1 then 
		   List.iter push acc
		 else
		   raise Not_found
	 in 
	   dec [] t1 t2
       and imitate t1 t2 =
	 let f = d_arg1 t1 in
	   if Term.occurs f t2 then raise Not_found else 
	     let a = d_arg2 t1 in
	     let y = Term.mk_fresh_var "a" in
	     let z = Term.mk_fresh_var "bnd" in
	       Term.Set.add y fresh;
	       push (f, abstract z (mk_cases z a t2 (mk_apply y z)))
       in
	 Stacks.clear el;
	 Stacks.push (t1, t2) el;
	 while not(Stacks.is_empty el) do
	   let t1, t2 = Stacks.pop el in
	     solve1 (Term.Subst.apply sl t1) (Term.Subst.apply sl t2)
	 done;
	 (fresh, sl))

(** Check for disequalities. *)
let rec is_diseq t1 t2 =
  try
    let _ = solve t1 t2 in
      false
  with
      Exc.Inconsistent -> true

let _ = 
  let m = Term.Methods.empty() in
    m.Term.Methods.printer <- Some(pp);
    m.Term.Methods.can <- Some(sigma);
    m.Term.Methods.is_diseq <- Some(is_diseq);
 (*   m.Term.Methods.solve <- Some(solve); *)
    Term.Methods.register theory m



(** {6 Inference System} *)

module T: Shostak.T = struct
  let th = theory
  let can = sigma
  let solve = solve
   
  exception Found of Term.t * Term.t

  let disjunction iter =
    raise Not_found
(*
    let rec split_in_term a =
      match op a, args a with
	| Sig.Combinator(Sig.C), [b; c; _; _] -> 
	    raise(Found(b, c))
	| _, al ->
	    List.iter split_in_term al
    in
      try
	split_in_term a;
	raise Not_found
      with
	  Found(b, c) -> 
	    let e = Atom.mk_equal (b, c)
	    and d = Atom.mk_diseq (b, c) in
	      Clause.of_list ([e; d],Jst.dep0)
*)

end

(*
module Infsys: Shostak.INFSYS = struct

  module I = Shostak.Infsys(T)

  let reset = I.reset

  let current = I.current
  let initialize = I.initialize
  let finalize = I.finalize
  let is_unchanged = I.is_unchanged

  (** Replace foreign first-order terms with variables. *)
  let rec export () = ()
(*
    Solset.iter export1 (current())
*)

  and export1 x (a, rho) = ()
(*
    if is_first_order a then
      begin
	G.Infsys.put (Jst.Fact.of_equal (x, a, rho));
	(* Solset.restrict (current()) x  *)
	()
      end 

  and is_first_order a =
    try
      let i = Term.theory_of a in
	not(Theory.eq i theory)
    with
	Not_found -> false
*)

  let abstract = I.abstract

  let branch = I.branch
 
  let normalize () = 
    I.normalize ();
    export ()
	
   (** Apply disequality [x <> y] to [C x y a b]. *)
  let propagate_diseq d = 
    I.propagate_diseq d
(*
    let (x, y, rho) = d in
    let disapply (z, a, tau) =
      let a' = Apply.disapply (x, y) a in
	if a == a' then () else 
	  let e' = Fact.Equal.make z a' (Jst.dep2 rho tau) in
	    failwith "l: to do"
(*
	    S.update (!p, current()) e'
*)
    in
      failwith "l: to do"
(*
      S.Dep.iter (current()) disapply x;
      S.Dep.iter (current()) disapply y;
      export ()
*)
*)

  let propagate_equal = I.propagate_equal
  let propagate_diseq = I.propagate_diseq

  let process_equal = I.process_equal
  let process_diseq = I.process_diseq
			  
end
*)

module Component = 
  Shostak.Make(T)

(*
module Unit = 
  E.Register(Component)
*)

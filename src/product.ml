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


let is_pure = Term.is_pure Th.p

let is_interp = function
  | Term.App(sym, _, _) -> Sym.theory_of sym = Th.p
  | _ -> false

let is_cons = function
  | Term.App(sym, [_;_], _) when Sym.Product.is_cons sym -> true
  | _ -> false

let d_interp = function
  | Term.App(sym, al, _) -> (Sym.Product.get sym, al)
  | _ -> raise Not_found

let d_cons a =
  match d_interp a with
    | Sym.Cons, [a; b] -> (a, b)
    | _ -> raise Not_found

let d_car a =
  match d_interp a with
    | Sym.Car, [a] -> a
    | _ -> raise Not_found

let d_cdr a =
  match d_interp a with
    | Sym.Cdr, [a] -> a
    | _ -> raise Not_found


(** [cons(car(x), cdr(x))] reduces to [x]. *)
let mk_cons a b = 
  try
    let x = d_car a and x' = d_cdr b in
      if Term.eq x x' then x else raise Not_found
  with
      Not_found -> 
	Term.App.mk_app Sym.Product.mk_cons [a; b]


(** [car(cons(a, _))] reduces to [a]. *)
let mk_car a = 
  try
    fst(d_cons a)
  with
      Not_found -> 
	Term.App.mk_app Sym.Product.mk_car [a]
  

(** [cdr(cons(_, b))] reduces to [b]. *)
let mk_cdr a =
  try
    snd(d_cons a)
  with
      Not_found -> 
	Term.App.mk_app Sym.Product.mk_cdr [a]


(** Tuple constructors *)
let rec mk_tuple = function
  | [a] -> a
  | [a; b] -> mk_cons a b
  | a :: al -> mk_cons a (mk_tuple al)
  | [] -> assert false

(** Generalized projection *)
let rec mk_proj i a =
  if i <= 0 then 
    mk_car a 
  else if i = 1 then 
    mk_cdr a
  else 
    mk_cdr (mk_proj (i - 1) a)


(** Apply term transformer [f] at uninterpreted positions. *)
let rec map f a =
  try
     (match d_interp a with
	| Sym.Cons, [b1; b2] ->
	    let b1' = map f b1
	    and b2' = map f b2 in
	      if b1 == b1' && b2 == b2' then a else mk_cons b1' b2'
	| Sym.Car, [b] ->
	    let b' = map f b in
	      if b == b' then a else mk_car b'
	| Sym.Cdr, [b] ->
	    let b' = map f b in
	      if b == b' then a else mk_cdr b'
	| _ ->
	    f a)
  with
      Not_found -> f a


let apply (x, b) =
  map (fun y -> if Term.eq x y then b else y)


let sigma op l =
  match op, l with
    | Sym.Cons, [a; b] -> mk_cons a b
    | Sym.Car, [a] -> mk_car a
    | Sym.Cdr, [b] -> mk_cdr b 
    | _ -> assert false


(** Reinitialized by solver. *)
let fresh = ref []

(** Fresh variables. *)
let mk_fresh () =
  let x = Term.Var.mk_fresh Th.p None Var.Cnstrnt.Unconstrained in
    fresh := x :: !fresh;
    x

let is_fresh x =
  let eqx = Term.eq x in
    List.exists eqx !fresh


(** Symbol table for renamings. *)
module Symtab = struct

  type t = (Term.t * (Term.t * Term.t)) list
      (** Bindings [x |-> (k1, k2)] represent the equalities
	[k1 = car(x)] and [k2 = cdr(y)]. *)

  let empty = []

  let pp =
    Pretty.map Term.pp (Pretty.pair Term.pp Term.pp)
      
  let lookup rho x =
    let rec search = function
      | [] -> raise Not_found
      | (y, (k1, k2)) :: l -> if Term.eq x y then (k1, k2) else search l
    in
      search rho 

  let fold f =
    let f' (x, (k1, k2)) = f (x, k1, k2) in
      List.fold_right f'

  (** replace [car(x)], [cdr(x)] in [a] inside out renaming context [rho] *)
  let rec abstract (a, rho) =
    try
      (match d_interp a with
	 | Sym.Cons, [b1; b2] ->
	     let b1', rho' = abstract (b1, rho) in
	     let b2', rho'' = abstract (b2, rho') in
	       (mk_cons b1' b2', rho'')
	 | Sym.Car, [b] ->
	     let (x, rho') = abstract (b, rho) in
	     let (k1, _, rho'') = represents x rho' in (* [k1 = car(x)] *)
	       (k1, rho'')
	 | Sym.Cdr, [b] -> 
	     let (x, rho') = abstract (b, rho) in
	     let (_, k2, rho'') = represents x rho' in (* [k2 = cdr(x)] *)
	       (k2, rho'')
	 | _ ->
	     (a, rho))
    with
	Not_found -> (a, rho)

  and represents x rho = 
    try
      let (k1, k2) = lookup rho x in
	(k1, k2, rho)
    with
	Not_found ->
	  let k1 = mk_fresh () and k2 = mk_fresh () in
	  let rho' = (x, (k1, k2)) :: rho in
	    (k1, k2, rho')
  
end 


(** Solving a pair equality.  For example [x = cons(car(x), y)] is 
  solved as [y |-> p!2; x |-> cons(p!1, p!2)] with [p!1], [p!2] fresh. *)
 
let rec solve e =
  fresh := [];                      (* reinitialize [is_fresh]. *)
  let e0, sl0, rho = pre e in
  let sl1 = solvel ([e0], sl0)in
  let sl2 = post rho sl1 in
    sl2

(** Preprocess the input equality [e] to replace each term of the
  form [car(x)] by [k1] with the solution [x = cons(k1, k2)] added
  to [sl], and similarly,[cdr(x)] can be replaced with [k2], for
  fresh [k2]. This completely eliminates [car] and [cdr] from the
  input equality.  Thus, [pre (a, b)] returns a triple consisting of 
  - an equality [a' = b'] such that [a'] and [b'] are built up from [cons]es only, 
    and none of the variables in [a'], [b'] is in the domain of the initial solution set, and
  - an initial solution set with equalities [x = cons(k1, k2)] for each entry *)
and pre (a, b) =
  let (a', rho) = Symtab.abstract (a, Symtab.empty) in (* Abstract [car(x)] and [cdr(x)]. *)
  let (b', rho') = Symtab.abstract (b, rho) in         (* [a'] and [b'] do not contain [car], [cdr]. *)
  let sl0 =                                            (* Add [x = cons(k1, k2) for each *)
    Symtab.fold                                        (* renaming pair. *)
      (fun (x, k1, k2) acc -> 
	 (x, mk_cons k1 k2) :: acc)
      rho' []
  in      
  let e0 = apply_to_eq sl0 (a', b') in                (* Apply these bindings. *)
    (e0, sl0, rho')
	
    

(** Repetitively apply the rules (symmetrically).
  - Triv:  [a = a, el; sl] ==> [el; sl]
  - Ext:   [cons(a', a'') = cons(b', b''), el; sl] ==> [a' = b', a'' = b'', el; sl]
  - Bot:   [x = a, el; sl] ==> [bot] 
              if [x] is a proper subterm of [a]
  - Subst: [x = a, el; sl] ==> [sigma(el[a/x]); s o {x = a}] 
              if [x] occurs in [el] but not in [a].
  
  Either a variable in [el] or a rhs in [sl] is eliminated (by Subst), or the 
  size of [el] decreases, so the corresponding lexicographic measure yields 
  termination. *)
and solvel (el, sl) =
  match el with
    | [] -> sl
    | (a, b) :: el1 ->
	solve1 (a, b) (el1,  sl) 

and solve1 (a, b) (el, sl) =
  if Term.eq a b then                      (* Triv *)
    solvel (el, sl) 
  else                      
    try          
      let (a', a'') = d_cons a             (* Ext *)
      and (b', b'') = d_cons b in
	solvel (((a', b') :: (a'', b'') :: el), sl)
    with
	Not_found ->       
	  assert(not(Term.eq a b)); 
          let (x, a) = if is_cons a then (b, a) else (a, b) in
	    assert(not(is_interp x));
	    if Term.subterm x a then       (* Bot *) 
	      raise Exc.Inconsistent
	    else                           (* Subst *)
	      let (x, a) = orient (x, a) in
	      let el' = apply_to_eqs [(x, a)] el in
	      let sl' = compose (x, a) sl in
		solvel (el', sl')

(** Ensure [y = x] with [y] fresh and [x] not fresh can not happen. *)
and orient (a, b) =
  if Term.is_var a && Term.is_var b then
    if is_fresh a && not(is_fresh b) then
      (b, a)
    else 
      Term.orient (a, b)
  else
    (a, b)

and apply_to_trm rho =
  let lookup x =
    try Term.Subst.lookup rho x with Not_found -> x
  in
    map lookup 
  
and apply_to_eq rho (a1, a2) =
  (apply_to_trm rho a1, apply_to_trm rho a2)
  
and apply_to_eqs rho a =
  List.map (apply_to_eq rho) a
       
and compose (x, a) sl =
  assert(not(is_interp x));
  Term.Subst.compose apply (x, a) sl

(** Postprocessing is needed to ensure that the solution does not
  contain new variables. We can then discard any equations of the form 
  [k = a] in [sl] for newly introduced [k]. The result does not necessarily
  use a minimal number of fresh variables. *)
and post rho sl =
  List.filter (fun (x, _) -> not (is_fresh x)) sl

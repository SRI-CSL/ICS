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

(** Inference system for canonizable, ground confluent theories. *)

module G = Fact.Input
module P = Partition

module Tr = Trace


module type T = sig
  val th : Th.t
  val map : (Term.t -> Term.t) -> Term.t -> Term.t
  val sigma : Sym.t -> Term.t list -> Term.t
end

module type DEDUCE = sig
  type t
  val of_equal : Fact.Equal.t -> P.t * t -> Fact.Equal.t list
  val of_var_equal : Fact.Equal.t -> P.t * t -> Fact.Equal.t list
  val of_var_diseq : Fact.Diseq.t -> P.t * t -> Fact.Equal.t list
  val disjunction : Partition.t * t -> Fact.t list
end

module Trace(D: DEDUCE): (DEDUCE with type t = D.t) = struct
  type t = D.t
  let of_equal d (p, s) =
    let el = D.of_equal d (p, s) in
      Tr.msg "deduce" "Of_equal" el (Pretty.list Fact.Equal.pp);
      el
 let of_var_equal d (p, s) =
    let el = D.of_var_equal d (p, s) in
      Tr.msg "deduce" "Of_var_equal" el (Pretty.list Fact.Equal.pp);
      el
 let of_var_diseq d (p, s) =
    let el = D.of_var_diseq d (p, s) in
      Tr.msg "deduce" "Of_var_diseq" el (Pretty.list Fact.Equal.pp);
      el

 let disjunction (p, s) = 
   let fl = D.disjunction (p, s) in
     Tr.msg "deduce" "Disjunction" fl (Pretty.list Fact.pp);
     fl
end 

(** Equalities of the form [x = a] with [a] flat. *)
module type EQS = sig
  module S : Solution.SET0
  type t = S.t
  val find : Partition.t * t -> Jst.Eqtrans.t
  val inv : Partition.t * t -> Jst.Eqtrans.t
  val can : Partition.t * t -> Jst.Eqtrans.t
end 

module E(Can: T) = struct

  (** Homomorphically apply equality transformer [f] 
    at  uninterpreted positions of [a]. *)
  let map c f a =
    let hyps = ref Jst.dep0 in
    let f' x =
      let (a, rho) = f x in
	if not(x == a) then 
	  hyps := Jst.dep2 rho !hyps; a
    in
    let b = Can.map f' a in
      (b, !hyps)

  (** [apply e a], for [e] of the form [x = y] substitutes [y] for [x] in [a]. *)
  let apply e a = 
    let (x, y, rho) = Fact.Equal.destruct e in
    let lookup z = if Term.eq z x then y else z in
    let b = Can.map lookup a in
    let tau = if a == b then Jst.dep0 else rho in
      (b, tau)

  (** Solution set for theory [Can.th]. *)
  module S: Solution.SET0 = Solution.Make0(
    struct
      let th = Can.th
      let map = Can.map
    end)

  type t = S.t

  let eq = S.eq
  let empty = S.empty
  let is_empty = S.is_empty
  let pp = S.pp
  let fold = S.fold
  let iter = S.iter
  let dep = S.dep
  module Dep = S.Dep
  let diff = S.diff

  (** A {i flat} term is of the form [f(x1,...,xn)] with [xi] variables. *)
  let is_flat a =
    try
      let (f, al) = Term.App.destruct a in
	List.for_all Term.is_var al
    with
	Not_found -> false


  (** A {i pure} term is built up with function symbols from theory [Can.th]. *)
  let rec is_pure a =
    try
      let (f, al) = Term.App.destruct a in
	Sym.theory_of f = Can.th && List.for_all is_pure al
    with
	Not_found -> true

  (** Destruct a pure [Can.th]-term into function symbol and operators. *)
  let d_interp a = 
    if is_pure a then
      Term.App.destruct a 
    else 
      raise Not_found

  let choose_apply (p, s) x =
    P.choose p (S.apply s) x 

  (** {i Find} of [a] in configuration [c] of the form [(p, s)]
    - [C0(x) = y] if [x =v y] and [y] canonical,
    - [C0(x) = x] otherwise
    - [C(x) = C0(x)]       
    - [C(x) = sigma(f(C0(z1), ..., C0(zn)))]  if [y = f(z1,...,zn)] in [s] with [x =V y],
    - [C(a) = a]   otherwise. *)
  let rec find ((p, s) as c) x = 
    if Term.is_app x then 
      Jst.Eqtrans.id x
    else 
      try
	let (a, rho) = choose_apply c x in   (* [rho |- x = f(a1,...,an). *)
	let (f, al) = d_interp a in
	let (al', tau) = findl c al in       (* [tau |- a1 = a1', ..., an = an'] *)
	  (Can.sigma f al', Jst.dep2 rho tau)
      with
	  Not_found -> P.find p x

  and findl ((p, s) as c) al =
   let hyps = ref Jst.dep0 in
   let find' a =
     try
       let (b, rho) = choose_apply c a in (* ??? *)
	 hyps := Jst.dep2 rho !hyps;
	 b
     with
	 Not_found -> a
   in
   let bl = Term.mapl find' al in
     (bl, !hyps)

  let find c = Jst.Eqtrans.trace "fnd" "Can.find" (find c)
	      
  (** Recursively replace variables [x] in [a] with [b], 
    with [b] such that [y = b] in [s] for [x] and [y]
    equal modulo [p]. *)
  let rec replace c =  
    map c (find c)

  (** {i Inverse lookup} of [a] in configuration [(v, s)]
    - [C^-1(x) = y]   if [x =v y] and [y] canonical
    - [C^-1(a) = y]   if [x = a] in [s] and [y =v x] with [y] canonical
    - [C^-1(f(a1, ...,an)) = C^-1(f(C^-1(a1), ..., C^-1(an)))] if there is [ai <> C^-1(ai)]
    - [C^-1(a)] = a *)
  let rec inv ((p, s) as c) a =
    if Term.is_var a then P.find p a else 
      try
	Jst.Eqtrans.compose (P.find p) (S.inv s) a
      with
	  Not_found ->
	    let (f, al) = d_interp a in    (* [a = f(a1,...,an)] *)
	    let (bl, rho) = invl c al in   (* [rho |- a1 = b1, ..., an = bn]. *)
	      if al == bl then
		Jst.Eqtrans.id a
	      else                         (* [tau |- f(b1, ..., bn) = a'] *)
		let (a', tau) = inv c (Can.sigma f bl) in
		  (a', Jst.dep2 rho tau)
	    
  and invl ((p, s) as c) al =
    let hyps = ref Jst.dep0 in
    let bl = 
      Term.mapl 
	(fun a -> 
	   try
	     let (b, rho) = inv c a in
	       hyps := Jst.dep2 rho !hyps;
	       b
	   with
	       Not_found -> a)
	al
    in
      (bl, !hyps)


  (** {i Canonization} with respect to a configuration [c]
    of the form [(v, s)] with [v] variable equalities and [s]
    an array context.
    - [C[| . |] = (C^-1(.) o C[.])] *)
  let  can c =
    Jst.Eqtrans.compose 
      (Jst.Eqtrans.totalize (inv c))
      (replace c)

end 



(** Inference system from a pseudo-solvable theory. *)
module Make(Can: T)
           (Deduce: (DEDUCE with type t = E(Can).t))
: 
(Infsys.IS with type e = E(Can).t) = 

struct

  type e = E(Can).t

  let is_pure = Term.is_pure Can.th

  let ths = Th.Set.singleton Can.th

  (** Solution set for theory [Can.th]. *)
  module E = E(Can)

  let rec abstract i a (g, s, p) =
    (if not(i = Can.th) then raise Not_found);
    assert(is_pure a);
    let ((x', rho'), (p', s')) = flatten (p, s) a in
      assert(Term.is_var x');
      let e' = Fact.Equal.make (x', a, rho') in
      let g' = G.instantiate e' g in
	(g', s', p')


  (** Flatten a pure array term [a] and introduce variables as
    necessary for naming subterms. The result is a variable [z]
    equal to [a] in the extended context. *)
  and flatten ((p, _) as c) a =
    assert(is_pure a);
    let (a, rho) = Jst.Eqtrans.totalize (E.inv c) a in
      if Term.is_var a then 
	((a, rho), c) 
      else 
	try
	  let (f, al) = E.d_interp a in
	  let ((bl, tau), c) = flatten_args c al in
	  let a' = Can.sigma f bl in 
	  let u' = mk_rename () in
	  let e' = Fact.Equal.make (u', a', rho) in
	  let c' = update c e' in
	    ((u', Jst.dep2 rho tau), c')
	with
	    Not_found -> 
	      assert(Term.is_var a);
	      let (a, tau) = P.find p a in
	      ((a, Jst.dep2 rho tau), c)
		  
  and flatten_args ((p, s) as c) al =
    let hyps = ref Jst.dep0 in
    let c = ref c in
    let xl = 
      Term.mapl
	(fun a -> 
	   let ((x, rho'), c') = flatten !c a in
	     assert(Term.is_var x);
	     hyps := Jst.dep2 rho' !hyps;
	     c := c';
	     x)
	al
    in
      ((xl, !hyps), !c)
  
    
  and mk_rename () =
    let v = Name.of_string (Th.to_string Can.th) in
      Term.Var.mk_rename v None Var.Cnstrnt.Unconstrained
  

  and merge i e ((g, s, p) as c) =
    assert(i = Can.th);
    assert(Fact.Equal.is_pure i e);
    let e = Fact.Equal.map (E.can (p, s)) e in
    let (a, b, _) = Fact.Equal.destruct e in
      if Term.eq a b then c else
	let (p, s) = merge_i e (p, s) in
	  (g, s, p)


  and merge_i e (p, s) =  
    Tr.msg "foo9" "Can.merge_i" e Fact.Equal.pp;
    assert(Fact.Equal.is_pure Can.th e);
      if Fact.Equal.is_var e then 
	merge_v e (Partition.merge p e, s)
      else 
	let (a, b, rho) = Fact.Equal.destruct e in
	let ((x, tau), (p, s)) = flatten (p, s) a in
	let ((y, sigma), (p, s)) = flatten (p, s) b in
	let e' = Fact.Equal.make (x, y, Jst.dep3 rho tau sigma) in
	  assert(Fact.Equal.is_var e');
	  merge_v e' (Partition.merge p e', s)
      

  and merge_v e (p, s) =
    Tr.msg "foo9" "Can.merge_v" e Fact.Equal.pp;
    assert(Fact.Equal.is_var e);
    let (p, s) = fuse1 (p, s) e in
    let el = Deduce.of_var_equal e (p, s) in
      close (p, s) el
    
  
  and fuse1 ((p, s) as c) e =
    assert(Fact.Equal.is_var e);
    let (x, y, rho) = Fact.Equal.destruct e in
    let norm a = E.apply e a in 
     E.S.Dep.fold s
       (fun e (p, s) ->
	  let e' = Fact.Equal.map_rhs norm e in
	    update (p, s) e')
       x (p, s)


  and update (p, s) e = 
    let (p, s) = E.S.update (p, s) e in
      close (p, s) (Deduce.of_equal e (p, s))


  and close (p, s) = function
    | [] -> (p, s) 
    | e :: el -> close (close1 e (p, s)) el

  and close1 e (p, s) =
    Tr.msg "foo" "Can.Close" e Fact.Equal.pp;
    let e = Fact.Equal.map (E.inv (p, s)) e in
    let (a, b, _) = Fact.Equal.destruct e in
      if Term.eq a b then (p, s) else
	if Fact.Equal.is_var e then
	  let p = P.merge p e in
	    (* merge_v e (p, s) *)
	    fuse1 (p, s) e
	else
	  begin
	    assert(Fact.Equal.is_pure Can.th e);
	    let (a, b, rho) = Fact.Equal.destruct e in
	    let ((x, tau), (p, s)) = flatten (p, s) a in
	    let ((y, sigma), (p, s)) = flatten (p, s) b in
	    let e' = Fact.Equal.make (x, y, Jst.dep3 rho tau sigma) in
	      assert(Fact.Equal.is_var e');
	      let p = P.merge p e' in
		fuse1 (p, s) e'
	  end 


  let propagate e ((g, s, p) as c) =
    assert(Fact.Equal.is_var e);
    Tr.msg "foo9" "Can.propagate" e Fact.Equal.pp;
    if E.is_empty s then c else  
      let (x, y, rho) = Fact.Equal.destruct e in 
      let (a, tau) = E.find (p, s) x
      and (b, sigma) = E.find (p, s) y in
	if Term.eq a b then c else 
	  let e = Fact.Equal.make (a, b, Jst.dep3 rho tau sigma) in
	  let (p, s) = merge_i e (p, s) in
	    (g, s, p)


  (** Close state on fresh variable disequalities *)
  let dismerge i d (g, s, p) =
    assert(Fact.Diseq.is_pure i d);
    let (a, b, rho) = Fact.Diseq.destruct d in
    let ((x, tau), (p, s)) = flatten (p, s) a in
    let ((y, sigma), (p, s)) = flatten (p, s) b in
    let d = Fact.Diseq.make (x, y, Jst.dep3 rho tau sigma) in
      assert(Fact.Diseq.is_var d);
      let p = Partition.dismerge p d in
	(g, s, p)



  (** Test if disequality [d] is unsatisfiable. *)
  let is_unsat (p, s) d = 
    assert(Fact.Diseq.both_sides (Term.is_pure Th.la) d);
    if E.is_empty s then None else  
      let d = Fact.Diseq.map (E.can (p, s)) d in
      let (a, b, rho) = Fact.Diseq.destruct d in
	if Term.eq a b then Some(rho) else None

  let propagate_diseq d (g, s, p) =
    assert(Fact.Diseq.is_var d);
    match is_unsat (p, s) d with
      | Some(rho) ->
	  raise(Jst.Inconsistent(rho))
      | None ->
	  let el = Deduce.of_var_diseq d (p, s) in
	  let (p, s) = close (p, s) el in
	    (g, s, p)


  let branch ((g, s, p) as c) =
    try
      let fl = Deduce.disjunction (p, s) in
	List.map (fun fct -> (G.add g fct, s, p)) fl
    with
	Not_found -> [c]

  let normalize c = c

end 



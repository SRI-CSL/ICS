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

module P = Partition
module S = Solution.Set


type config = P.t * S.t


module type T = sig 
  val th : Th.t
  val map : (Term.t -> Term.t) -> Term.t -> Term.t
  val sigma : Sym.t -> Term.t list -> Term.t
  val of_equal : Fact.Equal.t -> P.t * S.t -> Fact.Equal.t list
  val of_var_equal : Fact.Equal.t -> P.t * S.t -> Fact.Equal.t list
  val of_var_diseq : Fact.Diseq.t -> P.t * S.t -> Fact.Equal.t list
  val disjunction : Partition.t * S.t -> Clause.t
end

module type OPS = sig
  val is_flat : Term.t -> bool
  val is_pure : Term.t -> bool
  val find : P.t * S.t -> Jst.Eqtrans.t
  val inv : P.t * S.t -> Jst.Eqtrans.t
end 
    

(** Operations on canonizable theories. *)
module Ops(Can: T) = struct


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
	

  (** Homomorphically apply equality transformer [f] 
    at uninterpreted positions of [a]. *)
  let map c f a =
    let hyps = ref Jst.dep0 in
    let f' x =
      let (a, rho) = f x in
	if not(x == a) then 
	  hyps := Jst.dep2 rho !hyps; a
    in
    let b = Can.map f' a in
      (b, !hyps)


  (** {i Lookup} of [a] in configuration [C] of the form [(p, s)]
    - [lookup(C)(x) = y] if [x =p y] and [y] canonical,
    - [lookup(C)(x)] is undefined otherwise. *)
  let lookup (p, s) =
    P.choose p (S.apply s)
      

  (** {i Find} of [a] in configuration [c] of the form [(p, s)]
    - [C(a) = a] with [a] a nonvariable term,
    - [C(x) = sigma(f(lookup(C)(z1), ..., lookup(C)(zn)))] if [lookup(C)(x) = f(z1, ..., zn)], and
    - [C(x) = y] with [x =p y] and [y] canonical otherwise. *)
  let rec find ((p, s) as c) x = 
    if Term.is_app x then 
      Jst.Eqtrans.id x
    else 
      try
	let (a, rho) = lookup c x in         (* [rho |- x = f(a1,...,an). *)
	let (f, al) = d_interp a in
	let (al', tau) = findl c al in       (* [tau |- a1 = a1', ..., an = an'] *)
	  (Can.sigma f al', Jst.dep2 rho tau)
      with
	  Not_found -> P.find p x
	    
  and findl c al =
    let hyps = ref Jst.dep0 in
    let find' a =
      try
	let (b, rho) = lookup c a in     (* not recursive? *)
	  hyps := Jst.dep2 rho !hyps;
	  b
      with
	  Not_found -> a
  in
    let bl = Term.mapl find' al in
      (bl, !hyps)


  (** {i Inverse lookup} of [a] in configuration [(v, s)]
    - [C^-1(x) = y]   if [x =p y] and [y] canonical
    - [C^-1(a) = y]   if [x = a] in [s] and [y =p x] with [y] canonical
    - [C^-1(f(a1, ...,an)) = C^-1(f(C^-1(a1), ..., C^-1(an)))] if there is [ai =/= C^-1(ai)]
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


  (** Recursively replace variables [x] in [a] with [b], 
    with [b] such that [y = b] in [s] for [x] and [y]
    equal modulo [p]. *)
  let rec replace c =  
    map c (find c)


  (** {i Canonization} with respect to a configuration [c]
    of the form [(v, s)] with [v] variable equalities and [s]
    an array context.
    - [C[| . |] = (C^-1(.) o C[.])] *)
  let  can c =
    Jst.Eqtrans.compose 
      (Jst.Eqtrans.totalize (inv c))
      (replace c)

  (** [apply e a], for [e] of the form [x = y] substitutes [y] for [x] in [a]. *)
  let apply e a = 
    let (x, y, rho) = e in
    let lookup z = if Term.eq z x then y else z in
    let b = Can.map lookup a in
    let tau = if a == b then Jst.dep0 else rho in
      (b, tau)

end




(** Inference system from a pseudo-solvable theory. *)
module Make(Can: T): (Infsys.EQ with type e = S.t) = 

struct

  type e = S.t

  let is_pure = Term.is_pure Can.th

  (** Solution set for theory [Can.th]. *)
  module Ops = Ops(Can)
  open Ops

  let s = ref S.empty

  let current () = !s

  let protected = ref false

  let initialize s0 =
    protected := false;
    s := s0 

  let finalize () = !s

  open Infsys

  let inv a = Ops.inv (!p, !s) a
  let find a = Ops.find (!p, !s) a

		 
  (** Flatten a pure term [a] and introduce variables as
    necessary for naming subterms. The result is a variable [z]
    equal to [a] in the extended context. *)
  let rec flatten a =
    assert(is_pure a);
    let (a, rho) = Jst.Eqtrans.totalize inv a in
      if Term.is_var a then 
	(a, rho)
      else 
	try
	  let (f, al) = Ops.d_interp a in
	  let (bl, tau) = flatten_args al in
	  let a' = Can.sigma f bl in 
	  let u' = mk_rename () in
	  let e' = Fact.Equal.make u' a' rho in
	    update e';
	    (u', Jst.dep2 rho tau)
	with
	    Not_found -> 
	      assert(Term.is_var a);
	      let (a, tau) = P.find !p a in
		(a, Jst.dep2 rho tau)
	
	
  and flatten_args al =
    let hyps = ref Jst.dep0 in
    let xl = 
      Term.mapl
	(fun a -> 
	   let (x, rho') = flatten a in
	     assert(Term.is_var x);
	     hyps := Jst.dep2 rho' !hyps;
	     x)
	al
    in
      (xl, !hyps)
    

  and mk_rename () =
     let v = Name.of_string (Th.to_string Can.th) in
       Term.Var.mk_rename v None Var.Cnstrnt.Unconstrained
  

  and abstract a =
    assert(Term.is_pure Can.th a);
    let (x', rho') = flatten a in
      assert(Term.is_var x');
      let eq = Fact.Equal.make x' a rho' in
	G.replace eq !g


  and merge e =
    assert(Fact.Equal.is_pure Can.th e);
    let e = Fact.Equal.map (Ops.can (!p, !s)) e in
    let (a, b, _) = e in
      if not(Term.eq a b) then
	merge_i e


  and merge_i e =
    assert(Fact.Equal.is_pure Can.th e);
    if Fact.Equal.is_var e then 
      begin
	P.merge !p e;
	merge_v e
      end
    else 
      let e' = Fact.Equal.map flatten e in
	assert(Fact.Equal.is_var e');
	P.merge !p e';
	merge_v e'
     
 
  and merge_v e =
    assert(Fact.Equal.is_var e);
    fuse1 e;
    let el = Can.of_var_equal e (!p, !s) in
      close el


  and fuse1 e =
    assert(Fact.Equal.is_var e);
    let (x, y, rho) = e in
    let norm a = Ops.apply e a in 
      S.Dep.iter !s
	(fun e ->
	   let e' = Fact.Equal.map_rhs norm e in
	     update e')
	x


  and update e = 
    if not(!protected) then
      begin
	s := S.copy !s;
	protected := true
      end;
    S.update (!p, !s) e;
    close (Can.of_equal e (!p, !s))


  and close el =
    List.iter close1 el


  and close1 e =
    let e = Fact.Equal.map inv e in
    let (a, b, _) = e in
      if not(Term.eq a b) then
	if Fact.Equal.is_var e then
	  begin
	    P.merge !p e;    (* merge_v e (p, s) *)
	    fuse1 e
	  end
	else
	  begin
	    assert(Fact.Equal.is_pure Can.th e);
	    let e' = Fact.Equal.map flatten e in
	      assert(Fact.Equal.is_var e');
	      P.merge !p e';
	      fuse1 e'
	  end 


  let propagate e =
    assert(Fact.Equal.is_var e);
    if not(S.is_empty !s) then
      let e = Fact.Equal.map find e in
      let (a, b, _) = e in
	if not(Term.eq a b) then
	  merge_i e
	      

  (** Close state on fresh variable disequalities *)
  let dismerge d =
    assert(Fact.Diseq.is_pure Can.th d);
    let d = Fact.Diseq.map flatten d in
      assert(Fact.Diseq.is_var d);
      Partition.dismerge !p d


  (** Test if disequality [d] is unsatisfiable. *)
  let is_unsat d = 
    assert(Fact.Diseq.is_pure Can.th d);
    if S.is_empty !s then None else  
      let d = Fact.Diseq.map (Ops.can (!p, !s)) d in
      let (a, b, rho) = d in
	if Term.eq a b then Some(rho) else None


  let propagate_diseq d =
    assert(Fact.Diseq.is_var d);
    match is_unsat d with
      | Some(rho) ->
	  raise(Jst.Inconsistent(rho))
      | None ->
	  let el = Can.of_var_diseq d (!p, !s) in
	    close el


  let branch () =
    try
      let cl = Can.disjunction (!p, !s) in
	G.put_clause cl !g
    with
	Not_found -> ()
	  

  let normalize c = ()
		      
end 



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


module type T = sig
  val th : Th.t
  val map : (Term.t -> Term.t) -> Term.t -> Term.t
  val solve : Fact.Equal.t -> Fact.Equal.t list
  val disjunction : Fact.Equal.t -> Clause.t
end


module Make(Sh: T): (Infsys.EQ with type e = S.t) = struct

  (** Invariant: all right hand sides of equalities [x = a] in the 
    solution set are [Sh.th]-pure. *)
  let is_pure = Term.is_pure Sh.th

  open Infsys.Config

  type e = S.t


  (** Global equalitity configuration for this theory. *)
  let s = ref S.empty

  let current () = !s

  let protected = ref false

  let initialize s0 =
    protected := false; 
    s := s0  

  let finalize () = !s

  let find a = S.find !s a
  let inv a = S.inv !s a

  open Infsys

  (** Basic update of solutions sets. Only copies when needed. *)
  let update e =
    if !protected then
      S.update (!p, !s) e
    else 
      begin
	s := S.copy !s;
	protected := true;
	S.update (!p, !s) e
      end
    

  (** Replace dependent variables in [s] with corresponding
    right hand sides (assumed to be canonical). *)
  let can a =
    let hyps = ref Jst.dep0 in
    let lookup y = 
      try
	let (b, rho) = S.apply !s y in
	  hyps := Jst.dep2 rho !hyps; b
      with
	  Not_found -> 
	    let (x, rho) = P.find !p y in
	      hyps := Jst.dep2 rho !hyps; x
    in
    let b = Sh.map lookup a in
      (b, !hyps)

		
  (** Create fresh renaming variable. *)
  let mk_rename () = 
    let v = Name.of_string "v" in
    let cnstrnt = Var.Cnstrnt.Unconstrained in
      Term.Var.mk_rename v None cnstrnt

	
  (** Return a variable [x] for a term [a] with [x = a] in the
    (possibly extended) current solution set. *)
  let name a =
    assert(is_pure a);
    if Term.is_var a then
      Jst.Eqtrans.id a
    else 
      let (a, rho) = can a in         (* replace dependent variables. *)
	try
	  let (x, tau) = Jst.Eqtrans.compose (P.find !p) (S.inv !s) a in
	    (x, Jst.dep2 rho tau)
	with
	    Not_found ->
	      let x = mk_rename() in
	      let e = Fact.Equal.make x a rho in
		update e;
		(x, rho)

	
	
  (** {i Composing} a list of solved equalities [sl] with the current solution set. *)
  let rec compose sl =
    fuse sl;
    let upd e = 
      let x = Fact.Equal.lhs_of e in           (* drop [x = a] with [x] an *)
	if not(Term.Var.is_fresh Sh.th x) then (* internal variable. *)
	  update e
    in
      List.iter upd sl
	
  and  fuse sl =
    let fs1 e = fuse1 e in
      List.iter fs1 sl
	
  and fuse1 (x, b, rho) = 
    let apply_to_term a = 
      let lookup y = if Term.eq x y then b else y in
      let a' = Sh.map lookup a in
	if a == a' then Jst.Eqtrans.id a else (a', rho)
    in
    let apply_to_equality e = 
      update (Fact.Equal.map_rhs apply_to_term e)
    in
      S.Dep.iter !s apply_to_equality x
	

  let abstract a =
    assert(is_pure a);
    let (y, rho) = name a in
      assert(Term.is_var y);
      G.replace (Fact.Equal.make y a rho) !g
	
    
  let rec merge e =
    assert(Fact.Equal.is_pure Sh.th e);
    process (Fact.Equal.map can e)


  and process e =
    assert(Fact.Equal.is_pure Sh.th e);
    try
      let sl = Sh.solve e in
	compose sl
    with
	Exc.Incomplete ->
	  let (a, b, rho) = e in
	  let (x, tau) = name a in
	  let (y, sigma) = name b in
	  let e' = Fact.Equal.make x y (Jst.dep3 rho tau sigma) in
	    P.merge !p e'


  (** Propagate a derived variable equality [x = y] with [y] canonical. *)
  let rec propagate e =
    assert(Fact.Equal.is_var e);
    if not(S.is_empty !s) then
      let x, y, rho = e in 
	if occurs x && occurs y then
	  let (a, tau) = find x
	  and (b, sigma) = find y in
	    if not(Term.eq a b) then
	      let e' = Fact.Equal.make a b (Jst.dep3 rho tau sigma) in
		process e'

  and find x =
    S.find !s x

  and occurs x =
    S.is_dependent !s x || 
    S.is_independent !s x


  let dismerge d = 
    assert(Fact.Diseq.is_pure Sh.th d);
    let d' = Fact.Diseq.map name d in
      assert(Fact.Diseq.is_var d');
      Partition.dismerge !p d'
	

  (** Test if disequality [d] is unsatisfiable. *)
  let diseq_is_falsified d = 
    assert(Fact.Diseq.both_sides (Term.is_pure Th.la) d);
    if S.is_empty !s then None else  
      let d = Fact.Diseq.map can d in
      let (a, b, rho) = d in
	if Term.eq a b then Some(rho) else None


  (** No equalities from disequalities in convex theories. *)
  let propagate_diseq d =
    assert(Fact.Diseq.is_pure Sh.th d);
    if not(S.is_empty !s) then
      match diseq_is_falsified d with 
	| None -> ()
	| Some(rho) -> raise(Jst.Inconsistent(rho))


  exception FoundClause of Clause.t	

  (** No branching for convex theories. *)
  let rec branch () = 
    try
      let cl = disjunction () in
	G.put_clause cl !g
    with
	Not_found -> ()

  and disjunction () =
    try
      S.iter 
	(fun e ->
	   try
	     let cl = Sh.disjunction e in
	       raise(FoundClause(cl))
	   with
	       Not_found -> ())
	!s;
      raise Not_found;
    with
	FoundClause(cl) -> cl


  (** Ensures all variables in solution set are canonical terms. *)
  let normalize _ = ()

end 

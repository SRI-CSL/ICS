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

(** Inference system for array theory. *)

(** Decision procedure for the theory of arrays [Arr] as defined in 
  module {!Funarr}. This procedure works by canonization of array terms
  and forward chaining on configurations.

  A configuration [C] consists of a triple [(V, D, S)] with 
  - [V] a set of variable equalities,
  - [D] a set of variable disequalities,
  - [S] a set of array equalities [x = a] with [x] a variable 
  and [a] a pure array term.

  The pair [(V, D)] is passed around as a partitioning [P].

  Forward chaining is used to keep configurations {i confluent}.
  - (1) [u = a[i:=x]] ==> [x = u[i]]
  - (2) [i<>j] and [v = u[j]], [u = a[i:=x]] ==> [v = a[j]]
  - (3) [i<>j] and [u = a[i:=x]] and [v = u[j:=y]] ==> [v = a[j:=y][i:=x]], 
  - (4) [u = v[i:=x]], [v = a[i:=y]] ==> [u = a[i:=x]]
  - (5) [v = u[j]], [u = create(a)] ==> [v = a]
  i.e., flatten the rhs and add to the equality graph.

  These rules are {i eagerly} applied whenever the antecedent
  becomes applicable, that is, when
  - a new disequality is propagated
  - a new equality [u = a] is added

  Since not all disequalities are propagated, the rules [(2)], [(3)]
  are also applied {i lazily}. For rule [(2)], for example, whenever
  [a[j]] is canonized, then it is checked if rule [(2)] actually fires
  with an {i implicit} disequality.

  Case splitting needs to be performed on all pairs [(i, j)] such
  that [u = a'[j]], [a' = a[i:=x]] in order to make the procedure 
  complete.
*)


module T: Can.T =
  struct
      
    let th = Th.arr

    let map = Funarr.map Term.is_equal

    let sigma f al =
      try
	let op = Sym.Array.get f in
	  Funarr.sigma Term.is_equal op al
      with
	  Not_found -> Term.App.mk_app f al

  end

module E = Can.E(T)



(** Forward chaining is used to keep configurations {i confluent}.
  - (1) [u = a[i:=x]] ==> [x = u[i]]
  - (2) [i<>j] and [v = u[j]], [u = a[i:=x]] ==> [v = a[j]]
  - (3) [i<>j] and [u = a[i:=x]] and [v = u[j:=y]] ==> [v = a[j:=y][i:=x]], 
  - (4) [u = v[i:=x]], [v = a[i:=y]] ==> [u = a[i:=x]]
  - (5) [v = u[j]], [u = create(a)] ==> [v = a] *)
module D = struct

  type t = E.t
      
  (** Destructors for equalities. *)
  let d_update p e =
    let (x, a, rho) = Fact.Equal.destruct e in
    let (y, tau) = Partition.find p x in
      (y, Funarr.d_update a, Jst.dep2 rho tau)
      
  let d_select p e =
    let (x, a, rho) = Fact.Equal.destruct e in
    let (y, tau) = Partition.find p x in
      (y, Funarr.d_select a, Jst.dep2 rho tau)
      
  let d_create p e =
    let (x, a, rho) = Fact.Equal.destruct e in
    let (y, tau) = Partition.find p x in
      (y, Funarr.d_create a, Jst.dep2 rho tau)
      
  let rec of_equal e (p, s) =
    try
      of_update (d_update p e) (p, s)
    with
	Not_found ->
	  (try
	     of_select (d_select p e) (p, s)
	   with
	       Not_found ->
		 (try
		    of_create (d_create p e) (p, s)
		  with
		      Not_found -> []))

  (** Propagate an update equality using rules (1), (2), (3), (4). *)
  and of_update u (p, s) =
    of_update1 u (p, s)
      (of_update2 u (p, s) 
	 (of_update3 u (p, s)
	    (of_update4 u (p, s) [])))

  (** (1) [u = a[i:=x]] ==> [x = u[i]] *)
  and of_update1 (u, (a, i, x), rho) (p, s) acc =
    let ui = Funarr.Flat.mk_select u i in
    let e = Fact.Equal.make (x, ui, rho) in
      e :: acc
    
  (** (2) [i<>j], [v = u[j]], [u' = a[i:=x]], [u = u'] ==> [v = a[j]] *)
  and of_update2 (u', (a, i, x), rho) (p, s) acc =               (* [rho |- u' = a[i:=x]] *)
    let (u0, rho0) = Partition.find p u' in                      (* [rho0 |- u = u0] *)
      E.S.Dep.fold s
	(fun e acc ->
	   try
	     let (v, (u, j), tau) = d_select p e in      
	       if not(Term.eq u u0) then acc else                (* [tau |- v' = u[j]] *)
		 (match Partition.is_diseq p i j with
		    | None -> acc
		    | Some(sigma) ->                             (* [sigma |- i <> j] *)
			let theta = Jst.dep4 rho0 rho tau sigma in
			let aj = Funarr.Flat.mk_select a j in
			let e = Fact.Equal.make (v, aj, theta) in
			  e :: acc)
	   with
	       Not_found -> acc)
	u0 acc
    
  (** (3) [i<>j] and [u = a[i:=x]] and [v = u[j:=y]] ==> [v = a[j:=y][i:=x]] *)
  and of_update3 (u, (a, i, x), rho) (p, s) acc =
    E.S.fold
      (fun e acc ->
	 try
	   let (v, (u', j, y), tau) = d_update p e in
	     if not(u == v) && Term.eq u u' then
	       match Partition.is_diseq p i j with
		 | None -> acc
		 | Some(sigma) ->  
		     let theta = Jst.dep3 rho tau sigma in
		     let b =
		       Funarr.Flat.mk_update
			 (Funarr.Flat.mk_update a j y) i x
		     in
		     let e = Fact.Equal.make (v, b, theta) in
		       e :: acc
	     else 
	       acc
	 with
	     Not_found -> acc)
      s acc
	     
  (** (4) [u = v[i:=x]], [v = a[i:=y]]  ==> [u = a[i:=x]] *)
  and of_update4 upd (p, s) acc =
    of_update41 upd (p, s)
      (of_update42 upd (p, s) acc)


  and of_update41 (u, (v, i, x), rho) (p, s) acc = 
    E.S.Dep.fold s
      (fun e acc ->
	 try
	   let (v', (a, i', y), tau) = d_update p e in
	     if Term.eq v v' && Term.eq i i' then
	       let sigma = Jst.dep2 rho tau in
	       let aix =  Funarr.Flat.mk_update a i x in
	       let e = Fact.Equal.make (u, aix, sigma) in
		 e :: acc
	     else 
	       acc
	 with
	     Not_found -> acc)
      i acc

  and of_update42 (v, (a, i, y), rho) (p, s) acc = 
    E.S.Dep.fold s
      (fun e acc ->
	 try
	   let (u, (v', i', x), tau) = d_update p e in
	     if Term.eq v v' && Term.eq i i' then
	       let sigma = Jst.dep2 rho tau in
	       let e = Fact.Equal.make (u, Funarr.Flat.mk_update a i x, sigma) in
		 e :: acc
	     else 
	       acc
	 with
	     Not_found -> acc)
      i acc

  (** Forward chain on selections, that is rules (2) and (5). *)
  and of_select sel (p, s) =
    of_select2 sel (p, s)
       (of_select5 sel (p, s) [])

  (** (2) [i<>j] and [v = u[j]], [u' = a[i:=x]], [u = u'], ==> [v = a[j]]. *)
  and of_select2 (v, (u, j), rho) (p, s) =                 (* [rho |- v = u[j]] *)
    Partition.fold p
      (fun e acc ->
	 try
	   let (u', (a, i, x), tau) = d_update p e in      (* [tau |- u' = a[i:=x]] *)
	     if not(Term.eq u u') then acc else            (* [u == u'] *)
	       (match Partition.is_diseq p i j with 
		 | None -> acc
		 | Some(sigma) ->                          (* [sigma |- i <> j] *)  
		     let theta = Jst.dep3 rho tau sigma in
		     let aj = Funarr.Flat.mk_select a j in
		     let e = Fact.Equal.make (v, aj, theta) in
		       e :: acc)
	 with
	     Not_found -> acc)
      u 

  (** (5) [v = u[j]], [u = create(a)] ==> [v = a]. *)
  and of_select5 (v, (u, j), rho) (p, s) = 
    Partition.fold p
      (fun e acc -> 
	 try
	   let (u', a, tau) = d_create p e in
	     if Term.eq u u' then
	       let e' = Fact.Equal.make (v, a, Jst.dep2 rho tau) in
		 e' :: acc
	     else 
	       acc
	 with
	     Not_found -> acc)
      u

  (** Forward chain on create.
    (5) [v = u[j]], [u = create(a)] ==> [v = a]. *)
  and of_create (u, a, rho) (p, s) =
    E.S.Dep.fold s
      (fun e acc ->
	 try
	   let (v, (u', j), tau) = d_select p e in
	     if Term.eq u u' then
	       let theta = Jst.dep2 rho tau in
	       let e' = Fact.Equal.make (v, a, theta) in
		 e' :: acc
	     else 
	       acc
	       
	 with
	     Not_found -> acc)
      u []

  (** Forward chain on variable equalities. *)
  let rec of_var_equal e (p, s) = 
    assert(Fact.Equal.is_var e);
    of_var_equal2 e (p, s) 
      (of_var_equal3 e (p, s)
	 (of_var_equal4 e (p, s)
	    (of_var_equal5 e (p, s) [])))

  (** (2) [i<>j] and [v = u[j]], [u' = a[i:=x]], [u0 =v = u' =v u] ==> [v = a[j]] *)
  and of_var_equal2 e (p, s) =
    let u'' = Fact.Equal.lhs_of e in
    let (u0, rho0) = Partition.find p u'' in
      E.S.Dep.fold s
	(fun e acc ->
	   try
	     let (v, (u, j), rho) = d_select p e in
	       if not(Term.eq u0 u) then acc else
		 D.Set.fold 
		   (fun (i, tau) acc ->   
		      E.S.Dep.fold s
		        (fun e acc ->
			   try
			     let (u', (a, i', x), sigma) = d_update p e in
			       if not(Term.eq i i') then acc else
				 match Partition.is_equal p u0 u' with
				   | None -> acc
				   | Some(theta) -> 
				       let ups = Jst.dep5 rho0 rho tau sigma theta in
				       let e = Fact.Equal.make (v, Funarr.Flat.mk_select a j, ups) in
					 e :: acc	 
			   with
			       Not_found -> acc)
		        i acc)
		   (Partition.diseqs p j)
		   acc
	   with
	       Not_found -> acc)
	u0

  (** (3) [i<>j], [u' = a[i:=x]], [v = u[j:=y]], [u =v u'] ==> [v = a[j:=y][i:=x]] *)
  and of_var_equal3 e (p, s) =
    let (u0, rho) = Partition.find p (Fact.Equal.rhs_of e) in
      E.S.Dep.fold s
	(fun e1 acc1 ->
	   try
	     let (v, (u', j, y), tau) = d_update p e1 in
	       if not(Term.eq u' u0) then acc1 else
		 Partition.fold p
		   (fun e2 acc2 ->
		      try
			let (u', (a, i, x), sigma) = d_update p e2 in
			  match Partition.is_equal p u' u0, Partition.is_diseq p i j with
			    | Some(theta), Some(ups) -> 
				let eps = Jst.dep5 rho tau sigma theta ups in
				let ajy = Funarr.Flat.mk_update a j y in
				let ajyix = Funarr.Flat.mk_update ajy i x in
				let e = Fact.Equal.make (v, ajyix, eps) in
				  e :: acc2
			    | _ ->
				acc2
		      with
			  Not_found -> acc2)
		   u0 acc1
	   with
	       Not_found -> acc1)
	u0 
    

  (** (4) [u = v[i:=x]], [v' = a[i:=y]], [v = v'] ==> [u = a[i:=x]] *)
  and of_var_equal4 e (p, s) =
    let (v0, rho) = Partition.find p (Fact.Equal.rhs_of e) in
      E.S.Dep.fold s
	(fun e1 acc1 ->
	   try
	     let (u, (v', i, x), tau) = d_update p e1 in
	       if not(Term.eq v0 v') then acc1 else
		 Partition.fold p
		   (fun e2 acc2 ->
		      try
			let (v', (a, i', y), sigma) = d_update p e2 in
			  if not(Term.eq i i') then acc2 else
			    match Partition.is_equal p v' v0 with
			      | None -> acc2
			      | Some(theta) -> 
				  let aix = Funarr.Flat.mk_update a i x in
				  let ups = Jst.dep4 rho tau sigma theta in
				  let e = Fact.Equal.make (u, aix, ups) in
				    e :: acc2
		      with
			  Not_found -> acc2)
		   v0 acc1
	   with
	       Not_found -> acc1)
	v0 
      

  (** (5) [v = u[j]], [u' = create(a)], [u =v u'] ==> [v = a] *)
  and of_var_equal5 e (p, s) =
    let (u0, rho) = Partition.find p (Fact.Equal.rhs_of e) in
      E.S.Dep.fold s
	(fun e1 acc1 ->
	   try
	     let (v, (u', j), tau) = d_select p e1 in
	       if not(Term.eq u0 u') then acc1 else
		 Partition.fold p
		   (fun e2 acc2 ->
		      try
			let (u', a, sigma) = d_create p e2 in
			  match Partition.is_equal p u0 u' with
			    | None -> acc2 
			    | Some(theta) ->
				let ups = Jst.dep4 rho tau sigma theta in
				let e = Fact.Equal.make (v, a, ups) in
				  e :: acc2
			  with
			      Not_found -> acc2)
		   u0 acc1	
	   with
	       Not_found -> acc1)
	u0 

  (** Chaining on disequalities. *)
  let rec of_var_diseq d (p, s) = 
    assert(Fact.Diseq.is_var d);
    of_var_diseq2 d (p, s)
      (of_var_diseq3 d (p, s) [])

  (** (2) [i<>j] and [v = u[j]], [u = a[i:=x]] ==> [v = a[j]]. *)
  and of_var_diseq2 d (p, s) =
    let (i, j, rho) = Fact.Diseq.destruct d in
      E.S.Dep.fold s
	(fun e1 acc1 ->
	   try
	     let (v, (u, j'), tau) = d_select p e1 in
	       if not(Term.eq j j') then acc1 else
		 E.S.Dep.fold s
		   (fun e2 acc2 ->
		      try
			let (u', (a, i', x), sigma) = d_update p e2 in
			  if not(Term.eq u u') || not(Term.eq i i') then acc2 else
			    let theta = Jst.dep3 rho tau sigma in
			    let aj = Funarr.Flat.mk_select a j in
			    let e = Fact.Equal.make (v, aj, theta) in
			      e :: acc2
		      with
			  Not_found -> acc2)
		   i acc1
	   with
	       Not_found -> acc1)
	j

  (** (3) [i<>j] and [u = a[i:=x]] and [v = u[j:=y]] ==> [v = a[j:=y][i:=x]] *)
  and of_var_diseq3 d (p, s) =
    let (i, j, rho) = Fact.Diseq.destruct d in
      E.S.Dep.fold s
	(fun e1 acc1 ->
	   try
	     let (u, (a, i', x), tau) = d_update p e1 in
	       if not(Term.eq i i') then acc1 else
		 E.S.Dep.fold s
		   (fun e2 acc2 ->
		      try
			let (v, (u', j', y), sigma) = d_update p e2 in
			  if not(Term.eq u u') || not(Term.eq j j') then acc2 else
			    let theta = Jst.dep3 rho tau sigma in
			    let aj = Funarr.Flat.mk_select a j in
			    let e = Fact.Equal.make (v, aj, theta) in
			      e :: acc2
		      with
			  Not_found -> acc2)
		   j acc1
	   with
	       Not_found -> acc1)
	i

  exception Found of Fact.Equal.t * Fact.Diseq.t

  (** If [s] contains pattern [u' = v[j]], [v' = a[i:=x]] with 
    - [v = v'] in [p],
    - not([p |= i = j])
    - not([p |= i <> j]),
    then suggest the case split [i = j] or [i <> j]. *)
  let disjunction (p, s) = 
    try
      E.S.iter
	(fun e1 ->
	   let (u', (v, j), rho) = d_select p e1 in
	     E.S.iter
	       (fun e2 ->
		  let (v', (a, i, x), tau) = d_update p e2 in
		    match Partition.is_equal p v v' with
		      | None -> ()
		      | Some(sigma) ->
			  match Partition.is_equal_or_diseq p i j with
			    | Jst.Three.X -> 
				let theta = Jst.dep3 rho tau sigma in
				  raise(Found(Fact.Equal.make(i, j, theta),
					      Fact.Diseq.make(i, j, theta)))
			    | _ -> ())
	       s)
	s;
      raise Not_found
    with
	Found(e, d) -> [Fact.Equal(e); Fact.Diseq(d)]
 
end 

module D0 = Can.Trace(D)

module Infsys0: (Infsys.IS with type e = E.t) =
  Can.Make(T)(D0)


(** Congruence closure inference system. *)
module Infsys: (Infsys.IS with type e = E.t) =
  Infsys.Trace(Infsys0)
    (struct
       type t = E.t
       let level = "arr"
       let eq = E.S.eq
       let diff = E.S.diff
       let pp = E.S.pp
     end)

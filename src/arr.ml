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
  - [u = a[i:=x]] ==> [x = u[i]]
  - [i<>j] and [v = u[j], [u = a[i:=x]] ==> [v = a[j]]
  - [i<>j] and [u = a[i:=x]] and [v = u[j:=y]] ==> [v = a[j:=y][i:=x]], 
  i.e., flatten the rhs and add to the equality graph.

  The first rule is applied eagerly, whereas the rules depending
  on a disequality are added lazily in the canonization process.

  Case splitting needs to be performed on all pairs [(i, j)] such 
  that [u = a'[j]], [a' = a[i:=x]] in order to make the procedure 
  complete.
*)


(** Equality set for arrays *)
module A: Eqs.SET = 
  Eqs.Make0(
    struct
      let th = Th.arr
      let nickname = Th.to_string Th.arr
      let map = Funarr.map Term.is_equal
      let is_infeasible _ = false
    end)

type t = A.t

let eq = A.eq
let empty = A.empty
let is_empty = A.is_empty
let pp = A.pp
let copy = A.copy

let is_independent = A.is_independent
let is_dependent = A.is_dependent

let fold = A.fold
let apply = A.apply
let find = A.find
let inv = A.inv
let dep = A.dep


type config = Partition.t * t
    (** A {i configuration} consists of 
      - variable equalities [v]
      - variable disequalities [d]
      - equalities of the form [x = a], [x] not necessarily
      canonical, and [a] is a flat array term, that is, 
      {!Arr.is_flat}[a] holds. *)

 
(** Apply [f (u, (a, i, x), sigma)] for each [u' = a[i:=x]] in [s] 
  with [u=u'] in [p] and [u] canonical. *)
let update_index_iter (p, s) i f =
  A.Dep.iter s
    (fun e ->
       let (u', b, rho) = Fact.Equal.destruct e in
	 try
	   let (a, i', x) = Funarr.d_update b in
	     if Term.eq i i' then
	       let (u, tau) = Partition.find p u' in
		 f (u, (a, i, x), Jst.dep2 rho tau)
	 with
	     Not_found -> ())
    i

(** Apply [f (u, (a, j), sigma)] for each [u' = a[j] in [s] 
  with [u=u'] in [p] and [u] canonical. *)
let select_index_iter (p, s) j f =
  A.Dep.iter s
    (fun e ->
       let (u', b, rho) = Fact.Equal.destruct e in
	 try
	   let (a, j') = Funarr.d_select b in
	     if Term.eq j j' then
	       let (u, tau) = Partition.find p u' in
		 f (u, (a, j), Jst.dep2 rho tau)
	 with
	     Not_found -> ())
    j

(** Apply [f (u, (a, i, x), sigma)] for each [u' = a[i:=x]] in [s]
  and canonical [u] with [u = u'] in [p]. *)
let update_iter (p, s) u f =
  Partition.iter_if p
    (fun u' ->
       let (b, rho) = apply s u' in
       let (a, i, x) = Funarr.d_update b in
       let (u, tau) = Partition.find p u' in
	 f (u, (a, i, x), Jst.dep2 rho tau))
    u

(** Apply [f (u, (a, j), sigma)] for each [u' = a[j]] in [s]
  and canonical [u] with [u = u'] in [p]. *)
let select_iter (p, s) u f =
  Partition.iter_if p
    (fun u' ->
       let (b, rho) = apply s u' in
       let (a, j) = Funarr.d_select b in
       let (u, tau) = Partition.find p u' in
	 f (u, (a, j), Jst.dep2 rho tau))
    u

(** Apply [f (u, a, sigma)] for each [u' = create(a)] in [s]
  and canonical [u] with [u = u'] in [p]. *)
let create_iter (p, s) u f =
  Partition.iter_if p
    (fun u' ->
       let (b, rho) = apply s u' in
       let a = Funarr.d_create b in
       let (u, tau) = Partition.find p u' in
	 f (u, a, Jst.dep2 rho tau))
    u


(** Create a fresh variable for renaming terms. *)
let mk_rename = 
  let v = Name.of_string "v" in
    fun () -> Term.Var.mk_rename v None Var.Cnstrnt.Unconstrained
      

(** Return interpreted term for a dependent variable.
  The result is always canonical w.r.t. to the 
  configuration [(p, s)]. *)
let interp (p, s) a =
  if Term.is_var a then
    try
      Partition.choose p (apply s) a
    with
	Not_found -> Partition.find p a
  else 
    Jst.Eqtrans.id a


(** Test for disequalities; for now just partitions. *)
let is_diseq (p, _) i j =
  Partition.is_diseq p i j


(** Lazy forward chaining for flat selection and update terms [b]. *)
let rec close c b =
  try
    let (a, j) = Funarr.d_select b in
      close_select c a j
  with
      Not_found -> 
	(try 
	   let (a, i, x) = Funarr.d_update b in
	     close_update c a i x 
	 with 
	     Not_found -> ())
 

(** Lazy forward chaining on selection [a[j]]. The forward
  chains (1), (2), (5) above have [a[j]] in the conclusion
  and are slightly restated as follows.
  - (1') [a = b[j:=x]] ==> [x = a[j]]
  - (2') [i<>j], [v = u[j], [u = a[i:=x]] ==> [v = a[j]]
  - (5') [v = u[j]], [u = create(a)] ==> [v = a[j]] *)
and close_select c a j =
  let aj = Funarr.Flat.mk_select a j in
    update_index_iter c j
      (fun (a, (b, j', x), rho) ->         (* [rho |- a = b[j:=x]] *)
	 assert(Term.eq j j');             (* ==> [rho |- x = a[j]] by (1') *)
	 infer c (Fact.Equal.make (x, aj, rho))); 
    select_index_iter c j
      (fun (v, (u, j'), rho) ->            (* [rho |- v = u[j]] *)
	 assert(Term.eq j j');
	 update_iter c u
	   (fun (u', (a', i, x), tau) ->   (* [tau |- u = a[i:=x]] *)
	      assert(Term.eq u u');
	      if Term.eq a a' then
		match is_diseq c i j with
		  | None -> ()
		  | Some(sigma) ->         (* [sigma |- i <> j] *)
		      let e' = Fact.Equal.make (v, aj, Jst.dep3 rho tau sigma) in
			infer c e');       (* ==> [rho,tau,sigma |- x = a[j]] by (2') *)  
         create_iter c u
	   (fun (u', a, tau) ->            (* [tau |- u = create(a)] *)
	      assert(Term.eq u u');
	      let e' = Fact.Equal.make (v, aj, Jst.dep2 rho tau) in
		infer c e'))               (* ==> [rho, tau |- v = a[j]] *)


(** Lazy forward chaining on updates [a[i:=x]]. The forward
  chains (1), (2), (5) above have updates in the conclusion
  and are slightly restated as follows.
  - (3') [i<>j], [a = b[j:=y]], [u = b[i:=x]], [v = u[j:=y]] ==> [v = a[i:=x]],
  - (4') [u = v[i:=x]], [v = a[i:=y]] ==> [u = a[i:=x]] *)
and close_update c a i x =
  let aix = Funarr.Flat.mk_update a i x in
    update_index_iter c i
      (fun (u, (v, i, x), rho) ->          (* [rho |- u = v[i := x]] *)
	 update_index_iter c i
	   (fun (v', (a, i', y), tau) ->   (* [tau |- v = a[i:=y]] *)
	      if Term.eq v v' && Term.eq i i' then
		let e' = Fact.Equal.make (u, Funarr.Flat.mk_update a i x, Jst.dep2 rho tau) in
		  infer c e');             (* ==> [rho, tau |- u = a[i := x]] by (4') *)
	 ()   (* to do (3') *)
      )

(** Extend [s] with an inferred equation of the form [x = a]. *)
and infer ((p, s) as c) e =
  let (x, a, rho) = Fact.Equal.destruct e in
    assert(Term.is_var x);
    assert(Funarr.Flat.is a);
    try
      let (y, tau) = A.inv s a in
      let e' = Fact.Equal.make (x, y, Jst.dep2 rho tau) in
	Partition.merge p e'
    with
	Not_found -> 
	  if is_dependent s x then
	    begin
	      let u = mk_rename () in
	      let e' = Fact.Equal.make (u, a, Jst.dep0) 
	      and e'' = Fact.Equal.make (x, u, rho) in
		A.update c e';
		Partition.merge p e''
	    end 
	  else 
	    A.update c e

(** Return a name for a flat array term. *)
let name ((p, s) as c) a =
  assert(Funarr.Flat.is a);
  try
    Jst.Eqtrans.compose (Partition.find p) (A.name c) a
  with
      Not_found -> Partition.find p a


(** [uninterp c a] returns a canonical variable which
  is equivalent in a possibly extended configuration [c].
  [c] is destructively updated to contain all subterms in 
  the term universe and [c] is closed under lazy forward chaining,
  if the [extend] flag is set. *)
let rec uninterp ((p, s) as c) a =
  try
    Jst.Eqtrans.compose (Partition.find p) (A.inv s) a
  with
      Not_found -> Partition.find p a

and uninterp_with_chaining ((p, s) as c) a =
  let (a', rho) = install c a in   (* to do: add flag depending on whether or not destructive updates. *)
    assert(Funarr.Flat.is a');
    if Funarr.is_interp a' then
      try
	let (x', tau) = A.inv s a' in
	let (y', sigma) = Partition.find p x' in
	  (y', Jst.dep3 rho tau sigma)
      with
	  Not_found ->
	    let x' = mk_rename () in
	    let e' = Fact.Equal.make (x', a', Jst.dep0) in
	      A.update c e';
	      (x', rho)
    else 
      let (y', tau) = Partition.find p a' in
	(y', Jst.dep2 rho tau)
	
and install c a =
  try
    (match Funarr.d_interp a with
       | Sym.Create, [b] -> 
	   let (b', rho) = uninterp_with_chaining c b in   (* [rho |- b' = b] *)
	     assert(Term.is_var b');
	     if b == b' then Jst.Eqtrans.id a else 
	       (Funarr.Flat.mk_create b', rho)
       | Sym.Select, [b; j] -> 
	   let (b', rho) = uninterp_with_chaining c b in
	   let (j', tau) = uninterp_with_chaining c j in
	     assert(Term.is_var b' && Term.is_var j');
	     close_select c b' j';
	     if b == b' && j == j' then 
	       Jst.Eqtrans.id a 
	     else
	       (Funarr.Flat.mk_select b' j', Jst.dep2 rho tau)
       | Sym.Update, [b; i; x] -> 
	   let (b', rho) = uninterp_with_chaining c b in
	   let (i', tau) = uninterp_with_chaining c i in
	   let (x', sigma) = uninterp_with_chaining c x in
	     assert(Term.is_var b' && Term.is_var i' && Term.is_var x');
	     close_update c b' i' x';
	     if b == b' && i == i' && x == x' then 
	       Jst.Eqtrans.id a 
	     else
	       (Funarr.Flat.mk_update b' i' x', Jst.dep3 rho tau sigma)
       | _ ->
	   assert false)
  with
      Not_found -> Jst.Eqtrans.id a
 

let arr = Th.to_string Th.arr

(** Merging a variable equality [x = y] by replacing [x] by [y] 
  on right-hand sides of [s] and lazy forward chaining on the
  resulting terms. *)
let rec merge ((p, s) as c) e =
  assert(Fact.Equal.is_var e);
  Trace.msg arr "Merge" e Fact.Equal.pp;
  let (x, y, rho) = Fact.Equal.destruct e in
    A.Dep.iter s
      (fun e' -> 
	 let (x', a', rho') = Fact.Equal.destruct e' in
	 let a'' = Funarr.Flat.apply (x, y) a' in
	   close c a'';       (* lazy forward chain for [a']. *)
	   let e'' = Fact.Equal.make (x', a'', Jst.dep2 rho rho') in
	     A.update c e'')
      x



(** {6 Splitting} *)

exception Found of Term.t * Term.t

let split ((p, s) as cfg) =
  try
    (List.iter
      (fun (_, b) ->
	 try
	   let (u', j) = Funarr.d_select b in
	     (fun u -> 
		try
		  let (b, _) = apply s u in
		  let (_, i, _) = Funarr.d_update b in
		    match Partition.is_equal_or_diseq p i j with
		      | Jst.Three.X -> 
			  let (i, j) = Term.orient (i, j) in
			    raise (Found (i, j))
		      | _ -> 
			  ()
		with
		    Not_found -> ())
	     u'
	 with
	     Not_found -> ())
       (A.to_list s);
     raise Not_found)
  with
      Found(i, j) -> (i, j)

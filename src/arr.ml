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
  - (1) [u = a[i:=x]] ==> [x = u[i]]
  - (2) [i<>j] and [v = u[j], [u = a[i:=x]] ==> [v = a[j]]
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

let is_pure = Term.is_pure Th.arr

let is_flat = Funarr.Flat.is

let arr = Th.to_string Th.arr


(** Test for disequalities; for now just partitions. *)
let is_diseq (p, _) =
  Partition.is_diseq p


let is_equal_or_diseq (p, _) i j =
  Partition.is_equal_or_diseq p i j

(** Homomorphically apply equality transformer [f] at 
  uninterpreted positions of [a]. *)
let map c f a =
  let hyps = ref [] in
  let f' x =
    let (a, rho) = f x in
      if not(x == a) then hyps := rho :: !hyps; a
  in
  let is_equal_or_diseq' = 
    Jst.Three.to_three hyps (is_equal_or_diseq c) 
  in
  let b = Funarr.map is_equal_or_diseq' f' a in
    (b, Jst.dep !hyps)


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
  

(** Lookup of dependent variables. *)
let rec lookup ((p, s) as c) x =
  if Term.is_app x then 
    Jst.Eqtrans.id x
  else 
    let apply = Partition.choose p (A.apply s) in 
      try
	Jst.Eqtrans.compose (lookup c) apply x
      with
	  Not_found -> 
	    Partition.find p x
    

(** Recursively replace variables [x] in [a] with [b], 
  with [b] such that [y = b] in [s] for [x] and [y]
  equal modulo [p]. *)
let rec replace c =  
  map c (lookup c)



(** Recursively replace right-hand sides in [s] with 
  corresponding left-hand side variables. *)
let rec abstract ((p, s) as c) a =
  if Term.is_var a then Partition.find p a else 
    try
      Jst.Eqtrans.compose 
	(Partition.find p) 
	(A.inv s) 
	a
    with
	Not_found ->
	  (try
	     (match Funarr.d_interp a with
		| Sym.Create, [b] -> 
		    let (b', rho) = abstract c b in
		      if b == b' then Jst.Eqtrans.id a else
			let (a', tau) = abstract c (Funarr.mk_create b') in
			  (a', Jst.dep2 rho tau)
		| Sym.Select, [b; j] -> 
		    let (b', rho) = abstract c b
		    and (j', tau) = abstract c j in
		      if b == b' && j == j' then Jst.Eqtrans.id a else
			let (a', sigma) = 
			  abstract c (Funarr.mk_select Term.is_equal b' j') 
			in
			  (a', Jst.dep3 rho tau sigma)
		| Sym.Update, [b; i; x] -> 
		    let (b', rho) = abstract c b
		    and (i', tau) = abstract c i
		    and (x', sigma) = abstract c x in
		      if b == b' && i == i' && x == x' then 
			Jst.Eqtrans.id a 
		      else
			let (a', theta) = 
			  abstract c (Funarr.mk_update Term.is_equal b' i' x') 
			in
			  (a', Jst.dep4 rho tau sigma theta)
	        | _ ->
		    Jst.Eqtrans.id a)
	   with
	      Not_found -> Jst.Eqtrans.id a)

let abstract c = Jst.Eqtrans.trace "foobar" "Arr.abstract" (abstract c)


(** {i Canonization} with respect to a configuration [c]
  of the form [(v, s)] with [v] variable equalities and [s]
  an array context.
  - [C[| . |] = (C^-1(.) o C[.])] *)
let can c =
  Jst.Eqtrans.compose
    (abstract c)
    (replace c)


(** Lazy forward chaining for flat selection and update terms [b]. *)
let rec lazy_chain c b =
  try
    let (a, j) = Funarr.d_select b in
      lazy_chain_select c a j
  with
      Not_found -> 
	(try 
	   let (a, i, x) = Funarr.d_update b in
	     lazy_chain_update c a i x 
	 with 
	     Not_found -> ())
 
(** Lazy forward chaining on selection [a[j]].
  - (2') [i<>j], [v = u[j], [u' = a[i:=x]], [u = u'] ==> [v = a[j]] *)
and lazy_chain_select c a j =
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
		    let rho' = Jst.dep3 rho tau sigma in
		    let e' = Fact.Equal.make (v, Funarr.Flat.mk_select a j, rho') in
		      process_equal c e'))   (* ==> [rho,tau,sigma |- x = a[j]] by (2') *)  


(** Lazy forward chaining on updates [a[i:=x]].
 - (3') [i<>j], [a = b[j:=y]], [u = b[i:=x]], [v = u[j:=y]] ==> [v = a[i:=x]] *)
and lazy_chain_update c a i x =
  () (* to do (3') *)


(** Introduce a new name [u] for the flat term [a] if necessary 
  and extend the context with a new equality [u = a]. The result
  is always a variable. *)
and flatname ((p, s) as c) a =
  assert(is_flat a);
  lazy_chain c a;
  try
    Jst.Eqtrans.compose 
      (Partition.find p) 
      (A.inv s) 
      a
  with
      Not_found ->
	let u = mk_rename () in
	let rho = Jst.dep0 in
	let e = Fact.Equal.make (u, a, rho) in
	  update c e;
	  (u, rho)


(** Flatten a pure array term [a] and introduce variables as
  necessary for naming subterms. The result is a variable [z]
  equal to [a] in the extended context. *)
and name ((p, s) as c) a =
  assert(is_pure a);
  if Term.is_var a then
    Partition.find p a 
  else 
    try
      Jst.Eqtrans.compose 
	(Partition.find p) 
	(A.inv s) a
    with
	Not_found -> 
	  Jst.Eqtrans.compose
	    (flatname c)
	    (map c (name c)) a


(** Extend [s] with an updated equation of the form [x = a]
  and close under forward chaining. *)
and update ((p, s) as c) e =
  let (x, a, rho) = Fact.Equal.destruct e in
    assert(Term.is_var x);
    assert(is_flat a);
    A.update c e;
    chain c e


(** Forward chaining on a new equality. *)
and chain c e =
  let (u, a, rho) = Fact.Equal.destruct e in
    try
      (match Funarr.d_interp a with
	 | Sym.Update, [b; i; x] -> 
	     chain_update c (u, (b, i, x), rho)
	 | Sym.Select, [b; j] -> 
	     chain_select c (u, (b, j), rho)
	 | Sym.Create, [b] ->
	     chain_create c (u, b, rho)
	 | _ ->
	     ())
    with
	Not_found -> ()


(** Closing the state for a new equality [u = a[i:=x]] by applying
  all forward chains with such an antecedent. *)
and chain_update c e =   
  chain_update_1 c e;
  chain_update_2 c e;
  chain_update_3 c e;
  chain_update_4 c e


(** Forward chain on 
  - (1) [u' = a[i:=x]], [u = u'] ==> [x = u[i]] *)  (* [rho |- u = a[i:=x]] *)
and chain_update_1 ((p, _) as c) (u', (a, i, x), rho) = 
  let (u, tau) = Partition.find p u' in             (* [tau |- u = u'] *) 
  let ui = Funarr.Flat.mk_select u i in
  let e1 = Fact.Equal.make (x, ui, Jst.dep2 rho tau) in
    Trace.msg "arr''" "Arr.chain(1)" e1 Fact.Equal.pp;
    process_equal c e1


(** Forward chain on third hypothesis in
  - (2') [i<>j] and [v = u'[j]], [u = a[i:=x]], [u = u']] ==> [v = a[j]] *)
and chain_update_2 ((p, _) as c) (u, (a, i, x), rho) =
  Trace.msg "foo" "Chain2" (Fact.Equal.make (u,  (Funarr.Flat.mk_update a i x), rho)) Fact.Equal.pp;
  D.Set.iter                              (* [rho |- u = a[i:=x]] *)
    (fun (j, tau) ->                      (* [tau |- i <> j] *)
       Trace.msg "foo" "Diseq" j Term.pp;
       select_index_iter c j
       (fun (v, (u', j'), sigma) ->     (* [sigma |- v = u[j]] *)
	  assert(Term.eq j j');
          Trace.msg "foo" "Select" (Funarr.Flat.mk_select u' j) Term.pp;
	  match Partition.is_equal p u u' with
	    | None -> ()
	    | Some(theta) ->            (* [theta |- u = u'] *)
		let rho' = Jst.dep4 rho tau sigma theta in
		let e' = Fact.Equal.make (v, Funarr.Flat.mk_select a j, rho') in
		  Trace.msg "arr''" "Arr.chain(2)" e' Fact.Equal.pp;
		  process_equal c e'))
    (Partition.diseqs p i)


(** Forward chain on update hypotheses in
  - (3') [i<>j], [u = a[i:=x]], [v = u'[j:=y]], [u = u'] ==> [v = a[j:=y][i:=x]] *)
and chain_update_3 ((p, _) as c) (u, (a, i, x), rho) =
  D.Set.iter                              (* [rho |- u = a[i:=x]] *)
    (fun (j, tau) ->                      (* [tau |- i <> j] *)
       update_index_iter c j
       (fun (v, (u', j', y), sigma) -> (* [sigma |- v = u[j:=y]] *)
	  assert(Term.eq j j');
	  match Partition.is_equal p u u' with
	    | None -> ()
	    | Some(theta) ->           (* [theta |- u = u'] *)
		let (ajy, ups) = name c (Funarr.Flat.mk_update a j y) in
		let rho' = Jst.dep5 rho tau sigma theta ups in
		let e' = Fact.Equal.make (v, Funarr.Flat.mk_update ajy i x, rho') in
		  Trace.msg "arr''" "Arr.chain(3)" e' Fact.Equal.pp;
		  process_equal c e'))
    (Partition.diseqs p i)

(** Forward chain on
  - (4') [u = a[i:=x]], [v = u'[i:=y]], [u = u'] ==> [v = a[i:=y]] *)
and chain_update_4 ((p, _) as c) (u, (a, i, x), rho) =
  update_index_iter c i                     (* [rho |- u = a[i:=x]] *)
    (fun (v, (u', i', y), tau) ->           (* [tau |- v = u'[i':=y]] *)
       assert(Term.eq i i');
       match Partition.is_equal p u u' with
	 | None -> ()
	 | Some(sigma) ->                   (* [sigma |- u = u'] *)
	     let rho' = Jst.dep3 rho tau sigma in
	     let e' = Fact.Equal.make (v, Funarr.Flat.mk_update a i y, rho') in
	       Trace.msg "arr''" "Arr.chain(4)" e' Fact.Equal.pp;
	       process_equal c e')
					      

(** Forward chaining on selection hypotheses. *)  
and chain_select c e = 
  chain_select_2 c e;
  chain_select_5 c e


(** Forward chaining on selection
 - (2') [i<>j] and [v = u[j]], [u' = a[i:=x]], [u = u'] ==> [v = a[j]] *)
and chain_select_2 ((p, _) as c) (v, (u, j), rho) = 
  D.Set.iter                              (* [rho |- v = u[j]] *)
    (fun (i, tau) ->                      (* [tau |- i <> j] *)
       update_index_iter c i
         (fun (u', (a, i', x), sigma) ->  (* [sigma |- u = a[i:=x]] *)
	    assert(Term.eq i i');
	    match Partition.is_equal p u u' with
	      | None -> ()
	      | Some(ups) ->              (* [ups |- u = u'] *)
		  let rho' = Jst.dep4 rho tau sigma ups in
		  let e' = Fact.Equal.make (v, Funarr.Flat.mk_select a j, rho') in
		    Trace.msg "arr''" "Arr.chain(2)" e' Fact.Equal.pp;
		    process_equal c e'))
    (Partition.diseqs p j)


(** Forward chaining on selection 
 - (5') [u = v[j]], [v' = create(a)], [v = v'] ==> [u = a] *)
and chain_select_5 ((p, s) as c) (u, (v, j), rho) = 
  Partition.iter_if p                      (* [rho |- u = v[j]] *)
    (fun v' ->
       let (b, tau) = A.apply s v' in      (* [tau |- v' = create(a)] *)
       let a = Funarr.d_create b in
	 assert(Term.is_var a);
	 match Partition.is_equal p v v' with
	   | Some(sigma) ->                (* [sigma |- v = v'] *)
	       let rho' = Jst.dep3 rho tau sigma in
	       let e' = Fact.Equal.make (u, a, rho') in
		 Trace.msg "arr''" "Arr.chain(5)" e' Fact.Equal.pp;
		 Partition.merge p e'
	   | None -> ())
    v

  

(** Forward chaining on creation
  - (5') [u = v'[j]], [v = create(a)], [v = v'] ==> [u = a] *)
and chain_create ((p, s) as c) (v, a, rho) = (* [rho |- v = create(a)] *)
  let (v', tau) = Partition.find p v in      (* [tau |- v = v'] *)
    A.Dep.iter s
      (fun e ->
       let (u, b, sigma) = Fact.Equal.destruct e in
	 try                                 (* [sigma |- u = v'[j]] *)
	   let (v'', j) = Funarr.d_select b in
	     if Term.eq v'' v' then
	       let rho' = Jst.dep3 rho tau sigma in
	       let e' = Fact.Equal.make (u, Funarr.Flat.mk_select v' j, rho') in
		 Trace.msg "arr''" "Arr.chain(5)" e' Fact.Equal.pp;
		 process_equal c e'
	 with
	     Not_found -> ())
      v'  



(** Merging a variable equality [x = y] by replacing [x] by [y] 
  on right-hand sides of [s] and lazy forward chaining on the
  resulting terms. *)
and merge ((p, s) as c) e =
  assert(Fact.Equal.is_var e);
  let (x, y, rho) = Fact.Equal.destruct e in
    A.Dep.iter s
      (fun e' -> 
	 let (x', a', rho') = Fact.Equal.destruct e' in
	 let a'' = Funarr.Flat.apply (x, y) a' in
	 let e'' = Fact.Equal.make (x', a'', Jst.dep2 rho rho') in
	   update c e'')
      x


(** Like [name], but for variables do not necessarily return canonical
  variable. *)
and flatten c a =
  if Term.is_var a then
    Jst.Eqtrans.id a
  else 
    name c a


(** Process an equality over pure array terms by flattening both sides
  followed by merging the resulting variables. *)
and process_equal ((p, _) as c) e = 
  assert(Fact.Equal.both_sides is_pure e);
  let e = Fact.Equal.map (flatten c) e in
    assert(Fact.Equal.is_var e);
    Partition.merge p e;
    merge c e


and process_diseq ((p, _) as c) d = 
  assert(Fact.Diseq.both_sides is_pure d);
  let d = Fact.Diseq.map (flatten c) d in
    assert(Fact.Diseq.is_var d);
    Partition.dismerge p d;
    dismerge c d


(** Propagating a disequality [i <> j] using forward chains [(2)] and [(3)]. *)
and dismerge c d =
  chain_on_diseq_2 c d;
  chain_on_diseq_3 c d


(** Propagating a disequality [i <> j] using
  - (3') [i<>j], [u = a[i:=x]], [v = u'[j:=y]], [u = u'] ==> [v = a[j:=y][i:=x]] *)
and chain_on_diseq_3 ((p, s) as c) d =
  let (i, j, rho) = Fact.Diseq.destruct d in  (* [rho |- i <> j] *)
    update_index_iter c i
      (fun (u, (a, i', x), tau) ->            (* [tau |- u = a[i:=x]] *)
	 assert(Term.eq i i');
	 update_index_iter c j
	   (fun (v, (u', j', y), sigma) ->    (* [sigma |- v = u[j:=y]] *)
	      assert(Term.eq j j');
	      match Partition.is_equal p u u' with
		| None -> ()
		| Some(ups) ->                (* [ups |- u = u'] *)
		    let (ajy, nu) = name c (Funarr.Flat.mk_update a j y) in
		    let rho' = Jst.dep5 rho tau sigma ups nu in
		    let e' = Fact.Equal.make (v, Funarr.Flat.mk_update ajy i x, rho') in
		      Trace.msg "arr''" "Arr.chain(3)" e' Fact.Equal.pp;
		      process_equal c e'))
  

and chain_on_diseq_2 ((p, s) as c) d =
  let (i, j, rho) = Fact.Diseq.destruct d in  (* [rho |- i <> j] *)
  let chain2 (i, j) = 
    select_index_iter c j
      (fun (v, (u, j'), tau) ->               (* [tau |- v = u[j]] *)
	 assert(Term.eq j j');
	 update_index_iter c i
	   (fun (u, (a, i', x), sigma) ->     (* [sigma |- u = a[i := x]] *)
	      assert(Term.eq i i');
	      let theta = Jst.dep3 rho tau sigma in
	      let e = Fact.Equal.make (v, Funarr.Flat.mk_select a j, theta) in
		Trace.msg "arr''" "Arr.chain(2)" e Fact.Equal.pp;
		process_equal c e))
  in
    chain2 (i, j);  (* apply symmetrically *)
    chain2 (j, i)


(** Tracing. *)
let name c = Jst.Eqtrans.trace "arr" "name" (name c)
let process_diseq c = Trace.proc "arr" "process" Fact.Diseq.pp (process_diseq c)
let process_equal c = Trace.proc "arr" "process" Fact.Equal.pp (process_equal c)
let merge c = Trace.proc "arr" "merge" Fact.Equal.pp (merge c)
let dismerge c = Trace.proc "arr" "dismerge" Fact.Diseq.pp (dismerge c)


exception Found of Term.t * Term.t

(** Return a possible case split. *)
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

(*i
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
 i*)

(** Equality set for arrays *)
module A: Eqs.SET = 
  Eqs.Make0(
    struct
      let th = Th.arr
      let nickname = Th.to_string Th.arr
      let apply = Funarr.apply Term.is_equal
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

(** Test if [a] is a {i flat} term, that is , either uninterpreted or
  an application of an interpreted operation to variables only. *)
let is_flat a = 
  try
    let (_, xl) = Funarr.d_interp a in
      List.for_all Term.is_var xl
  with
      Not_found -> true

let apply = A.apply
let find = A.find
let inv = A.inv
let dep = A.dep

module Iter = struct
  (** Apply [f (v, (a, i), tau)] to each [tau |- v = a[i]] in [s]. *)
  let select_index s i f =
    A.Dep.iter s
      (fun e ->
	 let (v, b, tau) = Fact.Equal.destruct e in
	   try
	     let (a, i') = Funarr.d_select b in
	       if Term.eq i i' then
		 f (v, (a, i), tau)
	   with
	       Not_found -> ())
      i

  (** Select two equalities [u1 = a1[i]] and [u2 = a2[i]] with index [i] and 
    apply [f] to them if [u1] is not equal to [u2]. *)
  let select2_index s i f =
    select_index s i
      (fun ((u1, _, _) as e1) ->
	 select_index s i
	   (fun ((u2, _, _) as e2) -> 
	      if not(Term.eq u1 u2) then
		f e1 e2))


  (** Apply [f (v, (a, i, x), tau)] to each [tau |- v = a[i:=x]] in [s]. *)
  let update_index s i f =
    A.Dep.iter s
      (fun e ->
	 let (v, b, tau) = Fact.Equal.destruct e in
	   try
	     let (a, i', x) = Funarr.d_update b in
	       if Term.eq i i' then
		 f (v, (a, i, x), tau)
	   with
	       Not_found -> ())
      i

  (** Apply [f (v, (a, i, x), rho)] such that [rho |- v = a[i:=x]] *)
  let update (p, s) u f =
    Partition.iter_if p
      (fun v ->
	 try
	   let (b, rho) = apply s v in
	     f (u, Funarr.d_update b, rho)
	 with
	     Not_found -> ())
      u
end 


let interp (p, s) = Partition.choose p (apply s)


let uninterp (p, s) a =
  try
    Jst.Eqtrans.compose
      (Partition.find p)
      (inv s)
      a
  with
      Not_found -> Partition.find p a


(** Return the disequalities for an array term [a]. *)
let diseqs ((p, _) as cfg) a =
  try
    let d = Partition.d_of p in 
      D.map_diseqs d (uninterp cfg) a
  with
      Not_found -> D.Set.empty


(** [is_equal cfg a b] holds iff [a] and [b] are
  equal in the configuration [cfg]. *)
let is_equal ((p, _) as cfg) =
  Jst.Pred2.apply (uninterp cfg)
    (Partition.is_equal p)


let is_equal_or_diseq ((p, _) as cfg) =
  Jst.Rel2.apply (uninterp cfg)
    (Partition.is_equal_or_diseq p)

let mk_select = Funarr.mk_select Term.is_equal

let mk_update = Funarr.mk_update Term.is_equal

let d_update cfg a = 
  let (a', alpha') = interp cfg a in
  let (b, i, j) = Funarr.d_update a' in
    (b, i, j, alpha')


type config = Partition.t * t

let arr = Th.to_string Th.arr

let merge p e =
  Trace.msg arr "Merge" e Fact.Equal.pp;
  Partition.merge p e


let name cfg a =
  let (x, rho) = A.name cfg a in
    Trace.msg arr "Name" (x, a) Term.Equal.pp;
    (x, rho)

(** Variable-abstract an array term. The result is a variable, and
  the state is updated with flat array terms. *)
let rec abstract cfg a =
  try
    let op, al = Funarr.d_interp a in
    let (bl, rhol) = Fact.Equal.Inj.mapl (abstract cfg) al in 
    let b = Funarr.sigma Term.is_equal op bl in         (* [rho |- a = b] *)
    let rho = Jst.abstract (a, b) rhol in
    let (x, tau) = name cfg b in
      (x, Jst.trans x b a tau rho)
  with
      Not_found -> Jst.Eqtrans.id a

  
let rec process_equal cfg e =
  assert(Fact.Equal.is_pure Th.arr e);
  Trace.msg arr "Process" e Fact.Equal.pp;
  let e = Fact.Equal.map (abstract cfg) e in
    A.fuse cfg [e];   (* keep rhs canonical *)
    arr1_eq cfg e;    (* and forward chain on equality preconditions. *)
    arr3_eq cfg e;
    arr4_eq cfg e


and process_diseq cfg d =
  assert(Fact.Diseq.is_var d);
  Trace.msg arr "Process" d Fact.Diseq.pp;
  let d = Fact.Diseq.map (abstract cfg) d in
    arr2_deq cfg d;      (* forward chain on disequality precondition. *)
    arr4_deq cfg d;
    arr5_deq cfg d;
    arr6_deq cfg d


(** I. [a[i:=x][i] = x] *)
and arr1_eq ((p, s) as cfg) e =
  let (i, j, rho) = Fact.Equal.destruct e in  (* [rho |- i = j] *)
  let propagate (i, j) = 
    Iter.select_index s j
      (fun (u, (v, j), tau) ->                (* [tau |- u = v[j]] *)
	 Iter.update cfg v  
	 (fun (v, (a, i', x), sigma) ->       (* [sigma |- v = a[i:=x]] *)
	    if Term.eq i i' then              (* ==> [phi |- u = x] *)
	      let phi = Jst.array 1 u x [rho; tau; sigma] in
	      let e' = Fact.Equal.make (u, x, phi) in
		merge p e';
		A.restrict cfg u))
  in
    propagate (i, j);
    propagate (j, i)


(** II. [i <> j] implies [a[i:=x][j] = a[j]] *)
and arr2_deq ((_, s) as cfg) d =
  let (i', j', rho') = Fact.Diseq.destruct d in
  let propagate (i', j') =
    Iter.select_index s j'
      (fun (v, (u, j), rho) ->                (* [rho |- v = u[j]] *)
	 assert(Term.eq j j');
	 Iter.update cfg u
	   (fun (u, (a, i, x), tau) ->        (* [tau |- u = a[i:=x]] *)
	      if Term.eq i i' then            (* ==> [phi |- v = a[j]] *)
		let aj = mk_select a j in    
		let phi = Jst.array 2 v aj [rho'; rho; tau] in
		let e = Fact.Equal.make (v, aj, phi) in
		  A.update cfg e))
  in
    propagate (i', j');
    propagate (j', i')

	      
(** III. [a[j:=x] = b[j:=y]] implies [x = y]. *)
and arr3_eq ((p, _) as cfg) e =
  let (u, v, rho) = Fact.Equal.destruct e in    (* [rho |- u = v] *)
    Iter.update cfg u
      (fun (u, (a, j, x), tau) ->               (* [tau |- u = a[j:=x]] *)
	 (Iter.update cfg v 
	    (fun (v, (b, j', y), sigma) ->      (* [sigma |- v = b[j':= y]] *)
	       if Term.eq j j' then             (* ==> [phi' |- x = y] *)
		 let phi' = Jst.array 3 x y [rho; tau; sigma] in
		 let e' = Fact.Equal.make (x, y, phi') in
		   merge p e')))          
	  

(** IV. [a[j:=x] = b[k:=y]], [i<>j], [i<>k] implies [a[i] = b[i]] *)
and arr4_eq ((p, _) as cfg) e =
  let (u, v, rho) = Fact.Equal.destruct e in   (* [rho |- u = v] *)
    Iter.update cfg u
      (fun (u, (a, j, x), tau) ->              (* [tau |- u = a[j:=x]] *)
	 Iter.update cfg v 
	    (fun (v, (b, k, y), sigma) ->      (* [sigma |- v = b[k:= y]] *)
	       D.Set.iter              
	          (fun (i, phi) ->             (* [phi |- i <> j] *)
		     (D.Set.iter
			(fun (i', theta) ->    (* [theta |- i <> k] *)
			   if Term.eq i i' then  
			     let ai = mk_select a i and bi = mk_select b i in
			     let phi'' = Jst.array 4 ai bi [rho; tau; sigma; phi; theta] in
			     let e'' = Fact.Equal.make (ai, bi, phi'') in
			       merge p (Fact.Equal.map (name cfg) e''))
			(diseqs cfg k)))
	           (diseqs cfg j)))
      

and arr4_deq ((p, s) as cfg) d =
  let (i, j, rho) = Fact.Diseq.destruct d in
  let propagate (i, j) =
    Iter.update_index  s j
      (fun (u, (a, j, x), tau) ->                   (* [tau |- u = a[j :=x]] *)
	 (D.Set.iter
	    (fun (i, sigma) ->                      (* [sigma |- i <> j] *)
	       (D.Set.iter
		  (fun (k, ups) ->                  (* [ups |- i <> k] *)
		     (Iter.update_index s k
		        (fun (v, (b, k, y), phi) -> (* [phi |- v = b[k:=y]] *)
			   if not(Term.eq a b) then  
			     let ai = mk_select a i and bi = mk_select b i in
			     let theta = Jst.array 4 ai bi [rho; tau; sigma; ups; phi] in
			     let e = Fact.Equal.make (ai, bi, theta) in
			       merge p (Fact.Equal.map (name cfg) e))))
		  (diseqs cfg i)))
	    (diseqs cfg j)))
  in 
    propagate (i, j);
    propagate (j, i)

		   
(** V. [i <> j] and [i <> k] implies [a[j:=x][i] = a[k:=y][i]]  *)
and arr5_deq ((p, s) as cfg) d = 
  let (i, j, rho) = Fact.Diseq.destruct d in
  let propagate (i, j) = 
    Iter.select2_index s i
      (fun (v1, (u1, i), rho1)                    (* [rho1 |- v1 = u1[i]] *)
	   (v2, (u2, i), rho2) ->                 (* [rho2 |- v2 = u2[i]] *)
	     match Partition.is_equal p v1 v2 with
	       | Some _ -> ()
	       | None -> 
		   Iter.update cfg u1
	            (fun (u1, (a, j, x), tau) ->      (* [tau |- u1 = a[j:=x]] *)
	               D.Set.iter
	                (fun (k, sigma) ->              (* [sigma |- i <> k] *)
		          Iter.update cfg v2  
		           (fun (v2, (a', k, y), theta) -> 
			      if Term.eq a a' then  (* [theta |- w2 = a[k:=y]] *)
			        let phil = [rho; rho1; rho2; tau; sigma; theta] in
			        let phi = Jst.array 5 v1 v2 phil in
			        let e = Fact.Equal.make (v1, v2, phi) in
				  merge p e))
	                 (diseqs cfg i)))
  in
    propagate (i, j);
    propagate (j, i)


(** VI. [i <> j] ==> a[i:=x][j:= y] = a[j:=y][i:=x] *)
and arr6_deq ((p, s) as cfg) d =
  let (i, j, rho) = Fact.Diseq.destruct d in  (* [rho |- i <> j]. *)
    Iter.update_index s j
      (fun (u, (u', j, y), tau1) ->           (* [tau1 |- u = u'[j:=y]]  *)
	 Iter.update_index s i
	 (fun (v, (v', i, x), sigma1) ->      (* [sigma1 |- v = v'[i:=x]]  *)
	    Iter.update cfg u'
	      (fun (u', (a, i', x'),tau2) ->  (* [tau2 |- u' = a[i:=x]] *)
		 if Term.eq i i' && Term.eq x x' then
		   Iter.update cfg v'
		     (fun (v', (a', j', y'),sigma2) -> (* [sigma2 |- v' = a[i:=x]] *)
		 if Term.eq a a' && Term.eq j j' && Term.eq y y' then
		   let phi = Jst.array 6 u v [rho; tau1; sigma1; tau2; sigma2] in
		     merge p (Fact.Equal.make (u, v, phi))))))
	


(** {6 Splitting} *)

let splits ((p, s) as cfg) =
  A.fold
    (fun v (b, rho) acc ->
       try
	 let (u, j) = Funarr.d_select b in
	 let acc' = ref acc in
	   Iter.update cfg u 
	     (fun (u, (a, i, x), _) ->
		let (i, j) = Term.orient (i, j) in
		  acc' := Term.Set2.add (i, j) !acc');
	   !acc'
       with
	   Not_found -> acc)
    s
    Term.Set2.empty
       

let process_complete ((p, s)  as cfg) = 
  failwith "Arr.process_complete: to do"
  

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

(** Decision procedure for arrays based on canonization.

  A configuration [C] consists of a triple [(V, D, S)] with 
  - [V] a set of variable equalities,
  - [D] a set of variable disequalities,
  - [S] a set of array equalities [x = a] with [x] a variable and [a] a pure array term.

  The pair [(V, D)] is passed around as a partitioning [P].

  Forward chaining on
  - if [a[i:=x] = b[i:=y]], then [x = y]
  - if [i<>j], [k<>j], [a[i:=x] = b[k:=y]], then [a[j] = b[j]]
  - if [i<>j], [a[i:=x]= b[j:=y]] then [a[j] = y]

  is used to keep configurations {i confluent}. 
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

(** Return canonical name for an array term [a]. *)
let name (p, s) =
  Jst.Eqtrans.compose
    (Partition.find p)
    (A.name (p, s))


(** Normalizing the application [op(a1,...,an)] of the array 
  operator [op] w.r.t. to the partition [p]. *)
let norm p op al =
  let hyps = ref [] in
  let is_equal' = 
    Jst.Three.to_three hyps 
      (Partition.is_equal_or_diseq p)
  in
  let b = Funarr.sigma is_equal' op al in
    (b, Jst.dep !hyps)


type config = Partition.t * t

(** Inverse find: plugs in variables for interpreted terms
  - [C^-1[x] = find(V)(x)]
  - [C^-1[a] = y]  if [y' = a] in [E] and [y = find(V)(y')]
  - [C^-1[a] = a]  otherwise. *)
let invfind ((p, s) as c) a =
  if Term.is_var a then
    Partition.find p a
  else 
    try
      Jst.Eqtrans.compose (Partition.find p) (A.inv s) a
    with
	Not_found -> Jst.Eqtrans.id a
	
(** Canonization. *)
let rec can ((p, _) as c) a =
  try
    (match Funarr.d_interp a with
       | Sym.Create, [b] -> 
	   let (b', sigma) = can c b in
	     if b == b' then Jst.Eqtrans.id a else
	       let (a', rho) = norm p Sym.Create [b'] in
	       let (a'', tau) = invfind c a' in
		 (a'', Jst.dep3 rho tau sigma)
       | Sym.Update, [b; i; x] ->
	   let (b', sigma') = can c b
	   and (i', sigma'') = can c i 
	   and (x', sigma''') = can c x in
	     if b == b' && i == i' && x == x' then Jst.Eqtrans.id a else
	       let (a', rho) = norm p Sym.Update [b'; i'; x'] in
	       let (a'', tau) = invfind c a' in
		 (a'', Jst.dep5 rho tau sigma' sigma'' sigma''')
       | Sym.Select, [b; j] ->
	   let (b', sigma') = can c b 
	   and (j', sigma'') = can c j in
	     if b == b' && j == j' then Jst.Eqtrans.id a else
	       let (a', rho) = norm p Sym.Select [b'; j'] in
	       let (a'', tau) = invfind c a' in
		 (a'', Jst.dep4 rho tau sigma' sigma'')
       | _ -> 
	   Partition.find p a)
  with
      Not_found -> Partition.find p a


(** {6 Updates} *)

let rec merge ((p, s) as c) e =
  assert(Fact.Equal.is_pure Th.arr e);
  let e = Fact.Equal.map (Jst.Eqtrans.compose (A.name c) (can c)) e in
  let (x, y, rho) = Fact.Equal.destruct e in        (* [rho |- x = y]. *)
    match Partition.is_equal_or_diseq p x y with 
      | Jst.Three.Yes _ ->
	  ()   (* skip *)
      | Jst.Three.No(tau) ->                        (* [tau |- x <> y]. *)
	  raise(Jst.Inconsistent(Jst.dep2 rho tau))
      | Jst.Three.X -> 
	  merge_v c e


and dismerge ((p, s) as c) d = 
  let d = Fact.Diseq.map (Jst.Eqtrans.compose (A.name c) (can c)) d in
  let (x, y, rho) = Fact.Diseq.destruct d in        (* [rho |- x <> y]. *)
    match Partition.is_equal_or_diseq p x y with 
      | Jst.Three.No _ ->
	  ()    (* skip *)
      | Jst.Three.Yes(tau) ->                       (* [tau |- x = y]. *)
	  raise(Jst.Inconsistent(Jst.dep2 rho tau))
      | Jst.Three.X -> 
	  dismerge_v c d


and merge_v ((p, s) as c) e =
  assert(Fact.Equal.is_var e);
  Partition.merge p e;
  fuse c e;
  close_merge c e


(** Fuse an equality [e] of the form [x = b] on rhs [a] of the
  array equalities [u = a] and normalize the resulting [a'] according
  to the variable equalities and disequalities in [p]. Assumes that
  the equality [e] is already merged in [p].*)
and fuse ((p, s) as c) e =
  A.Dep.iter s
    (fun e -> 
       let e' = Fact.Equal.map_rhs (can c) e in
	 if not(e == e') then A.update c e')
    (Fact.Equal.lhs_of e)

    
and dismerge_v ((p, s) as c) d =
  assert(Fact.Diseq.is_var d);
  Partition.dismerge p d;
  fission c d;
  close_dismerge c d

(** Propagate a disequality [x <> y] on rhs [a] of the array 
  equalities [u = a']. Assumes the disequalities is already dismerged in [p]. *)
and fission ((p, s) as c) d =
  A.Dep.iter s
    (fun e -> 
       let e' = Fact.Equal.map_rhs (can c) e in
	 if not(e == e') then A.update c e')
    (Fact.Diseq.lhs_of d)


(** Forward chaining on 
  - if [a[i:=x] = b[i:=y]], then [x = y]
  - if [i<>j], [k<>j], [a[i:=x] = b[k:=y]], then [a[j] = b[j]]
  - if [i<>j], [a[i:=x]= b[j:=y]] then [a[j] = y]

  This is realized by
  - (1) whenever [u'] is merged with [v'], then
       for [u = a[i:=x]], [v = b[i:=y]] with [u =V u'], [v =V v']
          add [x = y]. 
  - (2a) whenever [u'] is merged with [v'], then
         for [u = a[i:=x]], [v = b[k:=y]] with [u =V u'], [v =V v'],
         for all [j] with [i<>j], [k<>j], 
            add [a[j] = b[j]].
  - (2b) whenever [i <> j] is dismerged, then
          for [u = a[i:=x]],
          for [k] such that [k <> j], 
          for [v = b[k:=y]] with [u =V v], 
             add a[j] = b[j]
  - (3a) whenever [u'] is merged with [v'], then
            for  [u = a[i:=x]] and [v = b[k:=y]] with [i <> k], [u =V u'], [v =V v'],
            add [a[k] = y].
  - (3b) whenever [i <> j] is dismerged, then 
           for [u = a[i:=x]], [v = b[j:=y]] with [u =V v], 
	     add [a[j] = y] and [b[i] = x].

  [close_merge] forward chains using (1), (2a), (3a), and
  [close_dismerge] forward chains using (2b), (3b). 
*)
and close_dismerge c d =
  close_dismerge_2b c d;
  close_dismerge_3b c d
 
and close_dismerge_3b ((p, s) as c) d =
  let (i, j, rho') = Fact.Diseq.destruct d in  
    update_index_iter c i
      (fun (rho, u, (a, i', x)) ->                             (* [rho |- u = a[i := x]] *)
	 assert(Term.eq i i');
	 update_index_iter c j 
	    (fun (tau, v, (b, j', y)) ->                       (* [tau |- v = b[j:= y]] *)
	       assert(Term.eq j j');  
	       match Partition.is_equal p u v with
		 | None -> ()
		 | Some(ups) ->                                (* [ups |- u = v] *)
		     let sigma = Jst.dep4 rho' rho tau ups in  (* ==> [sigma |- a[j] = y, b[i] = x]. *)
		     let e1 = Fact.Equal.make (mk_select a j, u, sigma) (* by Rule (3b). *)
		     and e2 = Fact.Equal.make (mk_select b i, x, sigma) in
		       merge c e1;
		       merge c e2))

and close_dismerge_2b ((p, s) as c) d =
  let (i, j, rho') = Fact.Diseq.destruct d in  
    update_index_iter c i
      (fun (rho, u, (a, i', x)) ->                             (* [rho |- u = a[i := x]] *)
	 assert(Term.eq i i');
	 D.Set.iter
	   (fun (k, tau) ->                                    (* [tau |- k <> j] *) 
	      update_index_iter c k
	        (fun (sigma, v, (b, k', y)) ->                 (* [sigma |- v = b[k:= y]] *)
		   assert(Term.eq k k');
		   match Partition.is_equal p u v with
		     | None -> ()
		     | Some(ups) ->                            (* [ups |- u = v] *)
			 let theta' = Jst.dep5 rho' rho tau sigma ups in
			 let e' = Fact.Equal.make (mk_select a j, mk_select b j, theta') in
			   merge c e'))
	   (Partition.diseqs p j))

 
and close_merge ((p, s) as c) e =
  let (u', v', rho') = Fact.Equal.destruct e in                (* [rho' |- u' = v'] *)
    update_iter c u'
      (fun (_, (a, i, x), rho) ->                              (* [rho |- u' = a[i:=x]] *)
         update_iter c v'
            (fun (_, (b, k, y), tau) ->                        (* [tau |- v' = b[i:=y]] *)  
	       match Partition.is_equal_or_diseq p i k with 
		 | Jst.Three.Yes(ups) ->                       (* Rule (1): *)
		     let sigma = Jst.dep4 rho' rho tau ups in  (* ==> [sigma |- x = y]  *)
		     let e' = Fact.Equal.make (x, y, sigma) in
		       merge c e'
		 | Jst.Three.X ->                              (* Rule (2a): *)
		     D.Set.iter                           
			(fun (j, ups1) ->                      (* [ups1 |- i <> j] *)
			   match Partition.is_diseq p k j with
			     | None -> ()
			     | Some(ups2) ->                   (* [ups2 |- k <> j]. *)
				 let sigma = Jst.dep5 rho' rho tau ups1 ups2 in
				 let e' = Fact.Equal.make (mk_select a j, mk_select b j, sigma) in
				   merge c e')         (* ==> [sigma |- a[j] = bj]. *)
			(Partition.diseqs p i)
	         | Jst.Three.No(ups) ->                        (* Rule (3a) *)
		      let sigma = Jst.dep4 rho' rho tau ups in (* ==> [sigma |- a[k] = y]. *)
		      let e' = Fact.Equal.make (mk_select a k, y, sigma) in
			merge c e'))            
	       
		 
 (** Apply [f (v, (a, i, x), rho)] such that [rho |- u = v = a[i:=x]] *)
and update_iter (p, s) u f =
   Partition.iter_if p
     (fun v ->                      
	try
	  let (b, rho) = apply s v in
	    (match Partition.is_equal p u v with
	       | None -> ()
	       | Some(tau) -> 
		   f (u, Funarr.d_update b, Jst.dep2 tau rho))
	with
	    Not_found -> ())
     u
    
and mk_select a i = Funarr.mk_select Term.is_equal a i

and mk_update a i x = Funarr.mk_update Term.is_equal a i x

(** Apply [f (tau, v, (a, i, x))] to each [tau |- v = a[i:=x]] in [s]. *)
and update_index_iter (p, s) i f =
  A.Dep.iter s
    (fun e ->
       let (v, b, tau) = Fact.Equal.destruct e in
	 try
	   let (a, i', x) = Funarr.d_update b in
	     if Term.eq i i' then
	       f (tau, v, (a, i, x))
	   with
	       Not_found -> ())
    i


(** {6 Splitting} *)

let splits ((p, s) as cfg) =
  failwith "to do"
(*
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
*)
       

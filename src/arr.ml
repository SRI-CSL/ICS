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
      let map _ = failwith "Array map not used"
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

type config = Partition.t * t
    (** A {i configuration} consists of 
      - variable equalities [v]
      - variable disequalities [d]
      - equalities of the form [x = a], [x] not necessarily
      canonical, and [a] a pure array term in canonical form,
      that is, all variables in [a] are canonical and all 
      dependent variables have been ``plugged'' in. *)


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


(** Inverse find: plugs in variables for interpreted terms
  - [C^-1(x) = find(V)(x)]
  - [C^-1(a) = y]  if [y' = a] in [E] and [y = find(V)(y')]
  - [C^-1(a) = a]  otherwise. *)
let uninterp ((p, s) as c) a =
  if Term.is_var a then
    Partition.find p a
  else 
    try
      Jst.Eqtrans.compose (Partition.find p) (A.inv s) a
    with
	Not_found -> Jst.Eqtrans.id a


(** Normalizing the application [op(a1,...,an)] of the array 
  operator [op] w.r.t. to the partition [p]. Justifications
  for these equalities and disequalities are collected in [hyps]. *)
let rec norm c op al =
  let hyps = ref [] in
  let is_equal' = 
    Jst.Three.to_three hyps 
      (is_equal_or_diseq c)
  in
  let b = Funarr.sigma is_equal' op al in
    (b, Jst.dep !hyps)

(** Two canonical terms [a], [b] are disequal if [x = a], [y = b] holds in 
  configuration [c] with [x], [y] variables and [x <> y] a known variable disequality. *)
and is_diseq ((p, s) as c) =
  Jst.Pred2.apply
    (uninterp c)
    (Partition.is_diseq p)

and is_equal_or_diseq (p, _) a b =
  if Term.eq a b then Jst.Three.Yes(Jst.dep0) else
    Partition.is_equal_or_diseq p a b


(** Canonization by plugging in dependent variables. *)
let rec can ((p, _) as c) a = 
  let interp_can = Jst.Eqtrans.compose (interp c) (can c)
  in 
    try
      (match Funarr.d_interp a with
	 | Sym.Create, [b] -> 
	     let (b', tau) = interp_can b in
	     let (a', rho) = norm c Sym.Create [b'] in
	       (a', Jst.dep2 rho tau)
	 | Sym.Update, [b; i; x] ->
	     let (b', tau') = interp_can b
	     and (i', tau'') = interp_can i 
	     and (x', tau''') = interp_can x in
	     let (a', rho) = norm c Sym.Update [b'; i'; x'] in
	       (a', Jst.dep4 rho tau' tau'' tau''')
	 | Sym.Select, [b; j] ->
	     let (b', tau') = interp_can b 
	     and (j', tau'') = interp_can j in
	     let (a', rho) = norm c Sym.Select [b'; j'] in
	       (a', Jst.dep3 rho tau' tau'')
	 | _ -> 
	     Partition.find p a)
    with
	Not_found -> Partition.find p a


(** Fuse an equality [e] of the form [x = b] on rhs [a] of the
  array equalities [u = a] and normalize the resulting [a'] according
  to the variable equalities and disequalities in [p]. Assumes that
  the equality [e] is already merged in [p].*)
let fuse ((p, s) as c) e =
  A.Dep.iter s
    (fun e -> 
       let e' = Fact.Equal.map_rhs (can c) e in
	 if not(e == e') then A.update c e')
    (Fact.Equal.lhs_of e)


(** Propagate a disequality [x <> y] on rhs [a] of the array 
  equalities [u = a']. Assumes the disequalities is already
  dismerged in [p]. *)
let fission ((p, s) as c) d =
  A.Dep.iter s
    (fun e -> 
       let e' = Fact.Equal.map_rhs (can c) e in
	 if not(e == e') then A.update c e')
    (Fact.Diseq.lhs_of d)


(** {6 Updates} *)

let arr = Th.to_string Th.arr

(** Propagating an equality [a = b] between pure array terms [a], [b] by
  first canonizing these terms and then merging the variable equality
  [x = y] with [x = a], [y = b] in [s] ([x], [y] may be fresh terms).
  Merging of such a variable equality [x = y] is done by 
  - merging [x = y] in the variable partitioning [p],
  - fusing [x = y] on rhs of equalities in [s] in order to recanonize rhs of [s] w.r.t
    to the extended partitioning, and
  - forward chaining on the rules that get activated by the new equality [x = y]. *)
let rec merge ((p, s) as c) e =
  assert(Fact.Equal.is_pure Th.arr e);
  let e = Fact.Equal.map (Jst.Eqtrans.compose (A.name c) (can c)) e in 
    assert(Fact.Equal.is_var e);
    let (x, y, rho) = Fact.Equal.destruct e in        (* [rho |- x = y]. *)
      match Partition.is_diseq p x y with 
	| Some(tau) ->                               (* [tau |- x <> y]. *)
	    raise(Jst.Inconsistent(Jst.dep2 rho tau))
	| None ->  
	    Trace.msg arr "Merge" e Fact.Equal.pp;
	    merge_v c e

and merge_v ((p, _) as c) e =
  assert(Fact.Equal.is_var e);
  Partition.merge p e;
  fuse c e;
  close_merge c e

(** Progagating a disequality [a <> b] between pure array terms [a], [b] by
  first canonizing these terms and then merging the variable disequality
  [x <> y] with [x = a], [y = b] in [s] ([x], [y] may be fresh terms). 
  Dismerging of such a variable disequality [x <> y] is done by 
  - dismerging [x <> y] in the variable partitioning [p],
  - fissioning [x <> y] on rhs of equalities in [s] in order to recanonize rhs of [s]
    w.r.t to the extended partitioning, and
  - forward chaining on the rules that get activated by the new equality [x = y]. *)
and dismerge ((p, s) as c) d =  
  let d = Fact.Diseq.map (Jst.Eqtrans.compose (A.name c) (can c)) d in
  let (x, y, rho) = Fact.Diseq.destruct d in        (* [rho |- x <> y]. *)
    match Partition.is_equal p x y with 
      | Some(tau) ->                                (* [tau |- x = y]. *)
	  raise(Jst.Inconsistent(Jst.dep2 rho tau))
      | None ->  
	  Trace.msg arr "Dismerge" d Fact.Diseq.pp;
	  dismerge_v c d

and dismerge_v ((p, s) as c) d =
  assert(Fact.Diseq.is_var d);
  Partition.dismerge p d;
  fission c d;
  close_dismerge c d

(** Deduce is similar to {!Arr.merge} with the notable exception of trace messages.
  It is used to merge variable equalities deduced by forward chaining. *)
and deduce ((p, _) as c) e =
  let e = Fact.Equal.map (Jst.Eqtrans.compose (A.name c) (can c)) e in
  let (x, y, rho) = Fact.Equal.destruct e in
  match Partition.is_equal_or_diseq p x y with
    | Jst.Three.Yes _ ->
	()
    | Jst.Three.No(tau) ->
	raise(Jst.Inconsistent(Jst.dep2 rho tau))
    | Jst.Three.X -> 
	Trace.msg arr "Deduce" e Fact.Equal.pp;
	let e = Fact.Equal.map (name c) e in
	  merge_v c e

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
		       deduce c e1;
		       deduce c e2))

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
			   deduce c e'))
	   (Partition.diseqs p j))

 
and close_merge ((p, s) as c) e =
  let (u', v', rho') = Fact.Equal.destruct e in                (* [rho' |- u' = v'] *)
    update_iter c u'
      (fun (_, (a, i, x), rho) ->                              (* [rho |- u' = a[i:=x]] *)
         update_iter c v'
            (fun (_, (b, k, y), tau) ->                        (* [tau |- v' = b[i:=y]] *)  
	       match is_equal_or_diseq c i k with 
		 | Jst.Three.Yes(ups) ->                       (* Rule (1): *)
		     let sigma = Jst.dep4 rho' rho tau ups in  (* ==> [sigma |- x = y]  *)
		     let e' = Fact.Equal.make (x, y, sigma) in
		       deduce c e'
		 | Jst.Three.X ->                              (* Rule (2a): *)
		     D.Set.iter                           
			(fun (j, ups1) ->                      (* [ups1 |- i <> j] *)
			   match is_diseq c k j with
			     | None -> ()
			     | Some(ups2) ->                   (* [ups2 |- k <> j]. *)
				 let sigma = Jst.dep5 rho' rho tau ups1 ups2 in
				 let e' = Fact.Equal.make (mk_select a j, mk_select b j, sigma) in
				   deduce c e')                (* ==> [sigma |- a[j] = bj]. *)
			(Partition.diseqs p i)
	         | Jst.Three.No(ups) ->                        (* Rule (3a) *)
		      let sigma = Jst.dep4 rho' rho tau ups in (* ==> [sigma |- a[k] = y]. *)
		      let e' = Fact.Equal.make (mk_select a k, y, sigma) in
			deduce c e'))     

	       
		 
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

let splits ((_, s) as cfg) =
  A.fold
    (fun _ (b, _) acc ->
       Term.Set2.union (Funarr.splits b) acc)
    s
    Term.Set2.empty
       

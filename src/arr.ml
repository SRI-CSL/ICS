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
  - if [a[i:=x] = b[j:=y]], then [y = a[i:=x][j]]
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

type config = Partition.t * t
    (** A {i configuration} consists of 
      - variable equalities [v]
      - variable disequalities [d]
      - equalities of the form [x = a], [x] not necessarily
      canonical, and [a] is a flat array term, that is, 
      {!Arr.is_flat}[a] holds. *)

let is_flat = function
  | Term.Var _ -> true
  | Term.App(f, al, _) ->
      Sym.Array.is f && List.for_all Term.is_var al


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
let invfind ((p, s) as c) a =
  if Term.is_var a then
    Partition.find p a
  else 
    try
      Jst.Eqtrans.compose (Partition.find p) (A.inv s) a
    with
	Not_found -> Jst.Eqtrans.id a

(** Find canonical variables for subterms before trying to
  find canonical variable for term itself. *)
let uninterp ((p, _) as c) a =
  try
    (match Funarr.d_interp a with
       | Sym.Create, [b] ->
	   let (a', rho') = 
	     let (b', rho') = invfind c b in
	       if b == b' then Jst.Eqtrans.id a else
		 (Funarr.mk_create b', rho')
	   in
	   let (a'', rho'') = invfind c a' in
	     (a'', Jst.dep2 rho' rho'')
       | Sym.Select, [b; j] -> 
	   let (a', rho') = 
	     let (b', rho') = invfind c b in
	     let (j', tau') = invfind c j in
	       if b == b' && j == j' then Jst.Eqtrans.id a else
		 (Funarr.mk_select Term.is_equal b' j',
		  Jst.dep2 rho' tau')
	   in
	   let (a'', rho'') = invfind c a' in
	     (a'', Jst.dep2 rho' rho'')
       | Sym.Update, [b; j; y] -> 
	   let (a', rho') = 
	     let (b', rho') = invfind c b in
	     let (j', tau') = invfind c j in
	     let (y', sigma') = invfind c y in
	       if b == b' && j == j' && y == y' then Jst.Eqtrans.id a else
		 (Funarr.mk_update Term.is_equal b' j' y', 
		  Jst.dep3 rho' tau' sigma')
	   in
	   let (a'', rho'') = invfind c a' in
	     (a'', Jst.dep2 rho' rho'')
       | _ ->
	   invfind c a)
  with
      Not_found -> invfind c a

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
  assert(Fact.Equal.both_sides is_flat e);
  let e = Fact.Equal.map (A.name c) e in 
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
  A.fuse c [e];
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
  let d = Fact.Diseq.map (A.name c) d in
  let (x, y, rho) = Fact.Diseq.destruct d in        (* [rho |- x <> y]. *)
    match Partition.is_equal p x y with 
      | Some(tau) ->                                (* [tau |- x = y]. *)
	  raise(Jst.Inconsistent(Jst.dep2 rho tau))
      | None ->  
	  Trace.msg arr "Dismerge" d Fact.Diseq.pp;
	  assert(Fact.Diseq.is_var d);
	  Partition.dismerge p d;
	  close_dismerge c d

(** Deduce is similar to {!Arr.merge} with the notable exception of trace messages.
  It is used to merge variable equalities deduced by forward chaining. *)
and deduce ((p, _) as c) e = merge c e

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
          for [v = b[j:=y]] with [u =V v], 
          for [k] such that [k<>i], [k<>j], 
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
    (fun (rho, u, (a, i', x)) ->                     (* [rho |- u = a[i := x]] *)
       assert(Term.eq i i');
       update_index_iter c j 
	 (fun (tau, v, (b, j', y)) ->                (* [tau |- v = b[j:= y]] *)
	    assert(Term.eq j j');  
	    match Partition.is_equal p u v with
	      | None -> ()
	      | Some(ups) ->                         (* [ups |- u = v] *)
		  let sigma = Jst.dep4 rho' rho tau ups in (*[sigma|-a[j]=y,b[i]=x] *)
		    deduce c (Fact.Equal.make (mk_select a j, y, sigma));   
		    deduce c (Fact.Equal.make (mk_select b i, x, sigma))))
    
and close_dismerge_2b ((p, s) as c) d =
  let (i, j, rho') = Fact.Diseq.destruct d in 
    deduce_2b c (i, rho');        (* apply symmetrically *)
    deduce_2b c (j, rho')  

and deduce_2b ((p, s) as c) (i, rho') =
  update_index_iter c i
    (fun (rho, u, (a, i', x)) ->              (* [rho |- u = a[i := x]] *)
       assert(Term.eq i i');
       D.Set.iter
	 (fun (j, tau) ->                     (* [tau |- i <> j] *)
	    D.Set.iter 
	    (fun (k, sigma) ->                (* [sigma |- j <> k] *)
	       update_index_iter c k
	       (fun (theta, v, (b, k', y)) -> (* [theta |- v = b[k := y]] *)
		  assert(Term.eq k k');
		  if not(Term.eq u v) then
		    match Partition.is_equal p u v with
		      | None -> ()
		      | Some(ups) ->          (* [ups |- u = v] *)
			  let a' = mk_select a j 
			  and b' = mk_select b j in
			  let ups' = Jst.dep [rho'; rho; tau; sigma; theta; ups] in
			  let e' = Fact.Equal.make (a', b', ups') in
			    deduce c e'))
	    (Partition.diseqs p j))
         (Partition.diseqs p i))

and close_merge ((p, s) as c) e =
  let (u', v', rho') = Fact.Equal.destruct e in     (* [rho' |- u' = v'] *)
    update_iter c u'
      (fun (_, (a, i, x), rho) ->                   (* [rho |- u' = a[i:=x]] *)
         update_iter c v'
            (fun (_, (b, k, y), tau) ->             (* [tau |- v' = b[k:=y]] *)
	       (D.Set.iter                          (* Rule (2a): *)          
	         (fun (j, ups1) ->                  (* [ups1 |- i <> j] *)
		    match Partition.is_diseq p k j with
		      | None -> ()
		      | Some(ups2) ->               (* [ups2 |- k <> j]. *)
			  let sigma = Jst.dep5 rho' rho tau ups1 ups2 in
			  let e' = Fact.Equal.make (mk_select a j, mk_select b j, sigma) in
			    deduce c e')    
	         (Partition.diseqs p i));
	       match Partition.is_equal_or_diseq p i k with 
		 | Jst.Three.Yes(ups) ->            (* Rule (1): *)
		     let sigma = Jst.dep4 rho' rho tau ups in
		     let e' = Fact.Equal.make (x, y, sigma) in
		       deduce c e' 
		 | Jst.Three.X ->                    (* [theta |- z = u'] *)
		     (let (z, theta) = Partition.find p u' in   (* Rule (1) *)
		      let sigma = Jst.dep4 rho' rho tau theta in
		        deduce c (Fact.Equal.make (y, mk_select z k, sigma));
			deduce c (Fact.Equal.make (x, mk_select z i, sigma)));
		     
	         | Jst.Three.No(ups) ->              (* Rule (3a) *)
		      let sigma = Jst.dep4 rho' rho tau ups in
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
    
and mk_select a i = 
 Funarr.mk_select Term.is_equal a i


(** Apply [f (tau, v, (a, i, x))] to each [tau |- v = a[i:=x]] in [s]. *)
and update_index_iter (_, s) i f =
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
		  let (i, j) = Term.orient (i, j) in
		    raise (Found (i, j))
		with
		    Not_found -> ())
	     u'
	 with
	     Not_found -> ())
       (A.to_list s);
     raise Not_found)
  with
      Found(i, j) -> (i, j)


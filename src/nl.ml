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


exception Found of Term.t * Jst.t

let nl = Th.inj Th.nl

(** A {i flat} term is of the form [x * y] with [x], [y] variables. *)
let is_flat a =
  try
    let (x, y) = Pprod.d_mult a in
      Term.is_var x && Term.is_var y
  with
      Not_found -> false


(** A {i pure} term is of the form [x] or [x1 * (x2 * (x3 * ... * xn))]
  with [xi] variables. *)
let rec is_pure a =
  try
    let (x, b) = Pprod.d_mult a in
      Term.is_var x && is_pure b
  with
      Not_found -> true


(** Deducing constraints from equalities [x = a], with [a] a
  binary multiplication term  [a1 * a2], as a side effect on updates in 
  the equality set. *)
module Deduce: (Eqs.EXT with type t = unit) =
struct
  type t = unit
  let empty = ()
  let pp _ _ = ()
  let do_at_restrict _ _ = ()
  let do_at_add (p, (), _) (x, a, rho) =   (* [rho |- x = a] *)
    let (y, tau) = Partition.find p x in   (* [tau |- x = y] *)
      try
	let (a1, a2) = Pprod.d_mult a in
	  (match Term.Var.is_slack a1, Term.Var.is_slack a2 with
	    | true, true ->
		if not(Term.Var.is_slack y) then
		  let nn = Fact.Nonneg.make (y, Jst.dep2 rho tau) in
		    Fact.Nonnegs.push nl nn
	    | true, false -> 
		if Term.Var.is_slack y then 
		  let nn = Fact.Nonneg.make (a2, Jst.dep2 rho tau) in
		    Fact.Nonnegs.push nl nn
	    | false, true -> 
		if Term.Var.is_slack y then 
		  let nn = Fact.Nonneg.make (a1, Jst.dep2 rho tau) in
		    Fact.Nonnegs.push nl nn
	    | false, false -> 
		())
      with
	  Not_found -> ()

end

module Eqs = Eqs.Make(
  struct
    let th = Th.nl
    let nickname = Th.to_string Th.nl
    let map = Pprod.map
    let is_infeasible _ = false
  end)
  (Deduce)

type t = Eqs.t

type config = Partition.t * t

let eq = Eqs.eq
let empty = Eqs.empty
let is_empty = Eqs.is_empty
let pp = Eqs.pp
let apply = Eqs.apply
let find = Eqs.find
let inv = Eqs.inv
let dep = Eqs.dep

let iter f = 
  Eqs.iter 
    (fun x (a, rho) ->
       try
	 let (y, z) = Pprod.d_mult a in
	   f x (y, z) rho
       with
	   Not_found -> ())


let copy = Eqs.copy

let fold = Eqs.fold

let is_dependent = Eqs.is_dependent
let is_independent = Eqs.is_independent


(** {i Lookup} of [a] in configuration [(v, s)]
  with [v] a set of variable equalities and [s] a flat array context.
  - [C(x) = y]            if [x =v y] and [y] canonical,
  - [C(x) = C(z) * C(u)]  if [y = z*u] in [s] with [x =V y], 
  - [C(a) = a]   otherwise. *)
let rec lookup ((p, s) as c) a =
  if Term.is_app a then
     Jst.Eqtrans.id a 
  else
    try
      let (b', rho) = Partition.choose p (Eqs.apply s) a in
      let (z, u) = Pprod.d_mult b' in
      let (z', tau) = lookup c z
      and (u', sigma) = lookup c u in
	if z == z' && u == u' then 
	  (b', rho)
	else
	  (Pprod.mk_mult z' u', Jst.dep3 rho tau sigma)
    with
	Not_found -> Partition.find p a


(** {i Replace} dependent variables in a term [a]. 
  - [C[x] = C(x)]
  - [C[a1*...*an] = C[a1]*...*C[an]]. *)
and replace c a =
  try
    let (a1, a2) = Pprod.d_mult a in
    let (b1, rho) = replace c a1
    and (b2, tau) = replace c a2 in
      if a1 == b1 && a2 == b2 then Jst.Eqtrans.id a else 
	(Pprod.mk_mult b1 b2, Jst.dep2 rho tau)
  with
      Not_found -> lookup c a


(** {i Inverse lookup} of [a] in configuration [(v, s)]
  - [C^-1(x) = y]   if [x =v y] and [y] canonical
  - [C^-1(a) = y]   if [x = a] in [s] and [y =v x] with [y] canonical
  - [C^-1(x*b) = C^-1(x*C^-1(b))]
  - [C^-1(a) = a]   otherwise. *)
let rec abstract ((p, s) as c) a =
  if Term.is_var a then Partition.find p a else 
    try
      Jst.Eqtrans.compose 
	(Partition.find p) 
	(Eqs.inv s) 
	a
    with
	Not_found ->
	  (try
	    let (x, b) = Pprod.d_mult a in
	    let (b', rho) = abstract c b in
	      if b == b' then Jst.Eqtrans.id a else 
		let a' = Pprod.mk_mult x b' in
		let (a'', tau) = abstract c a' in
		  (a'', Jst.dep2 rho tau)
	  with
	      Not_found -> Jst.Eqtrans.id a)



(** {i Canonization} with respect to a configuration [c]
  of the form [(v, s)] with [v] variable equalities and [s]
  an array context.
  - [C[| . |] = (C^-1(.) o C[.])] *)
and can c =
  Jst.Eqtrans.compose
    (abstract c)
    (replace c)


(** Introduce a new name [u] for the flat term [x*v] if necessary 
  and extend the context with a new equality [u = x*v]. The result
  is always a variable. *)
and name2 ((p, s) as c) (x, v) =
  let xv = Pprod.mk_mult x v in
    try
      Jst.Eqtrans.compose 
	(Partition.find p) 
	(Eqs.inv s) 
	xv
    with
	Not_found ->
	  let u = mk_rename () in
	  let rho = Jst.dep0 in
	  let e = Fact.Equal.make (u, xv, rho) in
	    update c e;
	    (u, rho)

and mk_rename () =
  let v = Name.of_string (Th.to_string Th.nl) in
    Term.Var.mk_rename v None Var.Cnstrnt.Unconstrained
     

(** Flatten a pure array term [a] and introduce variables as
  necessary for naming subterms. The result is a variable [z]
  equal to [a] in the extended context. *)
and name ((p, s) as c) a =
  assert(is_pure a);
  try
    let (x, b) = Pprod.d_mult a in
      assert(Term.is_var x);
      let (v, rho) = name c b in
	assert(Term.is_var v);
	let (z, tau) = name2 c (x, v) in
	  (z, Jst.dep2 rho tau)
  with
      Not_found -> Partition.find p a
	
    

(** Install a new equality [e] of the form [x = z*u] 
  in [s] and forward chain on the following two antecedents.
  - [x' = z*u], [y = x*v], [x =v x'] ==> [y = z*u*v]
  As a result, the state is always confluent, and the
  {i lookup} in canonization {!Nl.can} is a {i don't care}
  nondeterminism. *)
and update ((p, s) as c) e =
  assert(is_flat (Fact.Equal.rhs_of e));
  Eqs.update c e;
  chain1 c e;
  chain2 c e

and chain1 ((p, s) as c) e =
  let (x', b, rho) = Fact.Equal.destruct e in
    try
      let (z, u) = Pprod.d_mult b in
	Eqs.Dep.iter s
	  (fun e' ->
	     let (y, b', tau) = Fact.Equal.destruct e in
	       try
		 let (x, v) = Pprod.d_mult b' in
		   match Partition.is_equal p x x' with
		     | None -> ()
		     | Some(sigma) ->
			 let (uv, theta) = name2 c (u, v) in
			 let rho'' = Jst.dep4 rho tau sigma theta in
			 let zuv = Pprod.mk_mult z uv in
			 let e'' = Fact.Equal.make(y, zuv, rho'') in
			   update c e''
	       with
		   Not_found -> ())
	  x'
    with
	Not_found -> ()

and chain2 ((p, s) as c) e =
  let (y, b, rho) = Fact.Equal.destruct e in
    try
      let (x, v) = Pprod.d_mult b in
	Partition.iter_if p 
	  (fun x' ->
	     let (b', tau) = Eqs.apply s x in
	     let (z, u) = Pprod.d_mult b' in
	       match Partition.is_equal p x x' with
		 | None -> ()
		 | Some(sigma) -> 
		     let (uv, theta) = name2 c (u, v) in
		     let ups = Jst.dep4 rho tau sigma theta in
		     let e' = Fact.Equal.make (y, Pprod.mk_mult z uv, ups) in
		       update c e')
	  x
    with
	Not_found -> ()



(** Process an equality [e] over pure terms
  - by flattening of the left-hand and right-hand side of [e] using {!Nl.name},
  - by merging the resulting variable equality using {!Nl.merge}. *)
let rec process ((p, _) as c) e =
  Trace.msg "nl" "Process" e Fact.Equal.pp;
  assert(Fact.Equal.both_sides is_pure e);
  let e = Fact.Equal.map (name c) e in
    assert(Fact.Equal.is_var e);
    Partition.merge p e;      
    merge c e


(** Processing a variable equality [e] of the form [x = y] by
  - {i fusing}, that is substituting [y] for [x] on right-hand sides
  - {i minimizing}, that is, if [x' = a], [y' = a] with [x' =/= y'], 
    [x =v x'], [y =v y'], then remove [x' = a] or [y' = a]. Minimization
    is not strictly required but is added here for optimization purposes. *)
and merge ((p, s) as c) e =
  Trace.msg "nl" "Merge" e Fact.Equal.pp;
  assert(Fact.Equal.is_var e);
  fuse c e;
  minimize c e

and fuse ((p, s) as c) e =
  assert(Fact.Equal.is_var e);
  let (x, y, rho) = Fact.Equal.destruct e in
  let norm a = 
    (Pprod.apply (x, y) a, rho)
  in  
    Eqs.Dep.iter s
      (fun e ->
	 update c (Fact.Equal.map_rhs norm e))
      x

and minimize ((p, s) as c) e =
  assert(Fact.Equal.is_var e);
  let (x, y, _) = Fact.Equal.destruct e in    
  Partition.iter_if p
    (fun x' ->
       let (a, _) = Eqs.apply s x' in
	 Partition.iter_if p
	   (fun y' ->
	      if not(Term.eq x' y') then
		let (b, _) = Eqs.apply s y' in
		  if Term.eq a b then
		    if Term.(<<<) x' y' then
		      Eqs.restrict c x'
		    else 
		      Eqs.restrict c y')
	   y)
    x
		    

(** Propagate an equality [x = a], where [a] is a linear
  arithmetic term by instantiating rhs with this equality
  and and all solved forms [y = b] with [y] in [a]. *)
let rec propagate (p, la, nl) e =  
  Trace.msg "nl" "Propagate" e Fact.Equal.pp;    
  let isolate y = Fact.Equal.equiv (Arith.isolate y) in
  let (_, a, _) = Fact.Equal.destruct e in
    propagate1 (p, la, nl) e;
    Arith.iter
      (fun y -> 
	 let e' = isolate y e in
	   propagate1 (p, la, nl) e')
      a

and propagate1 (p, la, nl) e =
  let (x, a, _) = Fact.Equal.destruct e in
    assert(Term.is_var x && Arith.is_interp a);
    Eqs.Dep.iter nl           
      (fun e' ->                             (* [rho |- y = b] *)
	 let (y, b, rho) = Fact.Equal.destruct e' in
	 let (b', tau) = apply_subst e b in  (* [tau |- b = b'] *)
	   if b == b' then () else 
	     let (b'', sigma) = linearize (p, la, nl) b' in
	     let (y'', theta) = Partition.find p y in
	     let rho'' = Jst.dep4 rho tau sigma theta in
	     let e'' = Fact.Equal.make (y'', b'', rho'') in
	       if Term.is_var b'' then
		 begin
		   Partition.merge p e'';
		   merge (p, nl) e''
		 end 
	       else if is_pure b'' then
		 process (p, nl) e''
	       else if Arith.is_interp b'' then
		 La.process_equal (p, la) e''
	       else 
		 ())
      x


(** [apply e a], for [e] is of the form [x = b], applies the
  substition [x := b] to [a]  *)
and apply_subst e = 
  let (x, a, rho) = Fact.Equal.destruct e in
  let lookup y = if Term.eq x y then (a, rho) else (y, Jst.dep0) in
    Jst.Eqtrans.replace Pprod.map lookup


(** Replace linear arithmetic subterms by variables.
  The result is either a linear arithmetic term or a variable. *) 
and linearize (p, la, nl) a = 
  let (q, b) = Arith.poly_of a in  (* [a = q + b] *)
  let hyps = ref Jst.dep0 in
  let b' = 
    Arith.Monomials.fold 
      Arith.Monomials.is_true
      (fun (r, c) acc ->
	 let (c', rho) = name (p, nl) c in
	   if not(c == c') then hyps := Jst.dep2 rho !hyps;
	   let rc' = Arith.mk_multq r c' in
	     Arith.mk_add acc rc')
      a (Arith.mk_num q)
  in
    (b', !hyps)


exception Found of Term.Equal.t * Term.Equal.t

(** {i Implied disjunctions.} From equality [z' = x*y] in [s] deduce the disjunction 
  - [x = 0] or [y = 1] if  [z' = x] modulo [p]
  - [y = 0] or [x = 1] if [z' = y] modulo [p]. *)
let disjunction (p, s) =
  try
    (iter 
      (fun z' (x, y) _ ->
	 let (z, _) = Partition.find p z' in
	   match Partition.is_equal p z x with
	     | Some(_) -> 
		 let d1 = Term.Equal.make (x, Arith.mk_zero())
		 and d2 = Term.Equal.make (y, Arith.mk_one()) in
		   raise(Found(d1, d2))
	     | None -> 
		 (match Partition.is_equal p z y with
		    | Some(_) -> 
			let d1 = Term.Equal.make (y, Arith.mk_zero())
			and d2 = Term.Equal.make (x, Arith.mk_one()) in
			  raise(Found(d1, d2))
		    | None -> 
			()))
      s);
    raise Not_found;
  with
      Found(d1, d2) -> (d1, d2)
    
	 







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

open Mpa

exception Found of Term.t * Jst.t

let nl = Th.inj Th.nl

(** A {i flat} term is of the form [x * y]. *)
let is_flat a =
  try
    let (x, y) = Pprod.d_mult a in
      Term.is_var x && Term.is_var y
  with
      Not_found -> false

(** A {i pure} term is of the form [x] or [x1 * (x2 * (x3 * ... * xn))] *)
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

let eq = Eqs.eq
let empty = Eqs.empty
let is_empty = Eqs.is_empty
let pp = Eqs.pp
let apply = Eqs.apply
let find = Eqs.find
let inv = Eqs.inv
let dep = Eqs.dep

let fold = Eqs.fold

let is_dependent = Eqs.is_dependent
let is_independent = Eqs.is_independent

let copy = Eqs.copy

let name ((p, _) as cfg) = 
  Jst.Eqtrans.compose (Partition.find p) (Eqs.name cfg) 


(** Introduce a pure term [a] to the term universe. The
  result is a canonical variable, which is equal to [a] in a 
  extended configuration obtained by destructive updates. *)
let rec pure2var ((p, _) as cfg) a =
  assert(is_pure a);
  try
    let (x, b) = Pprod.d_mult a in
    let (y, rho) = pure2var cfg b in
    let a' = Pprod.mk_mult x y in
    let (z, tau) = name cfg a' in
      (z, Jst.dep2 rho tau)
  with
      Not_found -> Partition.find p a


(** Process an equality [e] over pure terms
  - by flattening of the left-hand and right-hand side of [e],
  - by merging the resulting variable equality in partition [p], and
  - by fusing the variable equality on right-hand side. *)
let rec process ((p, _) as cfg) e =
  Trace.msg "nl" "Process" e Fact.Equal.pp;
  assert(Fact.Equal.both_sides is_pure e);
  let e = Fact.Equal.map (pure2var cfg) e in
    assert(Fact.Equal.is_var e);
    Partition.merge p e;      
    Eqs.fuse cfg [e]
	    
let interp (p, s) a = 
  if Term.is_var a then
    Partition.choose p (apply s) a
  else 
    Jst.Eqtrans.id a

let uninterp (p, s) a =
  try
    Jst.Eqtrans.compose (Partition.find p) (inv s) a
  with
      Not_found -> Partition.find p a

let rec uninterp_with_chaining (p, s) a =
  Trace.msg "nl" "Uninterp" a Term.pp;
  close (p, s) a;
  uninterp (p, s) a
  
(** Locally apply forward chaining for a term [a] of
  the form [x1 * ... * xn].  
  - For all pairs [x], [y] in [a],  
  - let [a'] be [a / (x * y)],
  - for all [z] such that [z = x * y] in [s], and
  - for all [z' = z] in [p], [z' =/= z], and [b' <> x * y] 
    such that [z' = b'] in [s],
  process the equality [a = z * a' = z' * a' = b' * a']. *)
and close cfg a =
  Pprod.iter 
    (fun x n -> 
       assert(n >= 1);
       Pprod.iter
          (fun y m -> 
	     assert(m >= 1); 
	     close1 cfg a (x, y);
	     if n > 1 then close1 cfg a (x, x);
	     if m > 1 then close1 cfg a (y, y))
          a)
    a

and close1 ((p, s) as cfg) a (x, y) =
  try
    let (z, rho) = choose s (x, y) in            (* [rho |- z = x * y] *)
    let a' =                                     (* [|- a = a' * x * y] *)
      if Term.eq x y then 
	Pprod.divide a (x, 2)
      else 
	Pprod.divide (Pprod.divide a (x, 1)) (y, 1)
    in
      Partition.iter_if p
	(fun z' -> 
	   if not(Term.eq z z') then
	     match Partition.is_equal p z z' with (* [tau |- z = z'] *)
	       | Some(tau) ->                     (* [sigma |- z' = b'] *)
		   let (b', sigma) = Eqs.apply s z' in
		   let theta = Jst.dep3 rho tau sigma in
		   let e' = Fact.Equal.make (a, Pprod.mk_mult a' b', theta) in
		     process cfg e'
	       | None -> 
		   ()) (* should not happen *)
	z
  with
      Not_found -> ()


(** Choose a [z] with [z = x * y] in [s]. *)
and choose s (x, y) =
  let (x, y) = Term.orient (x, y) in   (* now [x << y] *)
    try
      Eqs.Dep.iter s
	(fun e ->
	   let (z, a, rho) = Fact.Equal.destruct e in
	     try
	       let (x', y') = Pprod.d_mult a in
		 if Term.eq x x' && Term.eq y y' then 
		   raise(Found(z, rho))
	     with
		 Not_found -> ())
	x;
      raise Not_found;
    with
	Found(z, rho) -> (z, rho)	



(** Processing a variable equality. *)
and merge cfg e =
  Trace.msg "nl" "Merge" e Fact.Equal.pp;
  assert(Fact.Equal.is_var e);
  Eqs.fuse cfg [e]


(** Propagate an equality [x = a], where [a] is a linear
  arithmetic term by instantiating rhs with this equality.
  This may introduce linear arithmetic subterms, which need
  to be renamed. *)
let rec propagate (p, la, nl) e =     
  Trace.msg "nl" "Propagate" e Fact.Equal.pp; 
  let (x, a, _) = Fact.Equal.destruct e in
    if is_independent nl x then
      propagate1 (p, la, nl) e;
    let isolate y = Fact.Equal.equiv (Arith.isolate y) in
      Arith.iter
	(fun y -> 
	   let e' = isolate y e in
	   let (x', _, _) = Fact.Equal.destruct e' in
	     if is_independent nl x' then
	       propagate1 (p, la, nl) e')
	a

and propagate1 (p, la, nl) e =                
  let (x, a, rho) = Fact.Equal.destruct e in        (* [rho |- x = a] *)
    assert(Term.is_var x && Term.is_pure Th.la a);
    let instantiate e =
      let (y, b, tau) = Fact.Equal.destruct e in    (* [tau |- y = b] *)
      let (c, sigma) = apply_subst e b in           (* [sigma |- b=c], with [c==b[x:=a]]*)
	if not(b == c) then                     
	  let (d, upsilon) = abstract (p, la, nl) c in (* [upsilon |- c = d] *)
	  let omega' = Jst.dep2 tau sigma in
	  let e' = Fact.Equal.make (y, c, omega') in (* [omega' |- y = c] *)
	    if Pprod.is_interp d then
	      Eqs.update (p, nl) e'
	    else 
	      begin
		Eqs.restrict (p, nl) y;
		Fact.Eqs.push (Th.inj Th.nl) e';
		let omega'' = Jst.dep2 omega' upsilon in
		let e'' = Fact.Equal.make (y, d, omega'') in
		  Fact.Eqs.push (Th.inj Th.nl) e''
	      end 
  in
    Eqs.Dep.iter nl instantiate x


(** [apply e a], for [e] is of the form [x = b], applies the
  substition [x := b] to [a]  *)
and apply_subst e = 
  let (x, a, rho) = Fact.Equal.destruct e in
  let lookup y = if Term.eq x y then (a, rho) else (y, Jst.dep0) in
    Jst.Eqtrans.replace Pprod.map lookup


(** Replace linear arithmetic subterms by variables.
  The result is either a nonarithmetic term or a power product. *) 
and abstract (p, la, nl) a = 
  let hyps = ref Jst.dep0 in
  let name_la a =
    let (x, rho) = La.name (p, la) a in
      hyps := Jst.dep2 rho !hyps;
      x
  and name_nl a =
    let (x, rho) = Eqs.name (p, nl) a in
      hyps := Jst.dep2 rho !hyps;
      x
  in  
  let b = 
    try
      (match Arith.d_interp a with
	 | Sym.Num _, [] -> name_la a
	 | Sym.Multq _, [b] -> 
	     let b' = name_nl b in
	       if b == b' then a else 
		 name_la a
	 | Sym.Add, bl -> 
	     let bl' = Term.mapl name_nl bl in
	       if bl == bl' then a else
		 name_la (Arith.mk_addl bl')
	 | _ ->
	     a)
    with
	Not_found -> a
  in
    (b, !hyps)

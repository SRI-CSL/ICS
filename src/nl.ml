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

let nl = Th.inj Th.nl

(** Deducing constraints from equalities [x = a], with [a] a power 
  product, as a side effect on updates in the equality set. *)
module Deduce: (Eqs.EXT with type t = unit) =
struct
  type t = unit
  let empty = ()
  let pp _ _ = ()
  let do_at_restrict _ _ = ()
  let do_at_add (p, ()) (x, a, rho) =      (* [rho |- x = a] *)
    let (y, tau) = Partition.find p x in   (* [tau |- x = y] *)
      if Pprod.is_one a then
	let e = Fact.Equal.make (y, Arith.mk_one(), Jst.dep2 rho tau) in
	  Fact.Eqs.push nl e
      else if Term.Var.is_slack y then
	let (b, c) = Pprod.split a in
	  assert(Pprod.is_nonneg b);
	  if Pprod.is_one c then () else     (* deduce [c >= 0]. *)
	    let nn =  Fact.Nonneg.make (c, Jst.dep2 rho tau) in
	      Fact.Nonnegs.push nl nn
      else                                   
	if Pprod.is_nonneg a then            (* deduce [y >= 0]. *)
	  let nn = Fact.Nonneg.make (y, Jst.dep2 rho tau) in
	    Fact.Nonnegs.push nl nn
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

  
(** Search for largest match on rhs. For example, if [a] is
 of the form [x * y] and there is an equality [u = x^2 * y],
 then [inv s a] returns [u * x] if there is no larger
 rhs which matches [a]. *)
let rec inv s a =
  let rec inv_plus (b, rho) =         (* [rho |- a = b] *)
    try 
      let (c, sigma) = inv1 s b in    (* [sigma |- y = z] *)
	assert(not(Term.eq b c));
	let tau = Jst.dep2 rho sigma in
	  inv_plus (c, tau)
    with 
	Not_found -> (b, rho)
  in
    inv_plus (inv1 s a)

and inv1 s pp =
  try
    let x = Pprod.choose pp in  (* arbitrary variable. *)
    let lookup = ref None in
      Eqs.Dep.iter s
	(fun e ->
	   let (u, qq, rho) = Fact.Equal.destruct e in  (* [rho |- u = qq] *)
	     match Pprod.div (qq, pp) with
	       | None -> ()
	       | Some(mm) ->                            (* [qq * mm = pp] *)
		   (match !lookup with                  (* thus: [rho |- u * mm = pp]. *)
		      | Some(_, _, mm') when Pprod.cmp mm mm' < 0 -> ()
		      | _ -> lookup := Some(rho, u, mm)))
	x;
      (match !lookup with
	| Some(rho, u, mm) -> 
	    let pp' = Pprod.mk_mult u mm in
	      if Term.eq pp pp' then raise Not_found else (pp', rho)
	| None ->
	    raise Not_found)
  with
      Not_found ->  (* [pp] a representation of [1]. *)
	Eqs.inv s pp

let is_dependent = Eqs.is_dependent
let is_independent = Eqs.is_independent

let copy = Eqs.copy

let name ((p, _) as cfg) = 
  Jst.Eqtrans.compose (Partition.find p) (Eqs.name cfg) 

(** Canonization by interpreting dependent variables
  followed by a reduction using Groebner-like cuts. *)
let rec can cfg =
  Jst.Eqtrans.compose
    (Jst.Eqtrans.totalize (uninterp cfg))
    (Jst.Eqtrans.totalize (interp cfg))

and interp (p, s) a = 
  if Term.is_var a then
    Partition.choose p (apply s) a
  else 
    Jst.Eqtrans.id a

and uninterp (p, s) a =
  try
    Jst.Eqtrans.compose (Partition.find p) (inv s) a
  with
      Not_found -> Partition.find p a


(** [replace s a] substitutes dependent variables [x]
  in [a] with their right hand side [b] if [x = b] in [s].
  The result is canonized using {!Pprod.map}. *)
let replace cfg =
  Jst.Eqtrans.replace Pprod.map (interp cfg)


(** Processing an equality. *)
let merge cfg e = 
  assert(Fact.Equal.is_pure Th.nl e);
  Trace.msg "nl" "Merge" e Fact.Equal.pp;
  let e = Fact.Equal.map (replace cfg) e in
  let e = Fact.Equal.map_lhs (Eqs.name cfg) e in  (* not necessarily canonical name. *)
    Eqs.compose cfg [e]


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

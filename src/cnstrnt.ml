
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
 * 
 * Author: Harald Ruess
 i*)

(*i*)
open Mpa         
(*i*)

(*s Set of rational numbers. *)

module Diseqs = Set.Make(
  struct
    type t = Q.t
    let compare = Q.compare
  end)


(*s A constraint consists an interval and a set of rational numbers. *)

type t = Interval.t * Diseqs.t

let destruct c = c

let eq (i, qs) (j, ps) = 
  Interval.eq i j && Diseqs.equal qs ps


(*s Empty constraint. *)

let mk_empty = (Interval.mk_empty, Diseqs.empty)

let is_empty (i,_) = Interval.is_empty i

let is_full (i,qs) = 
  Interval.is_full i && Diseqs.is_empty qs


(*s Constructing a constraint from components *)

let of_interval i = (i, Diseqs.empty)

let make (i, qs) =
  let eq q e =     (* test if [q] falls on endpoint [e]. *)
    Endpoint.is_nonstrict e && 
    Endpoint.is_q e && 
    Q.equal (Endpoint.q_of e) q
  in
  Diseqs.fold 
    (fun q (j,ps) ->
       if Interval.mem q j then
	 let (d,l,h) = Interval.destructure j in
	 if eq q l then  
	   (Interval.make (d, Endpoint.strict (Endpoint.q_of l), h), ps)
	 else if eq q h then
	   (Interval.make (d, l, Endpoint.strict (Endpoint.q_of h)), ps)
	 else 
	   (j, Diseqs.add q ps)
       else 
	 (j, Diseqs.add q ps))
    qs
    (i, Diseqs.empty)
	   

(*s Constraint for the real number line, the integers, and the
 natural numbers. *)

let mk_real = of_interval Interval.mk_real

let mk_int = of_interval Interval.mk_int
let mk_nat = of_interval (Interval.make (Dom.Int, Endpoint.nonstrict Q.zero, Endpoint.posinf))

(*s Constructing singleton constraints. *)

let mk_singleton q = of_interval (Interval.mk_singleton q)

let d_singleton (i,_) = 
  Interval.d_singleton i

let mk_zero = mk_singleton Mpa.Q.zero
let mk_one = mk_singleton Mpa.Q.one


(*s Disequality constraint. *)

let mk_diseq q = (Interval.mk_real, Diseqs.singleton q)

(*s Checks wether [c] is a subconstraint of [d]. *)

let sub (i,qs) (j,ps) =
  Interval.sub i j &&
  Diseqs.subset ps qs
  

(*s Intersection of two constraints *)

let inter (i,qs) (j,ps) =
  (Interval.inter i j, Diseqs.union qs ps)

(*s Comparison. *)

let cmp c d =
  let (i,qs) = destruct c 
  and (j,ps) = destruct d in
  match Interval.cmp i j with 
    | Binrel.Disjoint -> Binrel.Disjoint
    | Binrel.Overlap -> Binrel.Overlap
    | Binrel.Same when Diseqs.equal qs ps -> Binrel.Same
    | Binrel.Same when Diseqs.subset qs ps -> Binrel.Super
    | Binrel.Same when Diseqs.subset ps qs -> Binrel.Sub
    | Binrel.Same -> Binrel.Overlap
    | Binrel.Singleton(q) when Diseqs.mem q qs ||  Diseqs.mem q ps -> Binrel.Disjoint
    | Binrel.Singleton(q) -> Binrel.Singleton(q)
    | Binrel.Sub when Diseqs.subset ps qs -> Binrel.Sub
    | Binrel.Sub -> Binrel.Overlap
    | Binrel.Super when Diseqs.subset qs ps -> Binrel.Super
    | Binrel.Super -> Binrel.Overlap
 

(*s Checks wether two given lists are disjoint*)

let disjoint (i,_) (j,_) =
  Interval.disjoint i j

(*s Printing constraints. *)

let pp fmt c =
  let (i,qs) = destruct c in
  Format.fprintf fmt "@[";
  Interval.pp fmt i;
  if not(Diseqs.is_empty qs) then
    begin
      Format.fprintf fmt " with ";
      Pretty.set Mpa.Q.pp fmt (Diseqs.elements qs)
    end


(*s Additional constructors. *)

let of_endpoints (dom, lo, hi) = 
  of_interval (Interval.make (dom, lo, hi))

let mk_oo dom u v = of_endpoints (dom, Endpoint.strict u, Endpoint.strict v)
let mk_oc dom u v = of_endpoints (dom, Endpoint.strict u, Endpoint.nonstrict v)
let mk_co dom u v = of_endpoints (dom, Endpoint.nonstrict u, Endpoint.strict v)
let mk_cc dom u v = of_endpoints (dom, Endpoint.nonstrict u, Endpoint.nonstrict v)

let mk_lt dom u = of_endpoints (dom, Endpoint.neginf, Endpoint.strict u)
let mk_le dom u = of_endpoints (dom, Endpoint.neginf, Endpoint.nonstrict u)
let mk_gt dom u = of_endpoints (dom, Endpoint.strict u, Endpoint.posinf)
let mk_ge dom u = of_endpoints (dom, Endpoint.nonstrict u, Endpoint.posinf)

let mk_neg dom = mk_lt dom Q.zero
let mk_pos dom = mk_gt dom Q.zero
let mk_nonneg dom = mk_ge dom Q.zero
let mk_nonpos dom = mk_le dom Q.zero


(*s Abstract interpretation. *)

let add (i,_) (j,_) = 
  of_interval (Interval.add i j)

let rec addl = function
  | [] -> mk_singleton Q.zero
  | [c] -> c
  | c :: cl -> add c (addl cl)

let multq q (i,qs) =
  let j' = Interval.multq q i in
  let qs' = Diseqs.fold (fun p -> Diseqs.add (Mpa.Q.mult q p)) qs Diseqs.empty in
  make (j',qs')

let mult (i,_) (j,_) =
  of_interval (Interval.mult i j)


let rec multl = function
  | [] -> mk_singleton Q.one
  | [c] -> c
  | c :: cl -> mult c (multl cl)

let expt n (i,_) =
  of_interval (Interval.expt n i)

let div (i,_) (j,_) =
  of_interval (Interval.div i j)

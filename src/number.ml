
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

type t = Dom.t * Intervals.t

let destruct c = c

let eq (d1,i1) (d2,i2) =
  Dom.eq d1 d2 && Intervals.eq i1 i2


(*s Constructors. *)

let mk_bot = (Dom.Real, Intervals.empty)

let is_bot (_,i) = Intervals.is_bot i

let is_top (d,i) =
  Dom.eq d Dom.Real && Intervals.is_top i

let make (d,i) = (d,i)

(*s Various derived, arithmetic constructors. *)

let mk_real = (Dom.Real, Intervals.full)
let mk_int  = (Dom.Int, Intervals.full)
let mk_nat = (Dom.Int, Intervals.nonneg Dom.Int)

let mk_singleton q = (Dom.of_q q, Intervals.singleton q)

let mk_zero = mk_singleton Mpa.Q.zero
let mk_one = mk_singleton Mpa.Q.one


let mk_diseq q = (Dom.Real, Intervals.diseq Dom.Real q)


(*s Recognizers. *)

let is_real (d,_) = (d = Dom.Real)
 
let is_int (d,_) = (d = Dom.Int)

(*s Checks wether [c] is a subconstraint of [d]. *)

let sub (d,i) (e,j) =
  Dom.sub d e && Intervals.sub i j


(*s Intersection *)

let inter (d,i) (e,j) = 
  try
    let dom = Dom.inter d e in
    make (dom, Intervals.inter dom i j)
  with
      Dom.Empty -> mk_bot

(*s Union. *)

let union (d,i) (e,j) =
  let dom = Dom.union d e in
  make (dom, Intervals.union dom i j)

(*s Complement w.r.t. to the reals. *)

let compl (d,i) =
  let dom = Dom.compl d in
  make (dom, Intervals.compl dom i)

(*s Comparison. *)

let cmp (d,is) (e,js) =
  match Intervals.cmp is js with
    | Binrel.Same -> Dom.cmp d e
    | Binrel.Disjoint -> Binrel.Disjoint
    | Binrel.Overlap -> Binrel.Overlap
    | Binrel.Sub -> 
	(match Dom.cmp d e with
	   | (Binrel.Same | Binrel.Sub) -> Binrel.Sub
	   | Binrel.Super -> Binrel.Overlap
	   | Binrel.Disjoint -> Binrel.Disjoint
	   | Binrel.Overlap -> Binrel.Overlap)
    | Binrel.Super ->
	(match Dom.cmp d e with
	   | (Binrel.Same | Binrel.Super) -> Binrel.Super
	   | Binrel.Sub -> Binrel.Overlap
	   | Binrel.Disjoint -> Binrel.Disjoint
	   | Binrel.Overlap -> Binrel.Overlap)
 
let d_singleton (_,i) =
  if Intervals.is_singleton i then
    Some(Intervals.d_singleton i)
  else 
    None

(*s Checks wether two given lists are disjoint*)

let rec is_disjoint (d,i) (e,j) =
  Dom.disjoint d e || Intervals.is_disjoint i j

(*s Printing constraints. *)

let pp fmt (d,is) =
  Format.fprintf fmt "@[";
  if d = Dom.Int then
    Format.fprintf fmt "Int";
  if d = Dom.Int && List.length (Intervals.to_list is) = 1 then
    Format.fprintf fmt "(";
  Intervals.pp fmt is;
  if d = Dom.Int && List.length (Intervals.to_list is) = 1 then
    Format.fprintf fmt ")";
  Format.fprintf fmt "@]"


type typ = t  (* avoid type-check error below *)

module Set = Set.Make(
  struct
    type t = typ
    let compare = Pervasives.compare
  end)

module Map = Map.Make(
  struct
    type t = typ
    let compare = Pervasives.compare
  end)


(*s Additional constructors. *)

let inj dom l u =
  (dom, Intervals.inj dom (Interval.make dom l u))

let mk_oo dom u v = inj dom (Interval.strict u) (Interval.strict v)
let mk_oc dom u v = inj dom (Interval.strict u) (Interval.nonstrict v)
let mk_co dom u v = inj dom (Interval.nonstrict u) (Interval.strict v)
let mk_cc dom u v = inj dom (Interval.nonstrict u) (Interval.nonstrict v)

let mk_lt dom u = inj dom Interval.neginf (Interval.strict u)
let mk_le dom u = inj dom Interval.neginf (Interval.nonstrict u)
let mk_gt dom u = inj dom (Interval.strict u) Interval.posinf
let mk_ge dom u = inj dom (Interval.nonstrict u) Interval.posinf

let mk_neg dom = mk_lt dom Q.zero
let mk_pos dom = mk_gt dom Q.zero
let mk_nonneg dom = mk_ge dom Q.zero
let mk_nonpos dom = mk_le dom Q.zero


(*s Abstract interpretation. *)

let add (d,i) (e,j) =
  let dom = Dom.union d e in
  make (dom, Intervals.add  dom i j)

let rec addl = function
  | [] -> mk_singleton Q.zero
  | [c] -> c
  | c :: cl -> add c (addl cl)

let multq q (d,i) =
  let dom = Dom.union (Dom.of_q q) d in
  make (dom, Intervals.multq dom q i)

let mult (d,i) (e,j) =
  let dom = Dom.union d e in
  make (dom, Intervals.mult dom i j)


let rec multl = function
  | [] -> mk_singleton Q.one
  | [c] -> c
  | c :: cl -> mult c (multl cl)

let expt n (d,i) =
  make (d, Intervals.expt d n i)

let div (_,i) (_,j) =
  let d = Dom.Real in
  make (d, Intervals.div d i j)

(*
 * The contents of this file are subject to the ICS(TM) Community Research
 * License Version 2.0 (the ``License''); you may not use this file except in
 * compliance with the License. You may obtain a copy of the License at
 * http://www.icansolve.com/license.html.  Software distributed under the
 * License is distributed on an ``AS IS'' basis, WITHOUT WARRANTY OF ANY
 * KIND, either express or implied. See the License for the specific language
 * governing rights and limitations under the License.  The Licensed Software
 * is Copyright (c) SRI International 2003, 2004.  All rights reserved.
 * ``ICS'' is a trademark of SRI International, a California nonprofit public
 * benefit corporation.
 *)

open Mpa
open Sign


type t = {
  dom : Dom.t;
  lo : (bool * Q.t) option;     (* A rational [q] or negative infinity. *)
  hi : (Q.t * bool) option      (* A rational [q] or positive infinity. *)
}

and low = (bool * Q.t) 

(** {6 Accessors} *)

let dom i = i.dom
let lo i = i.lo
let hi i = i.hi

let destructure i = (i.dom, i.lo, i.hi)


(** {6 Pretty-printing} *)

let pp fmt i =
  Dom.pp fmt i.dom;
  if not(i.lo = None && i.hi = None) then
    begin
      (match i.lo with
	 | None -> Pretty.string fmt "(-inf"
	 | Some(alpha, q) -> Pretty.string fmt (if alpha then "[" else "("); Q.pp fmt q);
      Pretty.string fmt ",";
      (match i.hi with
	 | None -> Pretty.string fmt "inf)"
	 | Some(q, beta) ->  Q.pp fmt q; Pretty.string fmt (if beta then "]" else ")"))
    end

(** {6 Equality and Subsumption} *)

let eq i j =
  Dom.eq i.dom j.dom &&
  (match i.lo,  j.lo with
    | None, None -> true
    | Some(alpha, q), Some(beta, p) -> alpha = beta && Q.equal q p 
    | _ -> false) &&
  (match i.hi, j.hi with
     | None, None -> true
     | Some(q, alpha), Some(p, beta) -> alpha = beta && Q.equal q p
     | _ -> false)


(** {6 Constructors} *)

let mk_empty = { dom = Dom.Real; lo = Some(false, Q.zero); hi = Some(Q.zero, false) }

let mk_real = { dom = Dom.Real; lo = None; hi = None }

let mk_int = { dom = Dom.Int; lo = None; hi = None }

let mk_nat = { dom = Dom.Int; lo = Some(true, Q.zero); hi = None }

let mk_pos =  { dom = Dom.Real; lo = Some(false, Q.zero); hi = None }

let mk_nonneg = { dom = Dom.Real; lo = Some(true, Q.zero); hi = None }

let mk_neg = { dom = Dom.Real; lo = None; hi = Some(Q.zero, false) }

let mk_nonpos = { dom = Dom.Real; lo = None; hi = Some(Q.zero, true) }


let mk_dom = function
  | Dom.Real -> mk_real
  | Dom.Int -> mk_int

let mk_singleton q = { dom = Dom.of_q q; lo = Some(true, q); hi = Some(q, true) }

let mk_zero = mk_singleton Q.zero
let mk_one = mk_singleton Q.one
 
let rec make (d, lo, hi) =
  let lower (alpha, q) =    (* normalized lower bound *)
    match d with
      | Dom.Int when not(alpha && Q.is_integer q) -> 
	  (true, Q.of_z(Q.floor(Q.add q Q.one)))
      | _ -> 
	  (alpha, q)
  and upper (p, beta) =   (* normalized upper bound *)
    match d with
      | Dom.Int when not(beta && Q.is_integer p) -> 
	  (Q.of_z(Q.ceil(Q.sub p Q.one)), true)
      | _ -> 
	  (p, beta)
  in
    match lo, hi with
      | None, None -> 
	  mk_dom d
      | None, Some(p, beta) -> 
	  let (p', beta') = upper (p, beta) in
	  { dom = d; lo = None; hi = Some(p', beta') }
      | Some(alpha, q), None ->
	  let (alpha', q') = lower (alpha, q) in
	  { dom = d; lo = Some(alpha', q'); hi = None }
      | Some(alpha, q), Some(p, beta) ->
	  let (alpha', q') = lower (alpha, q)
	  and (p', beta') = upper (p, beta) in
	    if alpha' && beta' && Q.gt q' p' then
	      mk_empty
	    else if not(alpha' && beta') && Q.ge q' p' then
	      mk_empty
	    else 
	      { dom = d; lo = Some(alpha', q'); hi = Some(p', beta') }

(** {6 Recognizers} *)

let is_empty i = (i == mk_empty)

let is_full i = (i == mk_real)

let d_singleton i =
  match i.lo, i.hi with
    | Some(true, q), Some(p, true) 
	when Q.equal q p -> Some(q)
    | _ ->
	None

type status =
  | Empty
  | Full
  | Singleton of Mpa.Q.t
  | Other

let status i =
  if is_empty i then Empty
  else if is_full i then Full
  else match d_singleton i with
    | Some(q) -> Singleton(q)
    | None -> Other


(** {6 Membership test} *)

let mem q i =
  Dom.mem q i.dom &&
  (match i.lo with
     | None -> true
     | Some(alpha, l) -> 
	 if alpha then Q.ge q l else Q.gt q l) &&
  (match i.hi with
     | None -> true
     | Some(h, beta) -> 
	 if beta then Q.le q h else Q.lt q h)

let is_sub i j =
  Dom.sub i.dom j.dom &&
  (match i.lo, j.lo with  (* [i.lo >= j.lo] *)
     | None, None -> true
     | None, Some _ -> false
     | Some _, None -> true
     | Some(true, q), Some(false, p) -> Q.gt q p
     | Some(_, q), Some(_, p) -> Q.ge q p) &&
  (match i.hi, j.hi with  (* [i.hi >= j.hi] *)
     | None, None -> true
     | None, Some _ -> false
     | Some _, None -> true
     | Some(q, false), Some(p, true) -> Q.lt q p
     | Some(q, _), Some(p, _) -> Q.le q p)

let is_sub i j =
  Trace.call "foo" "is_sub" (i, j) (Pretty.pair pp pp);
  let res = is_sub i j in
    Trace.exit "foo" "is_sub" res Pretty.bool;
    res

let is_disjoint i j = 
  (match i.hi, j.lo with  (* [i.hi < j.lo] *)
     | Some(q, true), Some(true, p) -> Q.lt q p
     | Some(q, _), Some(_, p) -> Q.le q p
     | _ -> false)
  || 
  (match j.hi, i.lo with  (* [j.hi > i.lo] *)
     | Some(q, true), Some(true, p) -> Q.lt q p
     | Some(q, _), Some(_, p) -> Q.le q p
     | _ -> false)

let is_disjoint i j =
  Trace.call "foo" "is_disjoint" (i, j) (Pretty.pair pp pp);
  let res = is_disjoint i j in
    Trace.exit "foo" "is_disjoint" res Pretty.bool;
    res


(* {6 Intersection} *)

let inter i j =
  let low_eq = function            
    | (None, None) -> true
    | (Some(alpha, p), Some(beta, q)) -> alpha = beta && Q.equal q p
    | _ -> false
  and high_eq = function
    | (None, None) -> true
    | (Some(p, gamma), Some(q, delta)) -> gamma = delta && Q.equal q p
    | _ -> false
  in
  let d = Dom.inter i.dom j.dom 
  and l = 
    match i.lo, j.lo with   (* Maximum of lower bounds *)
      | None, None -> None
      | None, Some _ -> j.lo
      | Some _, None -> i.lo
      | Some(alpha, q), Some(beta, p) ->
	  let res = Q.compare q p in
	    if res = 0 then
	      Some(alpha && beta, q)
	    else if res < 0 then
	      j.lo
	    else (* [q > p] *)
	      i.lo
  and h = 
    match i.hi, j.hi with   (* Minimum of upper bounds *)
      | None, None -> None
      | None, Some _ -> j.hi
      | Some _, None -> i.hi
      | Some(q, gamma), Some(p, delta) -> 
	  let res = Q.compare q p in
	    if res = 0 then
	      Some(q, gamma && delta)
	    else if res < 0 then
	      i.hi
	    else (* [q > p] *)
	      j.hi
  in
    make (d, l, h)

let inter i j =
  Trace.call "i" "Inter" (i, j)  (Pretty.pair pp pp);
  let k = inter i j in
    Trace.exit "i" "Inter" k pp;
    k

let complement i =
  if i.dom <> Dom.Real then
    raise (Invalid_argument "interval not complementable")
  else
    match i.lo, i.hi with
      | None, None -> mk_empty
      | None, Some(p, beta) -> 
	  { dom = Dom.Real; lo = Some(not beta, p); hi = None }
      | Some(alpha, q), None -> 
	  { dom = Dom.Real; lo = None; hi = Some(q, not alpha) }
      | _ ->
	  raise (Invalid_argument "interval not complementable")

let is_complementable i = 
  match i.lo, i.hi with
    | Some _, Some _ -> false
    | _ -> true

	
(** {6 Interval Arithmetic} *)

(** Adding two intervals *)
let rec add i j =
  let dom = Dom.union i.dom j.dom
  and lo = 
    match i.lo, j.lo with
      | None, _ -> None       
      | _, None -> None         
      | Some(alpha, q), Some(beta, p) -> Some(alpha && beta, Q.add q p)
  and hi = 
    match i.hi, j.hi with
      | None, _ -> None       
      | _, None -> None         
      | Some(q, alpha), Some(p, beta) -> Some(Q.add q p, alpha && beta)
  in
    make (dom, lo, hi)

let rec addl = function
  | [] -> mk_zero
  | [i] -> i
  | [i; j] -> add i j
  | i :: il -> add i (addl il)


(** Subtraction of intervals *)
let rec sub i j =
  let dom = Dom.union i.dom j.dom
  and lo =
    match i.lo, j.hi with    (* [i.lo - j.hi] *)
      | None, _ -> None    
      | _, None -> None           
      | Some(alpha, q), Some(p, beta) -> Some(alpha && beta, Q.sub q p)
  and hi =
    match i.hi, j.lo with    (* [i.hi - j.lo] *)
      | None, _ -> None 
      | _, None -> None              
      | Some(q, gamma), Some(delta, p) -> Some(Q.sub q p, gamma && delta)
  in 
    make (dom, lo, hi)

      
(** Shifting by a rational. *)
let addq q i =
  add (mk_singleton q) i


(** Multiply by a rational. *)
let multq q i =
  if Q.is_zero q then
    mk_zero
  else if Q.is_one q then
    i
  else 
    let dom = match i.dom with
      | Dom.Real -> Dom.Real
      | Dom.Int -> if Q.is_integer q then Dom.Int else Dom.Real
    and (lo, hi) = 
      match i.lo, i.hi with    (* [min(q * i.lo, q * i.hi),max(q * i.lo, q * i.hi)] *)
	| None, None  ->
	    (None, None)
	| None, Some(p, beta) -> (* [min(q * -inf, q * p),max(q * -inf, q * p)] *)
            if Q.is_pos q then 
	      (None, Some(Q.mult q p, beta))
	    else 
	      (Some(beta, Q.mult q p), None)
	| Some(alpha, l), None ->  (* [min(q * l, q * inf), max(q * l, q * inf)] *)
	    if Q.is_pos q then
	      (Some(alpha, Q.mult q l), None)
	    else 
	      (None, Some(Q.mult q l, alpha))
	| Some(alpha, l), Some(h, beta) ->  (* [min(q * l, q * h), max(q * l, q * h)] *)
	    let ql = Q.mult q l and qh = Q.mult q h in
	    let (alpha', l') = if Q.le ql qh then (alpha, ql) else (beta, qh)
            and (beta', h') = if Q.le ql qh then (beta, qh) else (alpha, ql) in 
	      (Some(alpha', l'), Some(h', beta'))
	    in
      make (dom, lo, hi)

let multq q i = 
  Trace.call "foo" "Multq" (q, i) (Pretty.pair Mpa.Q.pp pp);
  let res = multq q i in
    Trace.exit "foo" "Multq" res pp;
    res


(** Multiplying intervals. *)



let mult i j =
  if is_empty i || is_empty j then
    mk_empty
  else
    match d_singleton i, d_singleton j with
      | Some(q), Some(p) ->
	  mk_singleton (Q.mult q p)
      | Some(q), None -> 
	  multq q j
      | None, Some(p) -> 
	  multq p i
      | None, None -> 
	  let d = Dom.union i.dom j.dom in
	  let (lo, hi) =     (*[min(i.lo*j.lo,i.lo*j.hi,i.hi*j.lo,i.hi*j.hi), max(...)]*)
	    match i.lo, i.hi, j.lo, j.hi with
	      | None, None, _, _ ->  (* [(-inf,inf) * j = (-inf, inf)] *)
		  (None, None)
	      | _, _, None, None ->  (* [i * (-inf,inf) = (-inf, inf)] *)
		  (None, None)
	      | None, Some(q, alpha), None, Some(p, beta) -> (* [(-inf,q}*(-inf,p} = {q*p,inf) if q,p<=0 *)
		  if Q.le q Q.zero && Q.le p Q.zero then
		    (Some(alpha && beta, Q.mult q p), None)
		  else 
		    (None, None)
	      | Some(alpha1,q1), None, Some(alpha2,q2), None ->
		  if Q.ge q1 Q.zero && Q.ge q2 Q.zero then (* {q1,inf)*{q2,inf)={q1*q2,inf) if q1,q2>=0 *)
		    (Some(alpha1 && alpha2, Q.mult q1 q2), None)
		  else 
		    (None, None)
	      | None, Some(p, beta), Some(alpha, q), None -> (* [(-inf,p}*{q,inf) = (-inf,q*p} if p<=0<=q*)
		  if Q.le p Q.zero && Q.le Q.zero p then
		    (None, Some(Q.mult q p, alpha && beta))
		  else 
		    (None, None)
	      | Some(alpha, q), None, None, Some(p, beta) -> (* [{q,inf)*(-inf,p} =(-inf,q*p} if p<=0<=q*)
		  if Q.le p Q.zero && Q.le Q.zero p then
		    (None, Some(Q.mult q p, alpha && beta))
		  else 
		    (None, None)
	      | Some(alpha1, q1), Some(p1, beta1), Some(alpha2, q2), Some(p2, beta2) ->
		  (None, None)
	      | _ ->  (* to do *)
		  (None, None)
	  in
	    make (d, lo, hi)
  

let rec multl = function
  | [] -> mk_one
  | [i] -> i
  | [i; j] -> mult i j
  | i :: il -> mult i (multl il)
    

(** Exponentiation of an interval. *)
let expt n i =
  let rec loop = function
    | 0 -> mk_singleton Q.one
    | n -> mult i (loop (n - 1))
  in
  let j = loop n in
    if n mod 2 = 0 then inter j mk_nonneg else j

 
(** Inverse of an Interval *)
let inv i =
  match i.lo, i.hi with
    | Some(alpha, l), Some(h, beta)
	when Q.is_pos l || Q.is_neg h ->
	let lo = Some(beta, Q.inv h) and hi = Some(Q.inv l, alpha) in
	  make (Dom.Real, lo, hi)
    | _ -> 
	mk_real


(** Division of interverals. *)
let div i j =
  mult i (inv j)



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
open Sign
open Endpoint
(*i*)

(*s General real intervals are represented by their endpoints [a] and [b]
  and two bits of information [alpha] and [beta], with [alpha] (resp. [beta])
  specifying whether [a] (resp. [b]) belongs to the interval. More formally.
  [(a,b,true,true)] denotes the closed interval [{x in R | u <= x <= v}],
  [(a,b,true,false)] denotes the right-open interval [{x in R | u <= x < v}],
  [(a,b,false,true)] denotes the left-open interval [{x in R | u < x <= v}],
  and [(a,b,false,false)] denotes the open interval [{x in R | u < x < v}]. *)

type t = tnode

and tnode = {
  dom : Dom.t;
  lo : Endpoint.t;
  hi : Endpoint.t
}

(*s Accessors. *)

let dom i = i.dom
let lo i = i.lo
let hi i = i.hi

let destructure i = (i.dom,i.lo,i.hi)


(*s Equality. *)

let eq i j =
  Dom.eq i.dom j.dom &&
  Endpoint.eq i.lo j.lo &&
  Endpoint.eq i.hi j.hi


(*s Constructing intervals. *)

let mk_empty =
  let z = Endpoint.strict Mpa.Q.zero in
  {dom = Dom.Real; lo = z; hi = z}

let rec make (d,lo,hi) =
  match d with
    | Dom.Int -> makeint lo hi
    | Dom.Real -> makereal lo hi
    | Dom.Nonint -> makenonint lo hi

and makenonint lo hi =
  let (a,l) = Endpoint.destruct lo
  and (b,k) = Endpoint.destruct hi 
  in
  let ((a',_) as lo') = 
    match Extq.destruct a with
      | Extq.Inject u when l && Q.is_integer u -> (a, false)
      | _ -> lo
  and ((b',_) as hi')  = 
    match Extq.destruct b with
      | Extq.Inject u when k && Q.is_integer u -> (b, false)
      | _ -> hi
  in
  if Extq.lt b' a' then
    mk_empty
  else
    {dom = Dom.Nonint; lo = lo'; hi = hi'}

    
and makereal lo hi = 
  let (a,l) = Endpoint.destruct lo 
  and (b,k) = Endpoint.destruct hi 
  in
  let is_empty = 
    match Extq.destruct a, Extq.destruct b with
      | Extq.Inject u, Extq.Inject v ->
	  (match Q.cmp u v with
	     | Q.Greater -> true
	     | Q.Equal -> not(l && k)
	     | Q.Less -> false)
      | Extq.Neginf, Extq.Neginf -> true
      | Extq.Posinf, _ -> true
      | _ -> false
  in
  if is_empty then
    mk_empty 
  else
    {dom = Dom.Real; lo = lo; hi = hi}
    
and makeint lo hi = 
  let (a,l) = Endpoint.destruct lo 
  and (b,k) = Endpoint.destruct hi 
  in
  let ((a',_) as lo') = 
    match Extq.destruct a with
      | Extq.Inject u when not(l && Q.is_integer u) ->
	  (Extq.of_q(Q.of_z(Q.floor(Q.add u Q.one))), true)
      | _ ->
	  lo
  and ((b',_) as hi')  = 
    match Extq.destruct b with
      | Extq.Inject u when not(k && Q.is_integer u) ->
	  (Extq.of_q(Q.of_z(Q.ceil(Q.sub u Q.one))), true)
      | _ ->
	  hi
  in
  if Extq.lt b' a' then
    mk_empty
  else
    {dom = Dom.Int; lo = lo'; hi = hi'}



(*s Derived constructors. *)

let mk_real = make (Dom.Real, Endpoint.neginf, Endpoint.posinf)
let mk_int = make (Dom.Int, Endpoint.neginf, Endpoint.posinf)
let mk_nonint = make (Dom.Nonint, Endpoint.neginf, Endpoint.posinf)


let mk_singleton q = 
  let bnd = Endpoint.nonstrict q in
  make (Dom.of_q q, bnd, bnd)

let mk_zero = mk_singleton Q.zero

(* Tests for special intervals. *)

let is_empty i = eq i mk_empty

let is_full i = eq i mk_real

let d_singleton i =
  let (a,l) = Endpoint.destruct i.lo
  and (b,k) = Endpoint.destruct i.hi 
  in
  if l && k && Extq.eq a b then
    Extq.to_q a
  else 
    None

let is_zero i = (eq i mk_zero)

(*s Return rational endpoints. *)

let rational_endpoints i =
  let (a,_) = Endpoint.destruct i.lo 
  and (b,_) = Endpoint.destruct i.hi
  in
  match Extq.to_q a, Extq.to_q b with
    | Some(q), Some(p) -> Some(q,p)
    | _ -> None

(*s Test if [q] is in the interval [i]. *)

let mem q i =
  Dom.mem q i.dom &&
  let (a,alpha) = Endpoint.destruct i.lo in
  let (b,beta) = Endpoint.destruct i.hi in
  match Extq.destruct a with
    | Extq.Inject u ->
	(match Extq.destruct b with
	   | Extq.Inject v ->
	       (match alpha, beta with
		  | true, true -> Q.le u q && Q.le q v
		  | true, false -> Q.le u q && Q.lt q v
		  | false, true ->  Q.lt u q && Q.le q v
		  | false, false -> Q.lt u q && Q.lt q v)
	   | Extq.Posinf ->
	       (match alpha with
		  | true -> Q.le u q
		  | false -> Q.lt u q)
	   | Extq.Neginf ->
	       false)
    | Extq.Neginf ->
	(match Extq.destruct b with
	   | Extq.Inject v ->
	       (match beta with
		  | true -> Q.le q v
		  | false -> Q.lt q v)
	   | Extq.Posinf ->
	       true
	   | Extq.Neginf ->
	       false)
    | Extq.Posinf ->
	false

(*s Printing an interval. *)

let pp fmt i =
  let (a,alpha) = Endpoint.destruct i.lo in
  let (b,beta) = Endpoint.destruct i.hi in
    if Extq.destruct a = Extq.Neginf &&
       Extq.destruct b = Extq.Posinf && 
       Endpoint.is_strict i.lo &&
       Endpoint.is_strict i.hi 
    then
      Dom.pp fmt i.dom
    else
      begin
	Format.fprintf fmt "";
	Dom.pp fmt i.dom;
	Format.fprintf fmt "%s" (if alpha && Extq.is_q a then "[" else "(");
	Extq.pp fmt a;
	Format.fprintf fmt "..";
	Extq.pp fmt b;
	Format.fprintf fmt "%s" (if beta && Extq.is_q b then "]" else ")")
      end

(* Union and intersection of intervals. *)

let mu a c alpha gamma =
  match Extq.cmp a c with
    | Q.Less -> alpha
    | Q.Equal -> alpha && gamma
    | Q.Greater -> gamma

let nu a c alpha gamma =
  match Extq.cmp a c with
    | Q.Less -> alpha
    | Q.Equal -> alpha || gamma
    | Q.Greater -> gamma

let inter i j =
  let (a,alpha) = Endpoint.destruct i.lo
  and (b,beta) = Endpoint.destruct i.hi
  and (c,gamma) = Endpoint.destruct j.lo 
  and (d,delta) = Endpoint.destruct j.hi
  in  
  try
    let d' = Dom.inter i.dom j.dom in
    let lo' = Endpoint.make (Extq.max a c, mu a c gamma alpha) in
    let hi' = Endpoint.make (Extq.min b d, mu b d beta delta) in
    make (d',lo',hi')
  with
      Dom.Empty -> mk_empty
     

let union i j =
  let (a,alpha) = Endpoint.destruct i.lo
  and (b,beta) = Endpoint.destruct i.hi
  and (c,gamma) = Endpoint.destruct j.lo 
  and (d,delta) = Endpoint.destruct j.hi
  in
  let d' = Dom.union i.dom j.dom
  and lo' = Endpoint.make (Extq.min a c, mu a c alpha gamma)
  and hi' = Endpoint.make (Extq.max b d, mu b d delta beta)
  in
  make (d', lo', hi')


(*s The implementation of interval arithmetic 
 operations follows the tables given in ``Interval Arithmetic: 
 from Principles to Implementation'' by Hickey, Ju, and Emden in the 
 Journal of ACM, 2002. *)


(*s Addition and Subtraction. *)

let rec add i j =
  let (a,alpha) = Endpoint.destruct i.lo
  and (b,beta) = Endpoint.destruct i.hi
  and (c,gamma) = Endpoint.destruct j.lo 
  and (d,delta) = Endpoint.destruct j.hi
  in
  let d' = add_dom i.dom j.dom
  and lo' = Endpoint.make (Extq.add a c, alpha && gamma)
  and hi' = Endpoint.make (Extq.add b d, beta && delta)
  in
  make (d',lo',hi')

and add_dom d1 d2 =
  match d1, d2 with
    | Dom.Real, _ -> Dom.Real
    | _, Dom.Real -> Dom.Real
    | Dom.Int, Dom.Int -> Dom.Int
    | Dom.Int, Dom.Nonint -> Dom.Nonint
    | Dom.Nonint, Dom.Nonint -> Dom.Nonint
    | Dom.Nonint, Dom.Int -> Dom.Nonint


(*s Classification of intervals according to the signs of endpoints. *)
  
type classification = 
  | M                 (*s [(a,b)] in [M] iff [a < 0 < b]. *)
  | Z                 (*s [(0,0)] is only interval in [Z]. *)
  | P0                (*s [(0,b)] in [P0] iff [b > 0]. *)
  | P1                (*s [(a,b)] in [P1] iff [0 < a <= b]. *)
  | N0                (*s [(a,0)] in [N0] iff [a < 0]. *)
  | N1                (*s [(a,b)] in [N1] iff [a <= b < 0]. *)


let classify i =
  let (a,alpha) = Endpoint.destruct i.lo
  and (b,beta) = Endpoint.destruct i.hi
  in
  match Extq.sign a with
    | Zero ->
	(match Extq.sign b with
	   | Zero -> Z
	   | Pos -> P0
	   | Neg -> assert false)  (* both_sides_div_pos_lt2 triggers this *)
    | Pos ->
	P1
    | Neg ->
	(match Extq.sign b with
	   | Zero -> N0
	   | Neg -> N1
	   | Pos -> M)

(*s Multiplication of general real intervals. *)

let mult i j =
  let dom = Dom.union i.dom j.dom
  and (a,alpha) = Endpoint.destruct i.lo
  and (b,beta) = Endpoint.destruct i.hi
  and (c,gamma) = Endpoint.destruct j.lo 
  and (d,delta) = Endpoint.destruct j.hi
  in
  let kind a c l k =
    (l && k) || 
    (l && (Extq.is_zero a)) || 
    (k && (Extq.is_zero c))
  in
  let ( * ) = Extq.mult in
  let make lo hi = make (dom,lo,hi) in
  match classify i, classify j with
    | (P0 | P1), (P0 | P1) ->
	make (a * c, kind a c alpha gamma) (b * d, kind b d beta delta)
    | (P0 | P1), M ->
	make (b * c, kind b c beta gamma) (b * d, kind b d beta delta)
    | (P0 | P1), (N0 | N1) ->
	make (b * c, kind b c beta gamma) (a * d, kind a d alpha delta)
    | M, (P0 | P1) ->
	make (a * d, kind a d alpha delta) (b * d, kind b d beta delta)
    | M, M ->
	union
	  (make (a * d, kind a d alpha delta) (b * d, kind b d beta delta))
	  (make (b * c, kind b c beta gamma) (a * c, kind a c alpha gamma))
    | M, (N0 | N1) ->
	make (b * c, kind b c beta gamma) (a * c, kind a c alpha gamma)
    | (N0 | N1), (P0 | P1) ->
	make (a * d, kind a d alpha delta) (b * c, kind b c beta gamma)
    | (N0 | N1), M ->
	make (a * d, kind a d alpha delta) (a * c, kind a c alpha gamma)
    | (N0 | N1), (N0 | N1) ->
	make (b * d, kind b d beta delta) (a * c, kind a c alpha gamma)
    | Z, (P0 | P1 | M | N0 | N1) ->
	mk_zero
    | (P0 | P1 | M | N0 | N1 | Z), Z ->
	mk_zero

let multq q i = 
  if Endpoint.eq i.lo Endpoint.neginf && Endpoint.eq i.hi Endpoint.posinf then 
    i
  else 
    mult (mk_singleton q) i

let subtract i j =
  add i (multq (Mpa.Q.minus Q.one) j)
	
let div i j =
  let dom = Dom.Real
  and (a,alpha) = Endpoint.destruct i.lo
  and (b,beta) = Endpoint.destruct i.hi
  and (c,gamma) = Endpoint.destruct j.lo 
  and (d,delta) = Endpoint.destruct j.hi
  in
  let make lo hi = make (Dom.Real, lo, hi) in
  let kind a c alpha gamma =
    (alpha && gamma) || (alpha && (Extq.is_zero a)) || (gamma && (Extq.is_zero c))
  in
  let ( / ) = Extq.div in
  match classify i, classify j with
    | (P0 | P1), P1 ->
	make (a / d, kind a d alpha delta) (b / c, kind b c beta gamma)
    | (P0 | P1), P0 ->
	make (a / d, kind a d alpha delta) posinf
    | (P0 | P1), M ->
	union
	  (make neginf (a / c, kind a c alpha gamma))
	  (make (a / d, kind a d alpha delta) posinf)
    | (P0 | P1), N0 ->
	make neginf (a / c, kind a c alpha gamma)
    | (P0 | P1), N1 ->
	make (b / d, kind b d beta delta) (a / c, kind a c alpha gamma)
    | M, P1 ->
	make (a / c, kind a c alpha gamma) (b / c, kind b c beta gamma)
    | M, (P0 | M | N0) ->
	make  neginf posinf
    | M, N1 ->
	make (b / d, kind b d beta delta) (a / d, kind a d alpha delta)
    | (N0 | N1), P1 ->
	make (a / c, kind a c alpha gamma) (b / d, kind b d beta delta)
    | (N0 | N1), P0 ->
	make neginf (b / d, kind b d beta delta)
    | (N0 | N1), M ->
	union
	  (make neginf (b / d, kind b d beta delta))
	  (make (b / c, kind b c beta gamma) posinf)
    | (N0 | N1), N0 ->
	make (b / c, kind b c beta gamma) posinf
    | (N0 | N1), N1 ->
	make (b / c, kind b c beta gamma) (a / d, kind a d alpha delta)
    | Z, (P0 | P1 | M | N0 | N1) ->
	mk_zero
    | _, Z ->
	mk_empty

let nonneg = make (Dom.Real, Endpoint.nonstrict Q.zero, Endpoint.posinf)

let expt n l =
  let rec loop = function
    | 0 -> mk_singleton Q.one
    | n -> mult l (loop (n - 1))
  in
  let c = loop n in
  if n mod 2 = 0 then
    inter c nonneg
  else 
    c

(*s Comparing two intervals. *)

let sub i j =
  let lo_ge (a,alpha) (c, gamma) =
    match Extq.cmp a c with
      | Mpa.Q.Less -> false  
      | Mpa.Q.Greater -> true
      | Mpa.Q.Equal -> not alpha || gamma
  in
  let hi_le (b,beta) (d, delta) =
    match Extq.cmp b d with
      | Mpa.Q.Less -> true
      | Mpa.Q.Greater -> false
      | Mpa.Q.Equal -> not beta || delta
  in
  Dom.sub i.dom j.dom &&
  lo_ge (Endpoint.destruct i.lo) (Endpoint.destruct j.lo) &&
  hi_le (Endpoint.destruct i.hi) (Endpoint.destruct j.hi)
 
  
let disjoint i j =
  let (b,beta) = Endpoint.destruct i.hi 
  and (c,gamma) = Endpoint.destruct j.lo in
  match Extq.cmp b c with
    | Mpa.Q.Less -> true
    | Mpa.Q.Equal -> not (beta && gamma)
    | Mpa.Q.Greater ->
	let (d,delta) = Endpoint.destruct j.hi 
	and (a,alpha) = Endpoint.destruct i.lo in
	(match Extq.cmp d a with
	   | Mpa.Q.Less -> true
	   | Mpa.Q.Equal -> not (delta && alpha)
	   | Mpa.Q.Greater -> false)
     
let rec cmp i j =
  if eq i j then 
    Binrel.Same
  else if sub i j then
    Binrel.Sub
  else if sub j i then
    Binrel.Super
  else 
    let (b,beta) = Endpoint.destruct i.hi 
    and (c,gamma) = Endpoint.destruct j.lo in
    match Extq.cmp b c with
      | Q.Less -> Binrel.Disjoint
      | Q.Equal when beta && gamma -> Binrel.Singleton (to_q b)
      | Q.Equal -> Binrel.Disjoint
      | Q.Greater ->
	  let (d,delta) = Endpoint.destruct j.hi
	  and (a,alpha) = Endpoint.destruct i.lo in
	  (match Extq.cmp d a with
	     | Q.Less -> Binrel.Disjoint
	     | Q.Equal when delta && alpha -> Binrel.Singleton (to_q a)
	     | Q.Equal -> Binrel.Disjoint
	     | Q.Greater -> Binrel.Overlap (inter i j))

and to_q x = 
  assert(Extq.is_q x);
  match Extq.to_q x with
    | Some(q) -> q
    | None -> assert false




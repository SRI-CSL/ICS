
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
open Hashcons
open Mpa
open Sign
open Allen
(*i*)

(*s General real intervals are represented by their endpoints [a] and [b]
  and two bits of information [alpha] and [beta], with [alpha] (resp. [beta])
  specifying whether [a] (resp. [b]) belongs to the interval. More formally.
  [(a,b,true,true)] denotes the closed interval [{x in R | u <= x <= v}],
  [(a,b,true,false)] denotes the right-open interval [{x in R | u <= x < v}],
  [(a,b,false,true)] denotes the left-open interval [{x in R | u < x <= v}],
  and [(a,b,false,false)] denotes the open interval [{x in R | u < x < v}]. *)

type t = tnode hashed

and tnode = {
  lo : endpoint;
  hi : endpoint
}

and endpoint = Extq.t * bool

let value e = fst e
let kind e = snd e

(*s Extreme endpoints *)

let neginf = (Extq.neginf, false)
let posinf = (Extq.posinf, false)

let strict u = (Extq.of_q u, false)
let nonstrict u = (Extq.of_q u, true)

(*s Accessors. *)

let lo i = i.node.lo
let hi i = i.node.hi

let destructure {node={lo=l;hi=h}} = (l,h)

(*s Two intervals have the same denotation iff they are physically equal. *)

let eq = (===)

(*s Hashconsing *)

module HashInt = Hashcons.Make( 
  struct 
    type t = tnode

    let equal {lo=li;hi=hi} {lo=lj;hi=hj} =
      let endpoint_eq (a,alpha) (b,beta) =
	match Extq.destruct a, Extq.destruct b with
	  | Extq.Inject u, Extq.Inject v -> alpha = beta && Q.equal u v
	  | Extq.Posinf, Extq.Posinf -> true
	  | Extq.Neginf, Extq.Neginf -> true
	  | _ -> false
      in
      endpoint_eq li lj && endpoint_eq hi hj

    let hash = Hashtbl.hash
  end)

let ht = HashInt.create 17

let _ = Tools.add_at_reset (fun () -> HashInt.clear ht)


(*s Hashconsed constructor. *)

let empty =
  let z = (Extq.zero, false) in
  HashInt.hashcons ht {lo = z; hi = z}

let rec make d =
  match d with
    | Dom.Int -> makeint
    | Dom.Real -> makereal
    | Dom.Nonint -> makenonint
    
and makereal ((a,l) as lo) ((b,k) as hi) = 
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
    empty 
  else
    HashInt.hashcons ht {lo = lo; hi = hi}

and makenonint ((a,l) as lo) ((b,k) as hi) =
  let lo' = if l && Extq.is_int a then (a,false) else lo in
  let hi' = if k && Extq.is_int b then (b,false) else hi in
  makereal lo' hi'
    
and makeint ((a,l) as lo) ((b,k) as hi) = 
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
    empty
  else
    HashInt.hashcons ht {lo = lo'; hi = hi'}


(*s Derived constructors. *)

let full = make Dom.Real neginf posinf

let singleton u = 
  let pnt = (Extq.of_q u, true) in
  make (Dom.of_q u) pnt pnt

let zero = singleton Q.zero

(* Tests for special intervals. *)

let is_empty i = (i === empty)

let is_full i = (i === full)

let is_singleton i =
  let (a,l) = lo i in
  let (b,k) = hi i in
  if l && k && Extq.eq a b then
    Extq.to_q a
  else 
    None

let is_zero i = (i === zero)

(*s Test if [q] is in the interval [i]. *)

let mem q {node={lo=(a,alpha); hi=(b,beta)}} =
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

let pp fmt {node={lo=(a,alpha); hi=(b,beta)}} =
  Format.fprintf fmt "@[";
  Format.fprintf fmt "%s" (if alpha && Extq.is_q a then "[" else "(");
  Extq.pp fmt a;
  Format.fprintf fmt "..";
  Extq.pp fmt b;
  Format.fprintf fmt "%s@]" (if beta && Extq.is_q b then "]" else ")")
	

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

let inter dom {node={lo=(a,alpha); hi=(b,beta)}}  
           {node={lo=(c,gamma); hi=(d,delta)}} =
  make dom (Extq.max a c, mu a c gamma alpha)
         (Extq.min b d, mu b d beta delta)
     
let union dom {node={lo=(a,alpha); hi=(b,beta)}}  
              {node={lo=(c,gamma); hi=(d,delta)}} =
  make dom (Extq.min a c, mu a c alpha gamma)
           (Extq.max b d, mu b d delta beta)


type one_or_two =
  | Empty
  | One of t
  | Two of t * t

let compl d {node={lo=(a,alpha); hi=(b,beta)}} =
  match Extq.eq a Extq.neginf, Extq.eq b Extq.posinf with
    | true, true -> Empty
    | true, false -> One(make d (b, not beta) posinf)
    | false, true -> One(make d neginf (a, not alpha))
    | false, false -> Two(make d neginf (a, not alpha), make d (b, not alpha) posinf)

(*s The implementation of interval arithmetic 
 operations follows the tables given in ``Interval Arithmetic: 
 from Principles to Implementation'' by Hickey, Ju, and Emden in the 
 Journal of ACM, 2002. *)


(*s Addition and Subtraction. *)

let add dom {node={lo=(a,alpha); hi=(b,beta)}}
            {node={lo=(c,gamma); hi=(d,delta)}} =
  make dom
    (Extq.add a c, alpha && gamma)
    (Extq.add b d, beta && delta) 
    

(*s Classification of intervals according to the signs of endpoints. *)
  
type classification = 
  | M                 (*s [(a,b)] in [M] iff [a < 0 < b]. *)
  | Z                 (*s [(0,0)] is only interval in [Z]. *)
  | P0                (*s [(0,b)] in [P0] iff [b > 0]. *)
  | P1                (*s [(a,b)] in [P1] iff [0 < a <= b]. *)
  | N0                (*s [(a,0)] in [N0] iff [a < 0]. *)
  | N1                (*s [(a,b)] in [N1] iff [a <= b < 0]. *)


let classify {node={lo=(a,alpha); hi=(b,beta)}} =
  match Extq.sign a with
    | Zero ->
	(match Extq.sign b with
	   | Zero -> Z
	   | Pos -> P0
	   | Neg -> assert false)
    | Pos ->
	P1
    | Neg ->
	(match Extq.sign b with
	   | Zero -> N0
	   | Neg -> N1
	   | Pos -> M)

(*s Multiplication of general real intervals. *)


let mult dom ({node={lo=(a,alpha); hi=(b,beta)}} as i)
             ({node={lo=(c,gamma); hi=(d,delta)}} as j) =
  let kind a c l k =
    (l && k) || 
    (l && (Extq.is_zero a)) || 
    (k && (Extq.is_zero c))
  in
  let ( * ) = Extq.mult in
  match classify i, classify j with
    | (P0 | P1), (P0 | P1) ->
	make dom (a * c, kind a c alpha gamma) (b * d, kind b d beta delta)
    | (P0 | P1), M ->
	make dom (b * c, kind b c beta gamma) (b * d, kind b d beta delta)
    | (P0 | P1), (N0 | N1) ->
	make dom (b * c, kind b c beta gamma) (a * d, kind a d alpha delta)
    | M, (P0 | P1) ->
	make dom (a * d, kind a d alpha delta) (b * d, kind b d beta delta)
    | M, M ->
	union dom
	  (make dom (a * d, kind a d alpha delta) (b * d, kind b d beta delta))
	  (make dom (b * c, kind b c beta gamma) (a * c, kind a c alpha gamma))
    | M, (N0 | N1) ->
	make dom (b * c, kind b c beta gamma) (a * c, kind a c alpha gamma)
    | (N0 | N1), (P0 | P1) ->
	make dom (a * d, kind a d alpha delta) (b * c, kind b c beta gamma)
    | (N0 | N1), M ->
	make dom (a * d, kind a d alpha delta) (a * c, kind a c alpha gamma)
    | (N0 | N1), (N0 | N1) ->
	make dom (b * d, kind b d beta delta) (a * c, kind a c alpha gamma)
    | Z, (P0 | P1 | M | N0 | N1) ->
	zero
    | (P0 | P1 | M | N0 | N1 | Z), Z ->
	zero

let multq dom q = mult dom (singleton q)
	
let div dom ({node={lo=(a,alpha); hi=(b,beta)}} as i)
        ({node={lo=(c,gamma); hi=(d,delta)}} as j) =
  let make = make Dom.Real in
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
	union dom
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
	union dom
	  (make neginf (b / d, kind b d beta delta))
	  (make (b / c, kind b c beta gamma) posinf)
    | (N0 | N1), N0 ->
	make (b / c, kind b c beta gamma) posinf
    | (N0 | N1), N1 ->
	make (b / c, kind b c beta gamma) (a / d, kind a d alpha delta)
    | Z, (P0 | P1 | M | N0 | N1) ->
	zero
    | _, Z ->
	empty

(*s [i before j] (resp. [j after i]) iff *)
(*s [lo(i)<lo(j), lo(i)<hi(j), hi(i)<lo(j), hi(i)<hi(j)] *)  
(*s [i meets j] (resp. [j metby i]) iff *)
(*s [lo(i)<lo(j), lo(i)<hi(j), hi(i)=lo(j), hi(i)<hi(j)] *)
(*s [i overlaps j] (resp. [j overlappedby i] iff *)
(*s [lo(i)<lo(j), lo(i)<hi(j), hi(i)>lo(j), hi(i)<hi(j)] *)
(*s [i sub j] (resp. [j super i]) iff *)
(*s [lo(i)>lo(j), lo(i)<hi(j), hi(i)>lo(j), hi(i)<hi(j) *)
(*s [i starts j] (resp. [j startedby i]) iff *)
(*s [lo(i)=lo(j),lo(i)<hi(j),hi(i)>lo(j),hi(i)<hi(j)] *)
(*s [i finishes j] (resp. [j finishedby i] iff *)
(*s [lo(i)>lo(j), lo(i)<hi(j),hi(i)>lo(j),hi(i)=hi(j)] *)
(*s [x equals y] iff *)
(*s [lo(i)=lo(j), lo(i)<hi(j),hi(i)>lo(j),hi(i)=hi(j)] *)
 

let rec cmp ({node={lo=li;hi=hi}} as i) ({node={lo=lj;hi=hj}} as j) =
  if i === j then
    Equals 
  else if i === empty then
    Sub
  else if j === empty then
    Super
  else 
    match Extq.cmp (value lj) (value hi) with
    | Q.Greater -> Before
    | Q.Equal ->
	(match kind lj , kind hi with
	   | false, false -> Before
	   | true, true -> Overlaps
	   | _ -> Meets)
    | Q.Less -> 
	(match Extq.cmp (value li) (value hj) with
	   | Q.Greater -> After
	   | Q.Equal -> 
	       (match kind li, kind hj with
		  | false, false -> After
		  | true, true -> Overlaps
		  | _ -> MetBy)
	   | Q.Less ->
	       (match Extq.cmp (value li) (value lj), Extq.cmp (value hi) (value hj) with
		  | Q.Less, Q.Less -> Overlaps
		  | Q.Less, Q.Equal -> 
		      (match kind hi, kind hj with
			 | false, true -> Overlaps
			 | true, false -> Super 
			 | _ -> FinishedBy)
		  | Q.Less, Q.Greater -> Super
		  | Q.Equal, Q.Less -> 
		      (match kind li, kind lj with
			 | false, true -> Sub
			 | true, false -> Overlaps
			 | _ -> Starts)
		  | Q.Equal, Q.Equal ->
		      cmp_equal_equal (kind li, kind hi) (kind lj, kind hj)
		  | Q.Equal, Q.Greater -> 
		      (match kind li, kind lj with
			 | false, true -> Overlaps
			 | true, false -> Super
			 | _ -> StartedBy)
		  | Q.Greater, Q.Less -> Sub
		  | Q.Greater, Q.Equal -> 
		      (match kind hi, kind hj with
			 | false, true -> Sub 
			 | true, false -> Overlaps
			 | _ -> Finishes)
		  | Q.Greater, Q.Greater -> OverlappedBy))

and cmp_equal_equal (k1,k2) (l1,l2) =
  if k1 = l1 && k2 = l2 then
    Equals
  else 
    match k1,k2,l1,l2 with
      | false, false, true, true -> Sub
      | true, true, false, false -> Super
      | true, true, true, false -> StartedBy
      | true, false, true, true -> Starts
      | false, true, true, true -> Finishes
      | true, true, false, true -> FinishedBy
      | _ -> Overlaps

		     

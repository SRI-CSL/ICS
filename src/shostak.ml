
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
 * Author: Harald Ruess, N. Shankar
 i*)

(*i*)
open Term
(*i*)

(*s Decision procedure state. *)

type t = {
  ctxt : Atom.Set.t;    (* Current context. *)
  u : Cc.t;             (* Congruence closure data structure. *)
  i : Th.t;             (* Interpreted theories. *)
  d : D.t               (* Disequalities. *)
}

let empty = {
  ctxt = Atom.Set.empty;
  u = Cc.empty;
  i = Th.empty;
  d = D.empty
}


(*s Canonical variables module [s]. *)

let v s = Cc.v s.u


(*s Constraint of [a] in [s]. *)

let cnstrnt s = Th.cnstrnt s.i

let deq s = D.deq_of s.d


(*s Pretty-printing. *)
  
let pp fmt s =
  Cc.pp fmt s.u;
  Th.pp fmt s.i;
  D.pp fmt s.d


(*s [is_diseq s a b] holds iff if [a] and [b] are known to be
 disequal in context [s]. *)

let is_diseq s a b =
  Term.is_diseq a b || D.is_diseq s.d a b


(*s Equality theories. *)

type e = 
  | Uninterp 
  | Interp of Th.i

let index a =
  assert(not(is_var a));
  match Th.index (Term.sym_of a) with
    | Some(op) -> Interp(op)
    | None -> Uninterp

let name_of = function
  | Uninterp -> "u"
  | Interp(i) -> Th.name_of i

let of_name = function
  | "u" -> Uninterp
  | x -> Interp(Th.of_name x)  (* might raise [Invalid_argument] *)


(*s Parameterized operations. *)

let inv i s = 
  match i with
    | Uninterp -> Cc.u s.u 
    | Interp(i) -> Th.inv i s.i

let find i s x =
  match i with
    | Interp(i) -> (try Th.find i s.i x with Not_found -> x)
    | Uninterp -> x

let use i s = 
  match i with
    | Interp(i) -> Th.use i s.i
    | Uninterp -> Cc.use s.u

let sigma = function
  | Uninterp -> App.sigma
  | Interp i -> Th.sigma i

(*s Return solution sets. *)

let solution e s =
  match e with
    | Uninterp -> Cc.solution s.u
    | Interp(i) -> Th.solution i s.i 

let cnstrnts s =
  Th.cnstrnts s.i


(*s Variable partitioning. *)

let partition s = Cc.v_of s.u


(*s Abstracting a term [a] in theory [i]. *)

let rec abs i s a =
  if Term.is_var a then
    (s, a) 
  else
    match index a with
      | Uninterp -> 
	  let (x,u') = Cc.extend a s.u in
	  ({s with u = u'}, x)
      | Interp(th) ->
	  let (x,i') = Th.extend th a s.i in
	  ({s with i = i'}, x)


(*s Canonization of terms. *)

let rec can_t s a =
  if is_var a then
    (s, v s a)
  else 
    let f, l = destruct a in
    let i = index a in  
    let (s',l') = can_l i s l in
    let a' = sigma i f l' in
    try
      (s', v s' (inv i s' a'))
    with
	Not_found ->
	  abs i s' a'
 
	
and can_l i s l =
  List.fold_right 
    (fun x (s, l) ->
       let (s', x') = can_t s x in
       let x'' = find i s x' in        (* not [s'] *)
       let (s'', x''') = abs i s' x'' in
       (s'', x''' :: l))
    l
    (s, [])
  

(*s Canonization of atoms. *)

let rec can s a =
  match a with
    | Atom.True -> (s, Atom.mk_true)
    | Atom.Equal(x,y) -> can_e s (x,y)
    | Atom.Diseq(x,y) -> can_d s (x,y)
    | Atom.In(c,x) -> can_c s c x
    | Atom.False -> (s, Atom.mk_false)
	  
and can_e s (a,b) =
  let (s', x) = can_t s a in
  let (s'', y) = can_t s' b in
  let p = 
    if Term.eq x y then
      Atom.mk_true
    else if is_diseq s'' x y then
      Atom.mk_false
    else
      Atom.mk_equal x y
  in
  (s'', p)

and can_d  s (a,b) =
  let (s', x) = can_t s a in   
  let (s'', y) = can_t s' b in
  let p = 
    if Term.eq x y then 
      Atom.mk_false
    else if is_diseq s'' x y then
      Atom.mk_true
    else
      Atom.mk_diseq x y
  in
    (s'', p)

and can_c s c a =
  let (s,x) = can_t s a in
  match cnstrnt s a with
    | None -> 
	(s, Atom.mk_in c x)
    | Some(d) -> 
	(match Cnstrnt.cmp c d with
	   | Binrel.Same | Binrel.Sub -> 
	       (s, Atom.mk_in c x)
	   | Binrel.Super ->
	       (s, Atom.mk_in d x)
	   | Binrel.Disjoint ->
	       (s, Atom.mk_false)
	   | Binrel.Singleton(q) ->
	       (s, Atom.mk_equal x (Arith.mk_num q))
	   | Binrel.Overlap ->
	       (s, Atom.mk_in (Cnstrnt.inter c d) x))



(*s Processing an atom *)

type 'a status = 
  | Valid 
  | Inconsistent 
  | Satisfiable of 'a

let rec process s a =
  let (s', a') = can s a in
  match a' with
    | Atom.True -> Valid
    | Atom.False -> Inconsistent
    | _ -> 
	(try 
	   Satisfiable(process1 s' a')
	 with 
	     Exc.Inconsistent -> Inconsistent)

and process1 s a =
  Trace.msg 1 "Process" a Atom.pp;
  let (s,a) = can s a in
  let s = {s with ctxt = Atom.Set.add a s.ctxt} in
  match a with
    | Atom.Equal(x,y) -> merge s (x,y)
    | Atom.Diseq(x,y) -> diseq s (x,y)
    | Atom.In(c,x) -> add s c x
    | Atom.True -> s  (* ignore. *)
    | Atom.False -> 
	raise Exc.Inconsistent

and merge s (x,y) = 
  mergel s (Veqs.singleton (Veq.make x y))
  
and mergel s es =
  if Veqs.is_empty es then
    s
  else 
    let (e', es') = Veqs.destruct es in
    let (s', acc') = merge1 s e' in
    mergel s' (Veqs.union es' acc')

and merge1 s e =
  Trace.msg 2 "Merge" e Veq.pp;
  let (u',el') = Cc.merge e s.u 
  and (i',el'') = Th.merge e s.i
  and d' = D.merge e s.d in
  ({s with u = u'; i = i'; d = d'},  
   Veqs.union el' el'')

and add s c x =
  Trace.msg 2 "Add" (x,c) (Pretty.infix Term.pp "in" Cnstrnt.pp);
  let (i',es') = Th.add (x,c) s.i in
  mergel {s with i = i'} es'

and diseq s (x,y) =
  Trace.msg 2 "Diseq" (x,y) (Pretty.infix Term.pp "<>" Term.pp);
  {s with d = D.add (x,y) s.d}

(*s Compression. *)

let compress s =
  {s with u = Cc.compress s.u}

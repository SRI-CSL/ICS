
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
open Term
(*i*)

(*s Decision procedure state. *)

type t = {
  ctxt : Atom.Set.t;    (* Current context. *)
  u : Cc.t;             (* Congruence closure data structure. *)
  i : Th.t;             (* Interpreted theories. *)
  c : C.t;              (* Constraints. *)
  d : D.t;              (* Disequalities. *)
  p : Prop.t            (* Propositional cases. *)
}

let empty = {
  ctxt = Atom.Set.empty;
  u = Cc.empty;
  i = Th.empty;
  d = D.empty;
  c = C.empty;
  p = Prop.mk_tt
}


(*s Canonical variables module [s]. *)

let v s x = 
  Cc.v s.u x

(*s Constraint of [a] in [s]. *)

let cnstrnt s a =
  let ctxt x = C.apply s.c (v s x) in
  Cnstrnt.arith ctxt a

let deq s = D.deq_of s.d


(*s Pretty-printing. *)
  
let pp fmt s =
  let v = Cc.v_of s.u in
  let u = Cc.u_of s.u in
  if not(Term.Map.empty == v) then
    (Format.fprintf fmt "v: "; Pretty.tmap fmt v);
  if not(Term.Map.empty == u) then
    (Format.fprintf fmt "u: "; Pretty.tmap fmt u);
  Th.pp fmt s.i;
  if not(C.empty == s.c) then
    (Format.fprintf fmt "c: "; C.pp fmt s.c);
  if not(D.empty == s.d) then
    (Format.fprintf fmt "d: "; D.pp fmt s.d)


(*s [is_diseq s a b] holds iff if [a] and [b] are known to be
 disequal in context [s]. *)

let is_diseq s a b =
  Term.is_diseq a b || D.is_diseq s.d a b


(*s Equality theories. *)

type e = Uninterp | Interp of Th.i

let index f =
  match Sym.destruct f with
    | Sym.Interp(op) -> Interp(Th.index op)
    | _ -> Uninterp

let name_of = function
  | Uninterp -> "u"
  | Interp(i) -> Th.name_of i


(*s Parameterized operations. *)

let inv i s = 
  match i with
    | Uninterp -> Cc.u s.u 
    | Interp(i) -> Th.inv i s.i

let find i s x =
  match i with
    | Interp(i) -> 
	(try Th.find i s.i x with Not_found -> x)
    | Uninterp -> 
	x

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

(*s Variable partitioning. *)

let partition s = Cc.v_of s.u

(*s Abstracting a term [a] in theory [i]. *)

let rec abs i s a =
  if Term.is_const a then
    (s, a) 
  else
    match index (Term.sym_of a) with
      | Uninterp -> 
	  let (x,u') = Cc.extend a s.u in
	  ({s with u = u'}, x)
      | j when i = j ->
	  (s, a)
      | Interp(th) ->
	  let (x,i') = Th.extend th a s.i in
	  ({s with i = i'}, x)


(*s Abstracting toplevel. *)

let topabs (s, a) =
  if is_const a then
    (s, a)
  else 
    match index (sym_of a) with
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
    let i = index (sym_of a) in  
    let (s',l') = can_l i s l in
    let a' = sigma i f l' in
    try
      (s', v s' (inv i s' a'))
    with
	Not_found -> (s', a')
 
	
and can_l i s l =
  List.fold_right 
    (fun x (s,l) ->
       let (s',x') = can_t s x in
       let x'' = find i s x' in        (* not [s'] *)
       let (s'',x''') = abs i s' x'' in
       (s'', x''' :: l))
    l
    (s, [])
  

(*s Canonization of atoms. *)

let rec can_a s a =
  match a with
    | Atom.True -> (s, Atom.mk_true)
    | Atom.Equal(x,y) -> can_e s (x,y)
    | Atom.Diseq(x,y) -> can_d s (x,y)
    | Atom.In(c,x) -> can_c s c x
    | Atom.False -> (s, Atom.mk_false)
	  
and can_e s (a,b) =
  let (s', a) = can_t s a in
  let (s'', b) = can_t s' b in
  let p = 
    if Term.eq a b then
      Atom.mk_true
    else if is_diseq s'' a b then
      Atom.mk_false
    else
      Atom.mk_equal a b
  in
  (s'', p)

and can_d  s (a,b) =
  let (s', a) = topabs (can_t s a) in    (* Arguments of *)
  let (s'', b) = topabs (can_t s' b) in  (* disequalities are always constants. *)
  let p = 
    if Term.eq a b then 
      Atom.mk_false
    else if is_diseq s'' a b then
      Atom.mk_true
    else
      Atom.mk_diseq a b
  in
    (s'',p)

and can_c s c a =
  let (s,a) = can_t s a in
  match cnstrnt s a with
    | None -> 
	(s, Atom.mk_in c a)
    | Some(d) -> 
	(match Number.cmp c d with
	   | Binrel.Same | Binrel.Sub -> 
	       (s, Atom.mk_in c a)
	   | Binrel.Super ->
	       (s, Atom.mk_in d a)
	   | Binrel.Disjoint ->
	       (s, Atom.mk_false)
	   | Binrel.Overlap ->
	       (s, Atom.mk_in (Number.inter c d) a))



(*s Canonization of propositions. *)

let rec can_p s p =
  match Prop.destruct p with
    | Prop.True | Prop.False -> (s,p)
    | Prop.Ite(a,x,y) ->
	let (s', a') = can_a s a in
	(s', Prop.mk_ite a' x y)


(*s Processing an atom *)

type 'a status = 
  | Valid 
  | Inconsistent 
  | Satisfiable of 'a

let rec process_a s a =
  let (s', a') = can_a s a in
  match a' with
    | Atom.True -> Valid
    | Atom.False -> Inconsistent
    | _ -> 
	(try 
	   let s'' = processl s' [a'] in
	   match process_p {s'' with p = Prop.mk_tt} s'.p with (* Reprocess *)
	      | Valid -> Satisfiable s''
	      | res -> res
	 with 
	     Exc.Inconsistent -> Inconsistent)

and processl s = function
  | [] -> s
  | a :: al -> 
      let (s',al') = process1 s a in
      processl s' (al' @ al)

and process1 s a =
  Trace.msg 1 "Process" a Pretty.atom;
  let (s,a) = can_a s a in
  let s = {s with ctxt = Atom.Set.add a s.ctxt} in
  match a with
    | Atom.Equal(x,y) -> equal s (x,y) 
    | Atom.Diseq(x,y) -> (diseq s (x,y), [])
    | Atom.In(c,x) -> inn s c x
    | Atom.True -> (s, [])
    | Atom.False -> raise Exc.Inconsistent

and equal s ((x,y) as e) = 
  match pure e with
    | Some(i) ->
	let (c',al') = 
	  match d_numequal e with
	    | Some(x,q) -> C.add (Number.mk_singleton q) x s.c
	    | None -> (s.c, [])
	in
	let (i'',xl'',al'') = Th.merge i (cnstrnt s) e s.i in
	mergel {s with i = i''; c = c'} xl'' (al' @ al'')
    | _ -> 
	let (s', x') = topabs (s, x) in
	let (s'', y') = topabs (s', y) in
	mergel s'' [(x', y')] []

and d_numequal (x,y) =
  match Arith.d_num x, Arith.d_num y with
    | Some(q), _ when V.is y -> Some(y,q)
    | _, Some(p) when V.is x -> Some(x,p)
    | _ -> None
  
and mergel s xl al =
  match xl with
    | [] -> (s, al)
    | ((x,y) as e) :: l ->
	if Cc.veq s.u x y then
	  mergel s l al
	else 
	  let (s',xl',al') = merge1 s e in
	  mergel s' (V.union xl' l) (al @ al')

and merge1 s e =
  let (u',xl) = Cc.merge e s.u in
  let (i',yl,al) = Th.merge_all (cnstrnt s) e s.i in
  let d' = D.merge e s.d in
  let (c',bl) = C.merge e s.c in
  let s' = {s with u = u'; i = i'; d = d'; c = c'} in
  let ((xl',al') as deriv) = (V.union xl yl, al @ bl) in
  (s', xl', al')

and diseq s (x,y) = 
  let d' = D.add (x,y) s.d in
  {s with d = d'}

and inn s c a =
  if Arith.is_interp a then                 (*s Slackify *)
    let k = Term.mk_const(Sym.mk_fresh_slack c) in
    let (i', xl, al) = Th.merge Th.A (cnstrnt s) (k,a) s.i in
    let s' = {s with i = i'} in
    mergel s' xl al
  else 
    let (c',al) = C.add c a s.c in
    let s' = {s with c = c'} in
    let al' = Th.propagate (cnstrnt s') (a,c) s.i in 
    (s', al @ al')

and pure (x,y) =
  let i = index (sym_of x) in
  let j = index (sym_of y) in
  match i, j with
    | Uninterp, Interp(j) when is_const x -> Some(j)
    | Interp(i), Uninterp when is_const y -> Some(i)
    | Interp(i), Interp(j) when i = j -> Some(i)
    | _ -> None

and pp_infer fmt (xl,al) = 
  V.pp fmt xl;  Pretty.list Pretty.atom fmt al


(*s Processing a proposititonal structures. We do a case split immediately,
    and whenever one of the two states is inconsistent we follow the other
    branch. Otherwise we delay the case split by adding the proposition
    to the field [p]. *)

and process_p s p = 
  Trace.msg 1 "Process" p Pretty.prop;
  let (s,p) = can_p s p in
  match Prop.destruct p with
    | Prop.True ->
	Valid
    | Prop.False -> 
	Inconsistent
    | _ ->
	(match Prop.d_conj p with
	   | None -> 
	       Satisfiable({s with p = Prop.mk_conj p s.p})
	   | Some(al,x) ->
	       try
		 let t =  processl s al in
		 Satisfiable({t with p = Prop.mk_conj x t.p})
	       with
		   Exc.Inconsistent -> Inconsistent)


(*s Compression. *)

let compress s =
  let f = Cc.v s.u in   (* canonical variables. *)
  let i' = Th.inst f s.i in
  let c' = C.inst f s.c in
  let d' = D.inst f s.d in
  let u' = Cc.compress s.u in
  {s with u = u'; i = i'; d = d'; c = c'}

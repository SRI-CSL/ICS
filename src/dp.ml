
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
open Sym
open Th
open Term
(*i*)

(*s Decision procedure state. *)

type t = {
  ctxt : Atom.Set.t;    (* current context. *)
  u : Cc.t;             (* Congruence closure data structure. *)
  i : Th.t;             (* interpreted theories. *)
  c : C.t;              (* constraints. *)
  d : D.t;              (* Disequalities. *)
  p : Prop.t            (* propositional cases. *)
}

let empty = {
  ctxt = Atom.Set.empty;
  u = Cc.empty;
  i = Th.empty;
  d = D.empty;
  c = C.empty;
  p = Prop.mk_tt
}

(*s Accessors. *)

let ctxt_of s = s.ctxt
let v_of s = Cc.v_of s.u
let u_of s = Cc.u_of s.u
let la_of s = Th.la_of s.i
let t_of s = Th.t_of s.i
let bv_of s = Th.bv_of s.i
let nla_of s = Th.nla_of s.i
let d_of s = D.deq_of s.d
let c_of s = C.cnstrnt_of s.c
let p_of s = s.p


(*s Pretty-printing. *)
  
let pp fmt s =
  Pretty.list Pretty.atom fmt (Atom.Set.elements s.ctxt)


(*s Compute best constraints. As long as the dynamic type table
 is empty, one can simply use [of_term0] which computes types
 from static information only. This is more efficient, since
 [of_term0] is memoized. *)

let cnstrnt s a =
  let ctxt x = C.apply s.c (Cc.v s.u x) in
  Cnstrnt.arith ctxt a

(*s [is_diseq s a b] holds iff if [a] and [b] are known to be
 disequal in context [s]. *)

let is_diseq s a b =
  Term.is_diseq a b || D.is_diseq s.d a b


(*s Equality theories. *)

type e = Uninterp | Interp of Th.i

let index f =
  match Sym.destruct f with
    | Sym.Uninterp _ -> Uninterp
    | Sym.Interp(op) ->
	(match op with
	   | Sym.Tuple _ -> Interp(Th.T)
	   | Sym.Bv _ -> Interp(Th.BV)
	   | Sym.Arith _ -> Interp(Th.LA)
	   | Sym.Nonlin _ -> Interp(Th.NLA)
	   | Sym.Bool _ -> Uninterp)
    | Sym.Internal _ -> 
	Uninterp


(*s Parameterized operations. *)


let find e s a =
  match e with
    | Uninterp -> (try Cc.u s.u a with Not_found -> a)
    | Interp(i) -> Th.find i s.i a

let inv i s a = 
  try
    Th.inv i s.i a
  with
      Not_found -> a


(*s Abstracting a term [a] in theory [i]. *)

let rec abs i s a =
  if Term.is_const a then
    (s, a) 
  else
    let j = index (Term.sym_of a) in
    if i = j then
      (s, a)
    else 
      match j with
	| Uninterp -> absu s a
	| Interp(j') -> absi j' s a

and absu s a = 
  let (x,u') = Cc.extend a s.u in
  ({s with u = u'}, x)

and absi i s a =
  let (x,i') = Th.extend i a s.i in
  ({s with i = i'}, x)


(*s Abstracting toplevel also. *)

let topabs (s, a) =
  match Sym.d_interp (Term.sym_of a) with 
    | Some(op) -> absi (Th.index op) s a
    | None -> if is_const a then (s, a) else absu s a


(*s Introduce slack variables. *)

let slackify s c a =
  if Linarith.is_interp a then
    let k = Term.mk_const(Sym.mk_slack c) in
    let (i',xl,al) = Th.merge Th.LA (cnstrnt s) (k,a) s.i in
    let s' = {s with i = i'}  in (* to do *)
    (s', Atom.mk_true)
  else 
    (s, Atom.mk_in c a)

(*s Canonization of terms. *)

let rec can_t s a =
  let can_l i s l =
    List.fold_right 
      (fun x (s,l) ->
	 let (s',x') = can_t s x in
	 let (s'',x'') = abs i s' x' in
	 (s'', x'' :: l))
      l
      (s, [])
  in
  let f, l = Term.destruct a in
  match Sym.destruct f, l with 
    | Sym.Interp(op), l -> 
	let i = Th.index op in
	let (s', l') = can_l (Interp(i)) s l in
	let a' = inv i s' (Th.sigma op l') in
	(s', Cc.v s'.u a')
    | _, [] ->      
	(s, Cc.v s.u a)  
    | _ ->
	let (s,l) = can_l Uninterp s l in
	let a = Term.mk_app f l in
	(try
	   (s, Cc.u s.u a)
	 with
	     Not_found -> absu s a)
	

  

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
  let (s, b) = can_t s' b in
  let p = 
    if Term.eq a b then
      Atom.mk_true
    else if is_diseq s a b then
      Atom.mk_false
    else
      Atom.mk_equal a b
  in
  (s, p)

and can_d  s (a,b) =
  let (s', a) = topabs (can_t s a) in    (* Arguments of *)
  let (s, b) = topabs (can_t s' b) in    (* disequalities are always constants. *)
  let p = 
    if Term.eq a b then 
      Atom.mk_false
    else if is_diseq s a b then
      Atom.mk_true
    else
      Atom.mk_diseq a b
  in
    (s,p)

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
	   let t = processl s [a] in
	   match process_p {t with p = Prop.mk_tt} t.p with  (* Reprocess propositions. *)
	      | Valid -> Satisfiable t
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
  match a with
    | Atom.Equal(x,y) -> equal s (x,y) 
    | Atom.Diseq(x,y) -> (diseq s (x,y), [])
    | Atom.In(c,x) -> inn s c x
    | Atom.True -> (s, [])
    | Atom.False -> raise Exc.Inconsistent

and equal s ((x,y) as e) = 
  match pure e with
    | Some(i) ->
	let (i',xl',al') = Th.merge i (cnstrnt s) e s.i in
	mergel {s with i = i'} xl' al'
    | _ -> 
	let (s', x') = topabs (s, x) in
	let (s'', y') = topabs (s', y) in
	mergel s'' [(x', y')] []

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
  Trace.call 2 "Merge" e Pretty.eqn;
  let pp fmt (xl,al) = V.pp fmt xl;  Pretty.list Pretty.atom fmt al in
  let (u',xl) = Cc.merge e s.u in
  let _ = Trace.msg 3 "Deriv(cc)" (xl,[]) pp in
  let (i',yl,al) = Th.merge_all (cnstrnt s) e s.i in
  let _ = Trace.msg 3 "Deriv(i)" (yl,al) pp in
  let d' = D.merge e s.d in
  let _ = Trace.msg 3 "Deriv(d)" ([],[]) pp in
  let (c',bl) = C.merge e s.c in
  let _ = Trace.msg 3 "Deriv(c)" ([],bl) pp in
  let s' = {s with u = u'; i = i'; d = d'; c = c'} in
  let ((xl',al') as deriv) = (V.union xl yl, al @ bl) in
  Trace.exit 2 "Merge" deriv pp;
  (s', xl', al')

and diseq s (x,y) = 
  let d' = D.add (x,y) s.d in
  {s with d = d'}

and inn s c a =
  if Linarith.is_interp a then                 (*s Slackify *)
    let k = Term.mk_const(Sym.mk_slack c) in
    let (i', xl, al) = Th.merge Th.LA (cnstrnt s) (k,a) s.i in
    let _ = Trace.msg 3 "Deriv(la)" (xl, al) pp_infer in
    let s' = {s with i = i'} in
    mergel s' xl al
  else 
    let (c',al) = C.add c a s.c in
    let _ = Trace.msg 3 "Deriv(c)" ([], al) pp_infer in
    let s' = {s with c = c'} in
    let al' = Th.propagate (cnstrnt s') (a,c) s.i in 
    let _ = Trace.msg 3 "Deriv(i)" ([], al') pp_infer in
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


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
open Context
open Three
(*i*)

(*s Sigma normal forms with builtin uninterpreted functions. *)

let sigma s f l =
  match Interp.index f with 
    | Some _ -> 
	Th.sigma f l
    | None -> 
	if Sym.is_builtin f then
	  Builtin.sigma s f l
	else 
	  App.sigma f l

(*s Only interpreted find. *)

let find i s x =
  match i with
    | Theories.Interp(i) -> Th.find i s.i x
    | Theories.Uninterp -> x


(*s Abstracting a term [a] in theory [i]
 by introducing a new name for it. *)

let extend s a =
  match Theories.index a with
    | Theories.Uninterp -> 
	let (x',u') = Cc.extend a s.u in
	({s with u = u'}, x')
    | Theories.Interp(th) ->
	let (x',i') = Th.extend th  a s.i in
	({s with i = i'}, x')


(*s Canonization of terms. *)

let rec can_t s a =
  let (s',a') = can_term s a in
  if is_var a' then (s', a') else extend s' a'

and can_term s a =
  if is_var a then
    can_var s a
  else 
    let f, l = destruct a in
    let i = Theories.index a in  
    let (s',l') = can_list i s l in
    let a' = sigma s f l' in
    try
      (s', v s' (inv i s' a'))
    with
	Not_found -> (s', a')

and can_var s x =
  (s, v s x)
 
and can_list i s l =
  List.fold_right 
    (fun x (s, l) ->
       let (s', x') = can_term s x in
       let x'' = find i s x' in (*i not [s'] i*)
       if Term.is_var x'' || 
          (i <> Theories.Uninterp && 
	   i = Theories.index x'') 
       then
	 (s', x'' :: l)
       else 
	 let (s'', x''') = extend s' x'' in
	 (s'', x''' :: l))
    l
    (s, [])

and eq s a b =
  let (s',a') = can_t s a in
  let (_, b') = can_t s' b in
  Term.eq a' b'

(*s Canonization of atoms. *)

let rec can s a = 
  Trace.call "top" "Can" a Atom.pp;
  let (s',a') = match a with
    | Atom.True -> (s, Atom.mk_true())
    | Atom.Equal(x,y) -> can_e s (x,y)
    | Atom.Diseq(x,y) -> can_d s (x,y)
    | Atom.In(c,x) -> can_c s c x
    | Atom.False -> (s, Atom.mk_false())
  in
  Trace.exit "top" "Can" a' Atom.pp;
  (s',a')
	  
and can_e s (a, b) =
  let (s', x') = can_t s a in
  let (s'', y') = can_t s' b in
  match Context.is_equal s'' x' y' with
    | Yes -> (s'', Atom.mk_true())
    | No -> (s'', Atom.mk_false())
    | _ -> (s'', Atom.mk_equal x' y')
 
and can_d s (a, b) =
  let (s', x') = can_t s a in
  let (s'', y') = can_t s' b in
  match Context.is_equal s'' x' y' with
    | Yes -> (s'', Atom.mk_false())
    | No -> (s'', Atom.mk_true())
    | X -> (s'', Atom.mk_diseq x' y')

and can_c s c a =
  let (s', a') = can_term s a in  (* result not necessarily a variable. *) 
  try                 
    let d = cnstrnt s' a' in
    match Cnstrnt.cmp c d with
      | Binrel.Sub -> 
	  (s', Atom.mk_in c a')
      | (Binrel.Super | Binrel.Same) ->
	  (s', Atom.mk_true())
      | Binrel.Disjoint ->
	  (s', Atom.mk_false())
      | Binrel.Singleton(q) ->
	  can_e s' (a', Arith.mk_num q)
      | Binrel.Overlap(cd) ->
	  (s', Atom.mk_in cd a')
  with
      Not_found -> 
	(s', Atom.mk_in c a')


(*s Processing an atom *)

type 'a status = 
  | Valid 
  | Inconsistent 
  | Satisfiable of 'a

let rec process s a =  
  Trace.msg "top" "Process" a Atom.pp;
  let s = {s with ctxt = Atom.Set.add a s.ctxt} in
  let (s', a') = can s a in
  try
    match a' with
      | Atom.True -> Valid
      | Atom.False -> Inconsistent
      | Atom.Equal(x,y) -> Satisfiable(merge s' (x,y))
      | Atom.Diseq(x,y) -> Satisfiable(diseq s' (x,y))
      | Atom.In(c,a) -> Satisfiable(add s' c a)
  with 
      Exc.Inconsistent -> Inconsistent

and merge s ((x,y) as e) = 
  mergel s (Veqs.singleton (Veq.make x y))
  
and mergel s es =
  if Veqs.is_empty es then
    s
  else 
    let (e', es') = Veqs.destruct es in
    let (s', es'') = merge1 s e' in
    mergel s' (Veqs.union es' es'')

and merge1 s e =
  Trace.call "top" "Merge" e Veq.pp;
  let (u',es') = Cc.merge e s.u 
  and (i',es'') = Th.merge e s.i in
  let es''' =  Veqs.union es' es'' in
  Trace.exit "top" "Merge" es''' Veqs.pp;
  ({s with u = u'; i = i'}, es''')

and add s c a =
  Trace.call "top" "Add" (a,c) pp_in;
  let (i',es') = Th.add (a,c) s.i in
  Trace.exit "top" "Add" es' Veqs.pp;
  mergel {s with i = i'} es'

and diseq s (x,y) =
  Trace.call "top" "Diseq" (x,y) pp_diseq;
  let (u', es') = Cc.diseq (x,y) s.u in
  Trace.exit "top" "Diseq" es' Veqs.pp;
  mergel {s with u = u'} es'  

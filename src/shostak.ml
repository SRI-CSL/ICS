
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


(*s Abstracting a term [a] in theory [i]. *)

let abs s a =
  match Theories.index a with
    | Theories.Uninterp -> 
	let (x,u') = Cc.extend a s.u in
	({s with u = u'}, x)
    | Theories.Interp(th) ->
	let (x,i') = Th.extend th a s.i in
	({s with i = i'}, x)


(*s Canonization of terms. *)

let rec can_t s a =
  let (s',a') = can_term s a in
  if is_var a' then (s', a') else abs s' a'

and can_term s a =
  if is_var a then
    (s, v s a)
  else 
    let f, l = destruct a in
    let i = Theories.index a in  
    let (s',l') = can_list i s l in
    let a' = sigma s f l' in
    try
      (s', v s' (inv i s' a'))
    with
	Not_found ->
	  (s', a')
 
and can_list i s l =
  List.fold_right 
    (fun x (s, l) ->
       let (s', x') = can_term s x in
       let x'' = find i s x' in (*i not [s'] i*)
       if Term.is_var x'' || 
          (i <> Theories.Uninterp && i = Theories.index x'') 
       then
	 (s', x'' :: l)
       else 
	 let (s'', x''') = abs s' x'' in
	 (s'', x''' :: l))
    l
    (s, [])

and is_equal s a b =
  let (s',a') = can_t s a in
  let (_, b') = can_t s' b in
  Term.eq a' b'

(*s Canonization of atoms. *)

let rec can s a = 
  Trace.call 3 "Can" a Atom.pp;
  let (s',a') = match a with
    | Atom.True -> (s, Atom.mk_true)
    | Atom.Equal(x,y) -> can_e s (x,y)
    | Atom.Diseq(x,y) -> can_d s (x,y)
    | Atom.In(c,x) -> can_c s c x
    | Atom.False -> (s, Atom.mk_false)
  in
  Trace.exit 3 "Can" a' Atom.pp;
  (s',a')
	  
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
  if Cnstrnt.is_empty c then
    (s, Atom.mk_false)
  else  
    let (s,x) = can_t s a in
    match Cnstrnt.d_singleton c with
      | Some(q) -> 
	  let (s,y) = can_t s (Arith.mk_num q) in
	  (s, Atom.mk_equal x y)
      | None ->
	  (match cnstrnt s x with
	     | None -> 
		 (s, Atom.mk_in c x)
	     | Some(d) -> 
		 (match Cnstrnt.cmp c d with
		    | Binrel.Sub -> 
			(s, Atom.mk_in c x)
		    | (Binrel.Super | Binrel.Same) ->
			(s, Atom.mk_true)
		    | Binrel.Disjoint ->
			(s, Atom.mk_false)
		    | Binrel.Singleton(q) ->
			let (s',y) = can_t s (Arith.mk_num q) in
			(s', Atom.mk_equal x y)
		    | Binrel.Overlap(cd) ->
			assert(Cnstrnt.d_singleton(cd) = None);
			(s, Atom.mk_in cd x)))


(*s Processing an atom *)

type 'a status = 
  | Valid 
  | Inconsistent 
  | Satisfiable of 'a

let rec process s a =  
  Trace.msg 1 "Process" a Atom.pp;
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
  let s = {s with ctxt = Atom.Set.add a s.ctxt} in
  match a with
    | Atom.Equal(x,y) -> merge s (x,y)
    | Atom.Diseq(x,y) -> diseq s (x,y)
    | Atom.In(c,x) -> add s c x
    | Atom.True -> s  (* ignore. *)
    | Atom.False -> raise Exc.Inconsistent

and merge s ((x,y) as e) = 
  mergel s (Veqs.singleton (Veq.make x y))
  
and mergel s es =
  if Veqs.is_empty es then
    s
  else 
    let (e', es') = Veqs.destruct es in
    let (s', acc') = merge1 s e' in
    mergel s' (Veqs.union es' acc')

and merge1 s e =
  let (u',el') = Cc.merge e s.u 
  and (i',el'') = Th.merge e s.i
  and d' = D.merge e s.d in
  ({s with u = u'; i = i'; d = d'},  
   Veqs.union el' el'')

and add s c x =
  let (i',es') = Th.add (x,c) s.i in
  mergel {s with i = i'} es'

and diseq s (x,y) =
  {s with d = D.add (x,y) s.d}

(*s Compression. *)

let compress s =
  {s with u = Cc.compress s.u}

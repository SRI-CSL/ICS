

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



(*s [term s a] returns a purified term, i.e. a term with function 
  symbols from one theory only. For example, [f(x+1)] is purified 
  to [f(u)] with the side effect that the fresh variable [u] is 
  introduced in the arithmetic state as [u |-> x + 1]. *)

let rec term th s a =
  let ext th' a' =  
    let c' = Dp.cnstrnt s a' in
    let x' = Rename.make c' in
    Dp.extend th' s (x', a'); 
    x'
  in
  let rec loop th a =
    let (f,l) = Term.destruct a in
    if l = [] then a else
      let th'  = Sym.classify f in
      let l' = Term.mapl (loop th') l in
      let a' = if l == l' then a else Term.make(f, l') in
      if th = th' || th' = Sym.U then
	a'
      else 
	ext th' a'
  in
  Trace.call 4 "Purify" a  Pretty.term;
  let b = loop th a in
  Trace.exit 4 "Purify" b Pretty.term;
  b

and atom s a =
  match a with
    | Atom.Equal(x,y) ->
	let x' = term (Term.theory_of x) s x in (* Do not purify toplevel. *)
	let y' = term (Term.theory_of y) s y in
	Atom.mk_equal x' y'    
    | Atom.Diseq(x,y) ->
	let x' = term Sym.U s x in
	let y' = term Sym.U s y in
	Atom.mk_diseq x' y'
    | Atom.In(c, x) ->
	let c' = Type.mk_number c in
	let x' = term (Term.theory_of x) s x in            
	if is_dom x' then          (* If purification yielded an uninterpreted *)
	  Atom.mk_in c x'          (* term of an interpreted constant, then done. *)
	else                       (* Otherwise introduce a slack variable with arity *)
	  let c' = Type.inter c' (Dp.cnstrnt s x') in (* the intersection of type respective types. *)
	  let y' = Rename.make c' in
	  Dp.extend (Term.theory_of x') s (y', x');
	  Atom.mk_true
    | Atom.True ->
	Atom.mk_true
    | Atom.False ->
	Atom.mk_false

and is_dom a =            
  let f,l = Term.destruct a in
  Sym.is_uninterp f || l = []

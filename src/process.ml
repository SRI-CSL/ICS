
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
open Dp
(*i*)

type 'a status = 
  | Valid 
  | Inconsistent 
  | Satisfiable of 'a

(*s Processing an atom. *)

let rec atom s a =
  match Can.atom s a with
    | Atom.True ->
	Valid
    | Atom.False -> 
	Inconsistent
    | b ->
	try
	  let t = atom1 s b in
	  (match prop {t with p = Prop.mk_tt} t.p with     (* Reprocess propositions. *)
	     | Valid -> Satisfiable t
	     | res -> res)
	with
	  | Exc.Inconsistent -> Inconsistent
	  | Exc.Valid -> assert false

and atom1 s a = 
  Trace.msg 4 "Process" a Pretty.atom; 
  Pending.clear ();         (* Clear stack of pending goals. *) 
  let s = Dp.copy s in
  s.ctxt <- Atom.Set.add a s.ctxt;
  process_atom s (Purify.atom s a);
  while Pending.is_nonempty () do
    process_atom s (Pending.pop ());
  done;
  s

and process_atom s a =
  match a with
    | Atom.Equal(x,y) ->
	process_eq s (x,y)
    | Atom.Diseq(x,y) ->
	process_d s (x,y)
    | Atom.In(c,x) ->
	process_in s c x
    | Atom.True ->
	()          (* ignore. *)
    | Atom.False ->
	raise Exc.Inconsistent
  
and process_d s (a,b) =
  s.d <- D.process (a,b) s.d

and process_eq s ((a,b) as e) =
  let f,l = Term.destruct a in
  let g,m = Term.destruct b in
  match Sym.destruct f, l, Sym.destruct g, m with
    | _, [], _, [] -> process_u s e           (* to be added: lookups. *)
    | Sym.Uninterp _,  _, _, []
    | _, [], Sym.Uninterp _, _
    | Sym.Uninterp _, _ , Sym.Uninterp _, _ -> 
	process_u s e
    | Sym.Interp(Sym.Arith _), _, _, _
    | _, _, Sym.Interp(Sym.Arith _), _ ->
	s.a <- Th.A.process e s.a
    | Sym.Interp(Sym.Tuple _), _, _, _
    | _, _, Sym.Interp(Sym.Tuple _), _ ->
	s.t <- Th.T.process e s.t
    | Sym.Interp(Sym.Bv _), _, _, _
    | _, _, Sym.Interp(Sym.Bv _), _ ->
	s.bv <- Th.BV.process e s.bv
    | Sym.Interp(Sym.Nonlin _), _, _, _
    | _, _, Sym.Interp(Sym.Nonlin _), _ ->
	s.nla <- Th.NLA.process e s.nla
    | _ -> 
	process_u s e

and process_u s e =       
  s.u <- U.process e s.u;
  s.d <- D.propagate e s.d;      
  s.a <- Th.A.propagate e s.a;  
  s.t <- Th.T.propagate e s.t;
  s.bv <- Th.BV.propagate e s.bv;
  s.nla <- Th.NLA.propagate e s.nla

 (*s Merging in a newly detected constraint for [x]. This
  does not change the structure but may trigger new constraints
  to be true. *)
 
and process_in s c a =
  s.c <- C.process c a s.c
(*
  Th.A.propc (Dp.cnstrnt s) a c s.a;
  Th.NLA.propc (Dp.cnstrnt s) a c s.nla
*)


(*s Processing a conjunction of atoms. *)

and atoml s = function
  | [] -> Valid
  | [a] -> atom s a
  | a :: al ->
      (match atom s a with
	 | Valid -> atoml s al
	 | Inconsistent -> Inconsistent
	 | Satisfiable(s') ->
	     (match atoml s' al with
		| Valid -> Satisfiable(s')
		| res -> res))

(*s Processing a proposititonal structures. We do a case split immediately,
    and whenever one of the two states is inconsistent we follow the other
    branch. Otherwise we delay the case split by adding the proposition
    to the field [p]. *)

and prop s p =
  let q = Can.prop s p in
  match Prop.destruct q with
    | Prop.True ->
	Valid
    | Prop.False -> 
	Inconsistent
    | _ ->
	(match Prop.d_conj q with
	   | None -> 
	       Satisfiable({s with p = Prop.mk_conj q s.p})
	   | Some(al,x) ->
	       (match atoml s al with
		  | Valid -> Satisfiable({s with p = Prop.mk_conj x s.p})
		  | Inconsistent -> Inconsistent
		  | Satisfiable(t) -> Satisfiable({t with p = Prop.mk_conj x t.p})))


(*s Some tests on atoms. *)

let is_valid s a =
  Atom.eq (Can.atom s a) Atom.mk_true

(*s Check for inconsistency. *)

let inconsistent s =
  let rec incons s p =
    match Prop.destruct p with
      | Prop.True -> false
      | Prop.False -> true
      | Prop.Ite(x,p,n) ->
	  (match atom s x with
	     | Valid -> incons s p
	     | Inconsistent -> incons s n
	     | Satisfiable(sp) ->
		 (match atom s (Atom.mk_neg x) with
		    | Valid -> incons s n
		    | Inconsistent -> incons s p
		    | Satisfiable(sn) ->    
			incons sp p && incons sn n))
  in
  incons s s.p




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
open Name.Map
(*i*)

(*s Global state. *)

type t = {
  mutable current : Dp.t;
  mutable symtab : Symtab.t;
  mutable inchannel : in_channel;
  mutable outchannel : Format.formatter
}

let init () = {
  current = Dp.empty;
  symtab = Symtab.empty;
  inchannel = Pervasives.stdin;
  outchannel = Format.std_formatter
}

let s = init ()

(*s Context. *)

let ctxt_of () = Dp.ctxt s.current

(*s Accessors to components of global state. *)

let current () = s.current
let symtab () = s.symtab

let inchannel () = s.inchannel
let outchannel () = s.outchannel


(*s Adding to symbol table *)

let def n a = 
  let e = Symtab.Def(a) in
  s.symtab <- Symtab.add n e s.symtab

let sgn n a =
  let e = Symtab.Arity(a) in
  s.symtab <- Symtab.add n e s.symtab


let typ n c =
  let e = Symtab.Type(c) in
  s.symtab <- Symtab.add n e s.symtab


(*s Type from the symbol table. *)

let type_of n =
  try
    match Symtab.lookup n s.symtab with
      | Symtab.Type(c) -> c
      | _ -> raise (Invalid_argument "Not a type definition")
  with
      Not_found -> Type.mk_top
 

(*s Term definition from symbol table. *)

let constant_of n =
  try
    match Symtab.lookup n s.symtab with
      | Symtab.Def(a) -> a
      | Symtab.Arity(a) -> Uninterp.mk_uninterp (n,a) []
      | _ -> raise (Invalid_argument "Not a definition")
  with
      Not_found -> 
	Uninterp.mk_uninterp (n,Arity.mk_constant Type.mk_top) []


(*s Uninterpreted function symbol w.r.t to symbol table. *)

let rec funsym_of n x =
  assert(n >= 0);
  try
    match Symtab.lookup x s.symtab with
      | Symtab.Arity(a) -> (x,a)
      | _ -> raise (Invalid_argument "Not a Function Symbol")
  with
      Not_found -> 
	let dl = top n in
	let a = Arity.mk_functorial dl Type.mk_top in
	(x,a)

and top n =
  match n with
    | 0 -> []
    | 1 -> top1
    | 2 -> top2
    | 3 -> top3
    | _ -> Type.mk_top :: top (n - 1)

and top1 = [Type.mk_top]
and top2 = [Type.mk_top; Type.mk_top]
and top3 = [Type.mk_top; Type.mk_top; Type.mk_top]

let enumtype_of n =
  try
    match Symtab.lookup n s.symtab with
      | Symtab.Type(c) ->
	  (match Type.destruct c with
	     | Type.Enumeration(ns) -> ns
	     | _ -> raise (Invalid_argument "Not an enumeration type"))
      | _ -> raise (Invalid_argument "Not a type name")
  with
      Not_found ->
	raise (Invalid_argument "Enumeration type not declared")


(*s Resetting all of the global state. *)

let reset () = 
  Tools.do_at_reset ();
  s.current <- Dp.empty;
  s.symtab <- Symtab.empty;
  s.inchannel <- Pervasives.stdin;
  s.outchannel <- Format.std_formatter

(*s Set input and output channels. *)

let set_inchannel ch = 
  s.inchannel <- ch

let set_outchannel fmt = 
  s.outchannel <- fmt

let flush () = Format.fprintf s.outchannel "@?"
let nl () = Format.fprintf s.outchannel "\n"


(*s Canonization w.r.t current state. *)

let can a = Can.term s.current a

let canatom p = Can.atom s.current p

(*s Adding a new fact *)

let process a =
  let t = current () in
  let status = Process.prop t a in
  (match status with                     (* Update state *)
    | Process.Satisfiable(t') -> 
	s.current <- t' 
    | _ -> ());
  status

(*s Extension of the equivalence class a term. *)

let ext a =
  U.fold (Dp.u_of (current())) Ptset.add Ptset.empty a

(*s Change current state. *)

let save n = 
  let e = Symtab.State(s.current) in
  s.symtab <- Symtab.add n e s.symtab

let restore n =
  try
    match Symtab.lookup n s.symtab with
      | Symtab.State(t) -> s.current <- t
      | _ -> raise Not_found
  with
      Not_found -> raise (Invalid_argument "Not a state name")

let remove n =      
  s.symtab <- Symtab.remove n s.symtab

let forget () =
  s.current <- Dp.empty

(*s Test if [n] is a substate of [m]. *)

let sub n m =
  try
    match Symtab.lookup n s.symtab, Symtab.lookup m s.symtab with
      | Symtab.State(x),Symtab.State(y) -> 
	  Dp.sub Can.atom x y
      | _ -> raise Not_found
  with 
      Not_found -> raise (Invalid_argument "Not a state name")

(*s Investigate the use structure. *)

let use_of th =
  let s = current() in
  match th with
    | Theory.Uninterp -> 
	U.use_of (Dp.u_of s)
    | Theory.Interp(ith) ->
	(match ith with
	   | Theory.A -> Th.A.use_of (Dp.a_of s)
	   | Theory.T -> Th.T.use_of (Dp.t_of s)
	   | Theory.BV -> Th.BV.use_of (Dp.bv_of s)
	   | Theory.NLA -> Th.NLA.use_of (Dp.nla_of s)
	   | _ -> raise (Invalid_argument "No use structure for this theory"))

let use th =
  let s = current() in
  match th with
    | Theory.Uninterp -> 
	U.use (Dp.u_of s)
    | Theory.Interp(ith) ->
	(match ith with
	   | Theory.A -> Th.A.use (Dp.a_of s)
	   | Theory.T -> Th.T.use (Dp.t_of s)
	   | Theory.BV -> Th.BV.use (Dp.bv_of s)
	   | Theory.NLA -> Th.NLA.use (Dp.nla_of s)  
	   | _ -> raise (Invalid_argument "No use structure for this theory"))

let find_of th =
  let s = current() in
  match th with
    | Theory.Uninterp -> 
	U.subst_of (Dp.u_of s)
    | Theory.Interp(ith) ->
	(match ith with
	   | Theory.A -> Th.A.subst_of (Dp.a_of s)
	   | Theory.T -> Th.T.subst_of (Dp.t_of s)
	   | Theory.BV -> Th.BV.subst_of (Dp.bv_of s)
	   | Theory.NLA -> Th.NLA.subst_of (Dp.nla_of s)  
	   | _ -> raise (Invalid_argument "No find structure for this theory"))


let find th =
  let s = current() in
  match th with
    | Theory.Uninterp -> 
	U.find (Dp.u_of s)
    | Theory.Interp(ith) ->
	(match ith with
	   | Theory.A -> Th.A.find (Dp.a_of s)
	   | Theory.T -> Th.T.find (Dp.t_of s)
	   | Theory.BV -> Th.BV.find (Dp.bv_of s)
	   | Theory.NLA -> Th.NLA.find (Dp.nla_of s) 
	   | _ -> raise (Invalid_argument "No find structure for this theory"))

let diseq_of () =
  let s = current () in
  D.deq_of (Dp.d_of s)

let diseq a =
  let s = current () in
  D.deq (Dp.d_of s) a

let cnstrnt a =
  Dp.cnstrnt s.current a

let cnstrnt_of () =
  (C.cnstrnt_of s.current.Dp.c)

let prop_of () =
  Dp.p_of s.current
  
let rec solve th e =
  try
    Some(match th with
	   | Theory.Interp(ith) ->
	       (match ith with
		  | Theory.A -> failwith "to do" (* Linarith.solve s.current.Dp.c e *)
		  | Theory.T -> Tuple.solve e
		  | Theory.BV -> Bv.solve e 
		  | Theory.NLA ->
		      failwith "to do"
 (*  Nonlin.solve (Th.NLA.rename (Dp.cnstrnt s.current) (Dp.nla_of s.current)) e *)
		  | _ -> raise (Invalid_argument "No such solver"))
	   | Theory.Uninterp ->
	       raise (Invalid_argument "No solver for uninterpreted theory"))
  with
      Exc.Inconsistent -> None
   
(*s Toggle variables. *)

type toggle = 
  | Printall

let toggle = function
  | Printall -> Pretty.set_print_all (not (Pretty.get_print_all()))



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

let ctxt_of () = 
  Dp.ctxt_of s.current

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

let can_t p = 
  let (t, b) = Dp.can_t s.current p in
  s.current <- t;
  b

let can_a p = 
  let (t, b) = Dp.can_a s.current p in
  s.current <- t;
  b



(*s Adding a new fact *)

let process_a a =
  let t = current () in
  let status = Dp.process_a t a in
  (match status with                     (* Update state *)
     | Dp.Satisfiable(t') -> 
	 s.current <- t' 
     | _ -> ());
  status

let process_p p =
  let t = current () in
  let status = Dp.process_p t p in
  (match status with                     (* Update state *)
     | Dp.Satisfiable(t') -> 
	 s.current <- t' 
     | _ -> ());
  status



(*s State compression. *)

let compress () =
  s.current <- Dp.compress s.current

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


(*s Accessors. *)

let u () = Dp.u_of s.current
let v () = Dp.v_of s.current
let a () = Dp.la_of s.current
let t () = Dp.t_of s.current
let bv () = Dp.bv_of s.current
let nla () = Dp.nla_of s.current

let diseq_of () =
  Dp.d_of s.current

let diseq a =
  failwith "to do"

let cnstrnt a =
  Dp.cnstrnt s.current a

let cnstrnt_of () =
  Dp.c_of s.current

let prop_of () =
  Dp.p_of s.current
   
(*s Toggle variables. *)

type toggle = 
  | Printall

let toggle = function
  | Printall -> Pretty.set_print_all (not (Pretty.get_print_all()))



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
  mutable current : Shostak.t;
  mutable symtab : Symtab.t;
  mutable inchannel : in_channel;
  mutable outchannel : Format.formatter
}

let init () = {
  current = Shostak.empty;
  symtab = Symtab.empty;
  inchannel = Pervasives.stdin;
  outchannel = Format.std_formatter
}

let s = init ()

(*s Context. *)

let ctxt_of () = s.current.Shostak.ctxt

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
  s.current <- Shostak.empty;
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
  let (t, b) = Shostak.can_t s.current p in
  s.current <- t;
  b

let can_a p = 
  let (t, b) = Shostak.can_a s.current p in
  s.current <- t;
  b



(*s Adding a new fact *)

let process_a a =
  let t = current () in
  let status = Shostak.process_a t a in
  (match status with                     (* Update state *)
     | Shostak.Satisfiable(t') -> 
	 s.current <- t' 
     | _ -> ());
  status

let process_p p =
  let t = current () in
  let status = Shostak.process_p t p in
  (match status with                     (* Update state *)
     | Shostak.Satisfiable(t') -> 
	 s.current <- t' 
     | _ -> ());
  status



(*s State compression. *)

let compress () =
  s.current <- Shostak.compress s.current

(*s Change current state. *)

let save n = 
  let e = Symtab.State (s.current) in
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
  s.current <- Shostak.empty


(*s Accessors. *)

let u_of () = Cc.u_of s.current.Shostak.u
let v_of () = Cc.v_of s.current.Shostak.u
let a_of () = Th.la_of s.current.Shostak.i
let t_of () = Th.t_of s.current.Shostak.i
let bv_of () = Th.bv_of s.current.Shostak.i
let diseq_of () = D.deq_of s.current.Shostak.d
let cnstrnt_of () = C.cnstrnt_of s.current.Shostak.c
let prop_of () = s.current.Shostak.p

let diseq a =
  failwith "to do"

let cnstrnt a =
  Shostak.cnstrnt s.current a

(*s Applying maps. *)

let find e = Shostak.find e s.current
let inv e = Shostak.inv e s.current
let use e = Shostak.use e s.current

(*s Solution sets. *)

let solution e = Shostak.solution e s.current

(*s Variable partitioning. *)

let partition () = Shostak.partition s.current
   
(*s Toggle variables. *)

type toggle = 
  | Printall

let toggle = function
  | Printall -> Pretty.set_print_all (not (Pretty.get_print_all()))


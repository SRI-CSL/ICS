
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
  mutable current : Context.t;
  mutable symtab : Symtab.t;
  mutable inchannel : in_channel;
  mutable outchannel : Format.formatter
}

let init () = {
  current = Context.empty;
  symtab = Symtab.empty;
  inchannel = Pervasives.stdin;
  outchannel = Format.std_formatter
}

let s = init ()

(*s Context. *)

let ctxt_of () = 
  Atom.Set.elements s.current.Context.ctxt

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


let entry_of n = 
  try
    Some(Symtab.lookup n s.symtab)
  with
      Not_found -> None
			   
(*s Type from the symbol table. *)

let type_of n =
  match Symtab.lookup n s.symtab with
    | Symtab.Type(c) -> Some(c)
    | _ -> None

(*s Getting the width of bitvector terms from the signature. *)

let width_of a =
  if Term.is_var a then
    let n = Term.name_of a in
    try
      match Symtab.lookup n s.symtab with
	| Symtab.Arity(i) -> Some(i)
	| _ -> None
    with
	Not_found -> None
  else
    Bitvector.width a

(*s Resetting all of the global state. *)

let reset () = 
  Tools.do_at_reset ();
  s.current <- Context.empty;
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


let can p = 
  let (t, b) = Shostak.can s.current p in
  s.current <- t;
  b

let cant p = 
  let (t, b) = Shostak.can_t s.current p in
  s.current <- t;
  b

let sigma f l =
  Context.sigma s.current f l


(*s Adding a new fact *)

let process a =
  let t = current () in
  let status = Shostak.process t a in
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
  s.current <- Context.empty


(*s Accessors. *)

let diseq a =
  let a' = cant a in
  Term.Set.elements (D.deq (s.current.Context.d) a')

let cnstrnt a =
  let a' = cant a in
  Context.cnstrnt s.current a'

(*s Applying maps. *)

let find e = Context.find e s.current
let inv e = Context.inv e s.current
let use e = Context.use e s.current

(*s Solution sets. *)

let solution e = Context.solution e s.current

(*s Variable partitioning. *)

let partition () = 
  Term.Map.fold
    (fun x ys acc ->
       (x, Term.Set.elements ys) :: acc)
    (Context.partition s.current)
    []
 
(*s Equality/disequality test. *)

let is_equal a b =
  Shostak.is_equal s.current a b

let is_int a =
  match Context.cnstrnt s.current a with
    | Some(c) -> Cnstrnt.dom_of c = Dom.Int
    | None -> false
	

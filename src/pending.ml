
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

(*s Set of pending subgoals realized as a stack. *)

type 'a t = { mutable st : Atom.t list }

let pending = { st = [] }

let clear () =
  pending.st <- [] 

let push str a =
  Trace.msg 1 ("Push(" ^ str ^ ")") a Pretty.atom;
  pending.st <- a :: pending.st

let is_nonempty () = (pending.st <> [])

exception Empty

let pop () =
  match pending.st with
    | a :: al ->  
	Trace.msg 1 "Pop" a Pretty.atom;
	pending.st <- al;
	a
    | [] -> 
	raise Empty

let top () =
  match pending.st with
    | a :: al -> a
    | [] -> raise Empty

let length () = List.length pending.st

let iter f = List.iter f pending.st

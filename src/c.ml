
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
(*i*)

type t = Number.t Term.map
 
let cnstrnt_of s = s

let empty = Ptmap.empty              
       
let apply s a = Ptmap.find a s

let mem s a = Ptmap.mem a s

let propagate ((a,b) as e) s =  
  Trace.msg 3 "Prop(c)" e Pretty.eqn;
  try
    let ca = Ptmap.find a s in
    try
      let cb = Ptmap.find b s in
      match Number.cmp ca cb with
	| (Binrel.Same | Binrel.Super) ->
	    Ptmap.remove a s
	| Binrel.Sub -> 
	    Ptmap.add b ca s
	| Binrel.Disjoint ->
	    raise Exc.Inconsistent
	| Binrel.Overlap ->
	    let cab = Number.inter ca cb in
	    match Number.d_singleton cab with
	      | Some(u) ->
		  let q = Linarith.mk_num u in
		  Pending.push "c" (Atom.mk_equal b q);
		  Ptmap.remove a (Ptmap.remove b s)
	      | None ->
		  Ptmap.add b cab (Ptmap.remove a s)
    with
	Not_found ->
	  Ptmap.add b ca (Ptmap.remove a s)
  with
      Not_found ->
	s

(*s Adding a positive constraint [x in c] *)

let process c a s =
  Trace.msg 3 "Proc(c)" (a,c) Pretty.inn;
  try
    let d = Ptmap.find a s in
    match Number.cmp c d with 
      | (Binrel.Same | Binrel.Super) -> 
	  s
      | Binrel.Sub ->
	  Ptmap.add a c s
      | Binrel.Disjoint ->
	  raise Exc.Inconsistent
      | Binrel.Overlap ->
	  let cd = Number.inter c d in
	  (match Number.analyze cd with
	     | Status.Singleton(u) ->    
		 let q = Linarith.mk_num u in
                 Pending.push "c" (Atom.mk_equal a q);
		 Ptmap.remove a s
	     | _ -> 
		 Ptmap.add a cd s)
  with
      Not_found ->
	Ptmap.add a c s

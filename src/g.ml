(*
 * The contents of this file are subject to the ICS(TM) Community Research
 * License Version 2.0 (the ``License''); you may not use this file except in
 * compliance with the License. You may obtain a copy of the License at
 * http://www.icansolve.com/license.html.  Software distributed under the
 * License is distributed on an ``AS IS'' basis, WITHOUT WARRANTY OF ANY
 * KIND, either express or implied. See the License for the specific language
 * governing rights and limitations under the License.  The Licensed Software
 * is Copyright (c) SRI International 2003, 2004.  All rights reserved.
 * ``ICS'' is a trademark of SRI International, a California nonprofit public
 * benefit corporation.
 *)

exception Empty

module Infsys = struct

  let g = Stacks.create ()

  let is_empty () = 
    Stacks.is_empty g

  let rec pp fmt () = 
    let l = Stacks.to_list g in
      Pretty.set pp_atom fmt l

  and pp_atom fmt a = a#pp fmt 

  let reset () = 
    Stacks.clear g
	 
  let initialize =
    let push fct = Stacks.push fct g in
      Trace.func 2 "G.initialize" (Pretty.set pp_atom) Pretty.unit
	(fun fcts ->
	   reset ();
	   List.iter push fcts)

  let get =
    Trace.func 2 "G.get" Pretty.unit pp_atom
      (fun () -> 
	 try Stacks.pop g with Stacks.Empty -> raise Empty)

      
  let put = 
    Trace.proc 2 "G.put" pp_atom
      (fun fct -> 
	 Stacks.push fct g)
 
end



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
open Term
open Binrel
(*i*)

type t = Number.t Map.t

let pp fmt s = 
  Pretty.map Number.pp fmt s
 
let cnstrnt_of s = s

let empty = Map.empty              
       
let apply s a = Map.find a s

let mem s a = Map.mem a s

let rec merge (a,b) s = 
  Trace.call 5 "Merge(c)" (a,b) Pretty.eqn;
  let (s',al') = merge1 (a,b) s in
  Trace.exit 5 "Merge(c)" al' (Pretty.list Pretty.atom);
  (s', al')

and merge1 (a,b) s =
  let (a,b) = V.orient (a,b) in
  try
    let ca = match Cnstrnt.of_term a with
      | Some(c) -> Number.inter (Map.find a s) c
      | None -> (Map.find a s)
    in
    try
      let cb = match Cnstrnt.of_term b with
	| Some(c) -> Number.inter (Map.find b s) c
	| None -> (Map.find b s)
      in
      match Number.cmp ca cb with
	| (Same | Super) ->
	    (Map.remove a s, [])
	| Sub -> 
	    (Map.add b ca s, [])
	| Disjoint ->
	    raise Exc.Inconsistent
	| Overlap ->
	    let cab = Number.inter ca cb in
	    match Number.d_singleton cab with
	      | Some(u) ->
		  let e' = Atom.mk_equal b (Arith.mk_num u) in
		  let s' = Map.remove a s in
		  (s', [e'])
	      | None ->
		  let s' = Map.add b cab (Map.remove a s) in
		  (s', [])
    with
	Not_found ->
	  (match Cnstrnt.of_term b with
	     | Some(cb) ->
		 (match Number.cmp ca cb with
		    | (Same | Super) ->
			(Map.remove a s, [])
		    | Sub -> 
			(Map.add b ca s, [])
		    | Disjoint ->
			raise Exc.Inconsistent
		    | Overlap ->
			let cab = Number.inter ca cb in
			match Number.d_singleton cab with
			  | Some(u) ->
			      let e' = Atom.mk_equal b (Arith.mk_num u) in
			      let s' = Map.remove a s in
			      (s', [e'])
			  | None -> 
			      let s' = Map.add b ca (Map.remove a s) in
			      (s', []))
	     | None -> 
		 (Map.add b ca (Map.remove a s), []))
  with
      Not_found ->
	(match Cnstrnt.of_term a, Cnstrnt.of_term b with
	   | Some(ca), Some(cb) ->
	       (match Number.cmp ca cb with
		  | (Same | Super) ->
		      (s, [])
		  | Sub ->
		      (Map.add b ca s, [])
		  | Disjoint ->
		      raise Exc.Inconsistent
		  | Overlap ->
		      let cab = Number.inter ca cb in
		      match Number.d_singleton cab with
			| Some(u) ->
			    (s, [Atom.mk_equal b (Arith.mk_num u)])
			| None ->
			    (Map.add b cab s, []))
	   | None, Some _ ->
	       (s, [])
	   | Some(ca), None ->
	       (Map.add b ca s, [])
	   | None, None ->
	       (s, []))

(*s Adding a positive constraint [x in c] *)

let add1 c a s =
  try
    let d = Map.find a s in
    match Number.cmp c d with 
      | (Same | Super) -> 
	  (s, [])
      | Sub ->
	  (Map.add a c s, [])
      | Disjoint ->
	  raise Exc.Inconsistent
      | Overlap ->
	  let cd = Number.inter c d in
	  (match Number.d_singleton cd with
	     | Some(u) ->                 (* do not remove constraint. *)
		 let e' = Atom.mk_equal a (Arith.mk_num u) in
		 let s' = Map.add a cd s in
		 (s', [e'])
	     | _ -> 
		 (Map.add a cd s, []))
  with
      Not_found ->
	(Map.add a c s, [])

let add c a s =
  Trace.call 5 "Add(c)" (a,c) Pretty.inn;
  let (s',al') = add1 c a s in
  Trace.exit 5 "Add(c)" al' (Pretty.list Pretty.atom);
  (s', al')


(*s Instantiation. *)

let inst f s =
  Map.fold
    (fun x c acc ->
       let x' = f x in
       if Term.eq x' x then
	 acc
       else 
	 Map.add x' c acc)
    s
    s

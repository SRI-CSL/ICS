
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
(*i*)


type t = Cnstrnt.t Map.t

let domains s =  
  Map.fold (fun x i acc -> (x, i) :: acc) s []


let pp fmt s =
  Pretty.map Term.pp Cnstrnt.pp fmt (domains s)


let empty = Map.empty

let is_empty s = (s = Map.empty)

let mem = Map.mem

let apply s x = Map.find x s

let find s x =
  try
    Map.find x s
  with
      Not_found -> Cnstrnt.mk_real


  (*s Merging all equalities [x = y] in [veqs] by
   combining their respective constraints. New equalities
   might be deduced. *)
	
let rec merge eqs s = 
  Trace.call "c" "Merge" eqs (Pretty.list pp_equal);
  let (s', eqs', ch') = 
    List.fold_right
      (fun (x, y) (s, eqs, ch) ->
	 let (s', ch', eq) = merge1 x y (s, ch) in
	 match eq with
	   | None -> (s', eqs, ch')
	   | Some(z, q) -> (s', (z, q) :: eqs, ch'))
      eqs
      (s, [], Set.empty)
  in
  Trace.exit "c" "Merge" (eqs', ch')
    (fun fmt (eqs, ch) ->
        Pretty.list pp_equal fmt eqs;
        Pretty.string fmt " ";
        Pretty.set Term.pp fmt (Set.elements ch));
  (s', eqs', ch')

and merge1 x y (s, ch) = 
  let (x, y) = Term.orient (x, y) in
  try
    let i = apply s x in
    let (s', ch') = 
      if mem x s then
	(restrict x s, Set.add x ch)
      else 
	(s, ch)
    in
    try
      let j = apply s y in
      match Cnstrnt.cmp i j with
	| Binrel.Disjoint ->
	    raise Exc.Inconsistent
	| (Binrel.Same | Binrel.Super) ->
	    (s', ch', None)
	| Binrel.Sub ->
	    (update y i s', Set.add y ch', None)
	| Binrel.Singleton(q) -> (* retain singleton constraints. *)
	    (match Cnstrnt.d_singleton j with
	       | Some _ ->
		   (s', ch', None)
	       | None ->                  
		   (update y j s', Set.add y ch', Some(y, Arith.mk_num q)))
	| Binrel.Overlap(ij) ->
	    (update y ij s', Set.add y ch', None)
    with
	Not_found ->          
	  (update y i s', Set.add y ch', None)
  with
      Not_found ->
	(s, ch, None)

and restrict x =
  Trace.msg "c" "Restrict" x Term.pp;
  Map.remove x

and update x i =
  Trace.msg "c" "Add" (x, i) pp_in;
  Map.add x i


(*s [add x i s] add a new constraint [x in i] to [s]. *)

let add x i s =
  Trace.call "c" "Add" (x,i) pp_in;
  match Cnstrnt.status i with
    | Status.Empty ->
	raise Exc.Inconsistent
    | _ ->
	try
	  let j = apply s x in
	  match Cnstrnt.cmp i j with
	    | Binrel.Disjoint ->
		raise Exc.Inconsistent
	    | Binrel.Sub ->
		(update x i s, None)
	    | Binrel.Super | Binrel.Same ->
		(s, None)
	    | Binrel.Singleton(q) ->  (* retain singleton constraints. *)
		(match Cnstrnt.d_singleton j with
		   | Some _ -> (s, None)
		   | None -> (restrict x s, Some(x, Arith.mk_num q)))
	    | Binrel.Overlap(ij) ->
		(update x ij s, None)
	with
	    Not_found ->
	      (update x i s, None)


(*s Splitting predicates. *)

let split s =
  Term.Map.fold
    (fun x i acc ->
       if Cnstrnt.is_finite i then
	 (x, i) :: acc
       else 
	 acc)
    s []

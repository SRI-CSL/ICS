
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
 * Author: Harald Ruess, N. Shankar
i*)

(*i*)
open Term
open Three
(*i*)

type t = {
  e : Term.t Map.t;
  d : D.t;
}

(*s Accessors. *)

let partition s = 
  Map.fold
    (fun x y acc -> (* [y] is ``more'' canonical than [x]. *)
       try
	 let xs = Map.find x acc in
	 try
	   let ys = Map.find y acc in
	   Map.add y (Set.add y (Set.union xs ys)) (Map.remove x acc)
	 with
	     Not_found ->
	       Map.add y (Set.add y (Set.add x xs)) (Map.remove x acc)
       with
	   Not_found ->
	     try
	       let ys = Map.find y acc in
	       Map.add y (Set.add y (Set.add x ys)) acc
	     with
		 Not_found ->
		   Map.add y (Set.add y (Set.singleton x)) acc)
    s.e
    Map.empty

let diseqs s = D.deq_of s.d


(*s Canonical representative of a variable *)

let rec find s x = 
  try 
    let y = Map.find x s.e in
    if eq x y then y else find s y
  with 
      Not_found -> x


(*s Canonical representative with dynamic path compression. *)

let find' s x =
  let rec loop acc x =
    try
      let y = Map.find x s.e in
      if eq x y then
	(acc, y)
      else 
	loop (x :: acc)  y
    with
	Not_found -> (acc, x)
  in
  let (xl, y) = loop [] x in
  let e' = List.fold_right (fun x -> Map.add x y) xl s.e in
  let s' = {s with e = e'} in
  (s', y)


(*s Variable equality modulo [s]. *)

let eq s x y = 
  Term.eq (find s x) (find s y)

let deq s x y =
  D.is_diseq s.d (find s x) (find s y)

let is_equal s x y = 
  let x' = find s x and y' = find s y in
  if Term.eq x' y' then Yes
  else if D.is_diseq s.d x' y' then No
  else X


(*s The empty context. *)

let empty = {
  e = Map.empty;
  d = D.empty;
}

(*s Adding a binding [a |-> b] to a context [s]. *)

let merge e s =
  let (x, y) = Veq.destruct e in
  match is_equal s x y with
    | Yes -> s
    | No -> raise Exc.Inconsistent
    | X ->
	let (s', y') = find' s y in
	let e' = Map.add x y' s'.e in
	{s' with e = e'}


(*s Adding a disequality. *)

let diseq (x,y) s =
  match is_equal s x y with
    | Yes -> raise Exc.Inconsistent
    | No -> s
    | X -> {s with d = D.add (x, y) s.d}


(*s Pretty-printing. *)


let rec pp fmt s =
  epp fmt (partition s);
  D.pp fmt s.d

and epp fmt m =
  if not(Map.empty == m) then
    begin
      Pretty.string fmt "v:"; 
      let l = Map.fold (fun x ys acc -> (x, Set.elements ys) :: acc) m [] in
      Pretty.map Term.pp (Pretty.set Term.pp) fmt l;
      Pretty.string fmt "\n"
    end
 

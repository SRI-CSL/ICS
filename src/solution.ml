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
(*i*)

type t = {
    find: Term.t Map.t;
    inv : Term.t Map.t;
    use : Use.t;
    changed : Term.Set.t
}

let empty = {
  find = Map.empty;
  inv = Map.empty;
  use = Use.empty;
  changed = Set.empty
}

let is_empty s = 
  s.find == Map.empty


let eq s t =
  s.find == t.find


(*s Fold over the [find] structure. *)

let fold f s = Term.Map.fold f s.find


(*s Solution set *)

let to_list s =
  fold (fun x b acc -> (x, b) :: acc) s []

(*s Pretty-printer. *)

let pp fmt s =
  Pretty.list (Pretty.eqn Term.pp) fmt (to_list s) 

let apply s x =
  Map.find x s.find

let find s x = 
  try Map.find x s.find with Not_found -> x

let inv s a = Map.find a s.inv
    
let mem s x = 
  Map.mem x s.find
   
let use s = Use.find s.use
    
(*s Does a variable occur in [s]. *)

let occurs s x =
  mem s x || not (Term.Set.is_empty (use s x))


(*s [union x b s] adds new equality [x = b] to [s],
 possibly overwriting an equality [x = ...] *)

let union (x, b) s =  
  assert(is_var x);
  Trace.msg "s" "Union" (x, b) Term.pp_equal;
  if Term.eq x b then 
    s 
  else
    let use' = 
      try 
	Use.remove x (Map.find x s.find) s.use 
      with 
	  Not_found -> s.use 
    in
    {find = Map.add x b s.find;
     inv = Map.add b x s.inv;
     use = Use.add x b use';
     changed = Set.add x s.changed}


(*s Extend with binding [x = b], where [x] is fresh *)

let extend b s = 
  let x = Term.mk_fresh_var (Name.of_string "v") None in
  (x, union (x, b) s)

(*s Restrict domain. *)

let restrict =
  fun x s ->
    try
      let b = Map.find x s.find in  
      Trace.msg "s" "Restrict" x Term.pp;
      {find = Map.remove x s.find;
       inv = Map.remove b s.inv;
       use = Use.remove x b s.use;
       changed = Set.remove x s.changed}
    with
	Not_found -> s
	    
	    (*s [name e a] returns a variable [x] if there is
	      a solution [x = a]. Otherwise, it creates a new name
	      [x'] and installs a solution [x' = a] in [e]. *)

let name (b, s) =
  try
    (inv s b, s)
  with
      Not_found -> extend b s

(*s Fuse. *)

let rec fuse map (p, s) r =
  Trace.msg "s" "Fuse" r (Pretty.list Term.pp_equal);
  let norm = 
    map (fun x -> try Term.assq x r with Not_found -> x) 
  in
  let focus = 
    List.fold_right
      (fun (x, _) -> Set.union (use s x)) 
      r Set.empty
  in
  Set.fold 
    (fun x acc ->
       try
	 let b = norm (apply s x) in
	 update (x, b) acc
       with
	   Not_found -> acc)
    focus
    (p, s)

and update (x, b) (p, s) =
  assert(is_var x);
  Trace.msg "s" "Update" (x, b) Term.pp_equal;
  if Term.eq x b then
    (p, restrict x s)
  else if is_var b then
    let e' = Fact.mk_equal x b None in
    let p' = Partition.merge e' p in
    let s' = 
      try
	let a = apply s b in
	if b <<< x then
	  restrict x s
	else 
	  let s' = restrict b s in
	  union (x, a) s'
      with
	  Not_found -> 
	    restrict x s 
    in
    (p', s')
  else
    try
      let y = inv s b in
      let e' = Fact.mk_equal x y None in
      let p' = Partition.merge e' p in
      let s' = 
	if y <<< x then 
	  restrict x s 
	else 
	  let s' = restrict y s in
	  union (x, b) s'
      in
      (p', s')
    with
	Not_found ->
	  let s' = union (x, b) s in
	  (p, s')


(*s Composition. *)

let compose map (v, s) r =
  Trace.msg "s" "Compose" r (Pretty.list Term.pp_equal);
  let (v', s') = fuse map (v, s) r in
  List.fold_right update r (v', s')

       
(*s Changed set *)

let changed s = s.changed

let reset s = {s with changed = Set.empty}


(* Make sure that rhs are canonical. *)

let instantiate p =
  Set.fold 
    (fun x s -> 
       try
	 let b = apply s x in
	 let y = Partition.v p x in
	 union (y, b) (restrict x s)
       with
	   Not_found -> s)


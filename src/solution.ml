
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
    find: (Term.t * Fact.justification option) Map.t;
    inv : Term.t Map.t;
    use : Use.t
}

let empty = {
  find = Map.empty;
  inv = Map.empty;
  use = Use.empty
}


let is_empty s = 
  s.find == Map.empty

let eq s t =
  s.find == t.find

(*s Changed sets. *)

let changed = Theories.Array.create Set.empty

module Changed = struct
		  
  let reset () = Theories.Array.reset changed Set.empty
		   
  let restore = 
    Theories.Array.iter (Theories.Array.set changed) 
      
  let save () = Theories.Array.copy changed
		  
  let stable () =
    Theories.Array.for_all Set.is_empty changed

end


(*s Fold over the [find] structure. *)

let fold f s = Term.Map.fold f s.find


(*s Solution set *)

let to_list s =
  fold (fun x (b,_) acc -> (x, b) :: acc) s []

(*s Pretty-printer. *)

let pp i fmt s =
  Pretty.string fmt "\n";
  Theories.pp fmt i;
  Pretty.string fmt ":";
  Pretty.list (Pretty.eqn Term.pp) fmt (to_list s) 

let apply s x =
  match x with
    | App _ -> raise Not_found (* Invariant: only vars in domain of [s]. *)
    | _ -> fst(Map.find x s.find)

let find s x = 
  match x with
    | App _ -> x
    | _ -> (try fst(Map.find x s.find) with Not_found -> x)


let justification s x = 
  match x with
    | App _ -> raise Not_found
    | _ -> Map.find x s.find

let equality s x =
  let (a, prf) = justification s x in
    Fact.mk_equal x a prf



let inv s a = Map.find a s.inv
    
let mem s x = 
  Map.mem x s.find
   
let use s = Use.find s.use
    
(*s Does a variable occur in [s]. *)

let occurs s x =
  mem s x || not (Term.Set.is_empty (use s x))


(*s [union x b s] adds new equality [x = b] to [s],
 possibly overwriting an equality [x = ...] *)

let union i e s =  
  let (x, b, j) = Fact.d_equal e in
  assert(is_var x);
  Trace.msg (Theories.to_string i) "Update" (x, b) Term.pp_equal;
  if Term.eq x b then 
    s 
  else
    let use' = 
      try 
	Use.remove x (fst(Map.find x s.find)) s.use 
      with 
	  Not_found -> s.use 
    in
      Theories.Array.set changed i (Set.add x (Theories.Array.get changed i));
      {find = Map.add x (b,j) s.find;
       inv = Map.add b x s.inv;
       use = Use.add x b use'}


(*s Extend with binding [x = b], where [x] is fresh *)

let extend i b s = 
  let x = Term.mk_fresh_var (Name.of_string "s") None in
  let e = Fact.mk_equal x b None in
    Trace.msg (Theories.to_string i) "Extend" e Fact.pp_equal;
    (x, union i e s)

(*s Restrict domain. *)

let restrict i x s =
  try
    let b = fst(Map.find x s.find) in  
      Trace.msg (Theories.to_string i) "Restrict" x Term.pp;
      Theories.Array.set changed i (Set.remove x (Theories.Array.get changed i));
      {find = Map.remove x s.find;
       inv = Map.remove b s.inv;
       use = Use.remove x b s.use}
  with
      Not_found -> s
	    
	    (*s [name e a] returns a variable [x] if there is
	      a solution [x = a]. Otherwise, it creates a new name
	      [x'] and installs a solution [x' = a] in [e]. *)

let name i (b, s) =
  try
    (inv s b, s)
  with
      Not_found -> extend i b s

(*s Theory-specific normalization. *)

let rec map i =
  match i with
    | Theories.A -> Arith.map
    | Theories.T -> Tuple.map
    | Theories.BV -> Bitvector.map
    | Theories.S -> Coproduct.map
    | Theories.U -> mapu

and mapu ctxt a =
  match a with
    | Var _ -> ctxt(a)
    | App(f, l) ->  
	let l' = mapl ctxt l in
	  if l == l' then a else 
	    mk_app f l'

(*s Fuse. *)

let rec fuse i (p, s) r = 
  Trace.msg (Theories.to_string i) "Fuse" r (Pretty.list Fact.pp_equal);
  let norm = map i (fun x -> assoc x r) in
  Set.fold 
    (fun x acc ->
       try
	 let b = apply s x in
	 let e' = Fact.mk_equal x (norm b) None in
	 update i e' acc
       with
	   Not_found -> acc)
    (dom s r)
    (p, s)

and assoc x = function
  | [] -> x
  | e :: el -> 
      let (a, b, _) = Fact.d_equal e in
	if Term.eq x a then b else assoc x el
	  
and dom s r = 
  List.fold_right
    (fun e ->
       let (x, _, _) = Fact.d_equal e in
	 Set.union (use s x))
    r Set.empty

and update i e (p, s) =
  let (x, b, _) = Fact.d_equal e in
  assert(is_var x);
  if Term.eq x b then
    (p, restrict i x s)
  else if is_var b then
    let p' = Partition.merge e p in
    let s' = 
      try
	let a = apply s b in
	if b <<< x then
	  restrict i x s
	else 
	  let e' = Fact.mk_equal x a None in
	  let s' = restrict i b s in
	  union i e' s'
      with
	  Not_found -> 
	    restrict i x s 
    in
    (p', s')
  else
    try
      let y = inv s b in
	if Term.eq x y then (p, s) else 
	  let e' = Fact.mk_equal x y None in
	  let p' = Partition.merge e' p in
	  let s' = 
	    if y <<< x then 
	      restrict i x s 
	    else 
	      let s' = restrict i y s in
		union i e s'
	  in
	    (p', s')
    with
	Not_found ->
	  let s' = union i e s in
	    (p, s')


(*s Composition. *)

let compose i (p, s) r =
  Trace.msg (Theories.to_string i) "Compose" r (Pretty.list Fact.pp_equal);
  let (p', s') = fuse i (p, s) r in
  List.fold_right (update i) r (p', s')


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
    use : Use.t
}

let empty = {
  find = Map.empty;
  inv = Map.empty;
  use = Use.empty
}

let is_empty s = s.find = Map.empty

(*s Fold over the [find] structure. *)

let fold f s = Term.Map.fold f s.find

(*s Solution set *)

let solution s =
  fold (fun x b acc -> (x, b) :: acc) s []

(*s Pretty-printer. *)

let pp fmt s =
  Pretty.list (Pretty.eqn Term.pp) fmt (solution s) 

let apply s x =
  Map.find x s.find

let find s x = 
  try Map.find x s.find with Not_found -> x

let inv s a = Map.find a s.inv
    
let mem s x = 
  Map.mem x s.find
   
let use s = Use.find s.use

let useinter s a =
  let xs =
    Term.fold 
      (fun z acc -> 
	 match acc with 
	   | None -> Some(use s z)
	   | Some(xs) -> Some(Set.inter (use s z) xs))
      a None
  in
  match xs with None -> Set.empty | Some(xs) -> xs


(*s [setuse s x ys] sets the [use] of [x] to [ys]. *)

let adduse x ys s =
  {s with use = Use.set x ys s.use}
    
    
(*s Does a variable occur in [s]. *)

let occurs s x =
  mem s x || not (Term.Set.is_empty (use s x))


(*s [union x b s] adds new equality [x = b] to [s],
 possibly overwriting an equality [x = ...] *)

let union x b s = 
  assert(is_var x);
  let use' = 
    try 
      Use.remove x (Map.find x s.find) s.use 
    with 
	Not_found -> s.use 
  in
  {find = Map.add x b s.find;
   inv = Map.add b x s.inv;
   use = Use.add x b use'}


(*s Extend with binding [x = b], where [x] is fresh *)

let extend b s = 
  let x = Term.mk_fresh_var (Name.of_string "v") None in
  (x, union x b s)

(*s Restrict domain. *)

let restrict =
  fun x s ->
    try
      let b = Map.find x s.find in 
      {find = Map.remove x s.find;
       inv = Map.remove b s.inv;
       use = Use.remove x b s.use}
    with
	Not_found -> s

(*s [name e a] returns a variable [x] if there is
 a solution [x = a]. Otherwise, it creates a new name
 [x'] and installs a solution [x' = a] in [e]. *)

type status =
    | Name of Term.t
    | Fresh of Term.t * t

let name b s =
  try
    Name(inv s b)
  with
      Not_found ->
	let (x',s') = extend b s in
	Fresh(x',s')


(*s Lookup in a list of solved form modulo variable equalities [v]. *)

let lookup map v sl x =
  let x' = V.find v x in
  let rec scan = function
    | [] -> x'
    | (y, b) :: l ->
	if Term.eq x' (V.find v y) 
	then map (V.find v) b  (* make sure all variables are actually canonical *)
	else  scan l
  in
  scan sl

let norml map (v, sl) = 
  map (lookup map v sl)
	

(*s Fuse. *)

let rec fuse map (v, s, sl, focus) =
  Trace.msg "s" "Fuse" sl (Pretty.list Term.pp_equal);
  Set.fold
    (fun x (v, s, focus, ch) ->
       Trace.msg "s" "Fuse" x Term.pp;
       try
	 let b = apply s x in 
	 let b' = norml map (v, sl) b in
	 Trace.msg "s" "Fuse.insert" (x, b') Term.pp_equal;
	 insert (x, b') (v, s, focus, ch)
       with
	   Not_found -> (v, s, focus, ch))
    (relevant v s sl)
    (v, s, focus, Set.empty)

and insert (x, b) (v, s, focus, ch) =
 if is_var b then
   let e' = Fact.mk_equal x b None in
   let (v', focus') = V.merge e' v in
   let s' = restrict x s in
   (v', s', V.Focus.union focus' focus, ch)
 else 
   try
     let x' = inv s b in
     if Term.eq x x' then
       (v, s, focus, ch)
     else 
       let e' = Fact.mk_equal x' x None in
       let (v', focus') = V.merge e' v in
       let s' = restrict x s in
       (v', s', V.Focus.union focus' focus, ch)
   with
       Not_found ->
	 let s' = union x b s in
	 let ch' = Term.Set.add x ch in
	 (v, s', focus, ch')

and relevant v s sl = 
  List.fold_right
    (fun (x, _) -> Set.union (use s x))  
    sl 
    Set.empty


(*s Compose. *)

let compose map (v, s, sl, focus) =
  List.fold_right 
    (fun (x, b) ->
       let b' = map (V.find v) b in  (* make sure all variables are canonical. *)
       insert (x, b'))
    sl 
    (fuse map (v, s, sl, focus))

(*s Pretty-printing. *)

let pp fmt s =
  let l = fold (fun x a l -> (x,a)::l) s [] in
  Pretty.set (Pretty.eqn Term.pp) fmt l 


(*s Instantiate variables on rhs with canonical ones *)

let inst v s =
  fold
    (fun x b s ->
       let x' = V.find v x in
       if Term.eq x x' then 
	 s 
       else 
	 union x' b (restrict x s))
    s s
       

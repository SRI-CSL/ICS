
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

module type INTERP = sig
  val name : string  
  val fold : (Term.t -> 'a -> 'a) -> Term.t -> 'a -> 'a  
  val map : (Term.t -> Term.t) -> Term.t -> Term.t
end

module Make(Th: INTERP) = struct

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
    
(*s Solution set *)

  let solution s =
    Map.fold
      (fun x b acc ->
	 (x, b) :: acc)
      s.find
      []

(*s Pretty-printer. *)

  let pp fmt s =
    Pretty.list (Pretty.eqn Term.pp) fmt (solution s) 

  let ppinv fmt s = 
    let l = Map.fold (fun x b acc -> (x, b) :: acc) s.inv [] in
    Pretty.list (Pretty.eqn Term.pp) fmt l


  let is_empty s =
    s.find == Map.empty

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


(*s [union a b s] sets the find of [a] to [b]. *)

  let union =
    let msg = "Union("^Th.name^")" in
    fun a b s ->
      Trace.msg 2 msg (a,b) pp_equal;
      let use' = 
	try 
	  Use.remove Th.fold a (Map.find a s.find) s.use 
	with 
	    Not_found -> s.use 
      in
      {find = Map.add a b s.find;
       inv = Map.add b a s.inv;
       use = Use.add Th.fold a b use'}



(*s Normalization function applied to logical context. *)

  let norm s = 
    Th.map 
      (fun x -> 
	 try 
	   Map.find x s.find
	 with 
	     Not_found -> x)


(*s Normalization w.r.t. an association list. *)

  let norml sl a =
    Th.map
      (fun x -> 
	 try 
	   Term.assq x sl 
	 with 
	     Not_found -> x)
      a


(*s Restrict domain. *)

  let restrict =
    let msg = "Restrict("^Th.name^")" in
    fun x s ->
      try
	let b = Map.find x s.find in 
	Trace.msg 2 msg x Term.pp;
	{find = Map.remove x s.find;
	 inv = Map.remove b s.inv;
	 use = Use.remove Th.fold x b s.use}
      with
	  Not_found -> 
	    Trace.msg 10 msg (Term.to_string x ^ " not found ") Pretty.string;
            s


(*s Extend with binding [x = b], where [x] is fresh *)

 let extend = 
   let msg = "Extend("^Th.name^")" in
   fun b s ->
     let x = Term.mk_fresh_var (Name.of_string "v") None in
     (x, union x b s)


(*s Merging in a list of solved form [sl]. *)

 let rec compose s sl = 
   let rec compose1 (a, b) ((s, veqs) as acc) =
     if eq a b then
       acc
     else 
       Set.fold                        
	 (fun x ((s,_) as acc) -> 
	    compose1 (x, norml sl (find s x)) acc)
	 (use s a)
	 (step1 (a, b) acc)
   in
   List.fold_right compose1 sl (s, Veqs.empty)

 and step1 (a, b) ((s, veqs) as acc) = 
   assert(is_var a);
   if is_var b then               (* External equality. *)
     external1 (a, b) (s, veqs)
   else 
     try 
       let a' = inv s b in     
       if eq a a' then 
	 (s, veqs)
       else if Term.(<<<) a' a then 
	 (restrict a s, Veqs.add a a' veqs)
       else 
	 (union a b (restrict a' s), Veqs.add a a' veqs)
     with
	 Not_found ->
	   (union a b s, veqs)

 and external1 (x, y) (s, veqs) =
   let (x, y) = Term.orient (x, y) in   (* now: keep binding for [x]. *)
   let s' = 
     match mem s x, mem s y with
       | true, true -> restrict y s
       | true, false -> s
       | false, true -> union x (apply s y) s
       | false, false -> s
   and veqs' = Veqs.add x y veqs in
   (s', veqs')


(*s Propagate equalities [a = ...] for [a] a parameter only occuring lhs. *)

(* to do
 let rec propagate s sl = 
   let rec propagate1 (a, b) ((s, veqs) as acc) =
     if eq a b then
       acc
     else 
       Set.fold                        
	 (fun x ((s,_) as acc) -> 
	    propagate1 (x, norml sl (find s x)) acc)
	 (use s a)
	 (step1 (a, b) acc)
   in
   List.fold_right compose1 sl (s, Veqs.empty)
 *)

(*s Instantiation of variables [x] in [s] with [f x]. *)

  let inst f s =
    Map.fold
      (fun x y acc ->
	 let x' = f x and y' = Th.map f y in
	 union x' y' acc) 
      s.find
      empty

 (*s Fold. *)

  let fold s f a e =
    Term.Set.fold
      (fun x acc ->
	 f x (find s x) acc)
      (use s a)
      e

end

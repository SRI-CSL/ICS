
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
    s.find = Map.empty

  let apply s x =
    Map.find x s.find

  let find s x = 
    try Map.find x s.find with Not_found -> x

  let inv s a = Map.find a s.inv
    
  let mem s x = 
    Map.mem x s.find
   
  let use s = Use.find s.use

(*s [setuse s x ys] sets the [use] of [x] to [ys]. *)

  let adduse x ys s =
    {s with use = Use.set x ys s.use}
    
    

(*s Does a variable occur in [s]. *)

  let occurs s x =
  mem s x || not (Term.Set.is_empty (use s x))


(*s [union a b s] sets the find of [a] to [b]. *)

  let union =
    fun a b s ->
      Trace.msg Th.name "Subst.Union" (a,b) pp_equal;
      let use' = 
	try 
	  Use.remove Th.fold a (Map.find a s.find) s.use 
	with 
	    Not_found -> s.use 
      in
      {find = Map.add a b s.find;
       inv = Map.add b a s.inv;
       use = Use.add Th.fold a b use'}


(*s [union'] is the version internal to this module
 for accumulating the change set of the domain of [find].
 This is called only within [propagate] and [compose]. *)

  let changed = ref Term.Set.empty

  let union' a b s =
    changed := Term.Set.add a !changed;
    union a b s

    
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
    fun x s ->
      try
	let b = Map.find x s.find in 
	Trace.msg Th.name "Subst.Restrict" x Term.pp;
	{find = Map.remove x s.find;
	 inv = Map.remove b s.inv;
	 use = Use.remove Th.fold x b s.use}
      with
	  Not_found -> 
            s

(*s Extend with binding [x = b], where [x] is fresh *)

 let extend b s = 
   let x = Term.mk_fresh_var (Name.of_string "v") None in
   Trace.msg Th.name "Subst.Extend" (x, b) Term.pp_equal;
   Atom.footprint "def" [] [Atom.mk_equal x b];
   (x, union x b s)

(*s Merging in a list of solved form [sl]. *)

 let rec compose s sl = 
   let rec compose1 (x, b) ((s, veqs) as acc) =
     if eq x b then
       acc
     else 
       Set.fold                        
	 (fun y ((s,_) as acc) -> 
	    let e = (y, norml sl (find s y)) in
	    compose1 e acc)
	 (use s x)
	 (step1 (x, b) acc)
   in
   changed := Set.empty;
   let (s', veqs') = List.fold_right compose1 sl (s, Veqs.empty) in
   (s', veqs', !changed)

 and step1 (a, b) ((s, veqs) as acc) =
   assert(is_var a);
   if eq a b then acc else
     if is_var b then               (* External equality. *)
       external1 (a, b) (s, veqs)
     else 
       try 
	 let a' = inv s b in    
	 let s' = 
	   if Term.(<<<) a' a then 
	     restrict a s
	   else 
	     union' a b (restrict a' s)
	 in
	 (s', Veqs.add (Veq.make a a') veqs)
       with
	   Not_found ->
	     (union' a b s, veqs)

 and external1 (x, y) (s, veqs) =
   let (x, y) = Term.orient (x, y) in   (* eliminate binding for [x]. *)
   let s' = restrict x s in
   let veqs' = Veqs.add (Veq.make x y) veqs in
   (s', veqs')


(*s Propagate equalities [x = ...] for [a] a parameter only occuring lhs. *)

 let rec propagate s sl = 
   let propagate1 (x, b) acc =
     if eq x b then
       acc
     else 
       Set.fold           
	 (fun y (s, veqs)->
	    try 
	      let a' = norml [(x,b)] (apply s y) in
	      if is_var a' then
		let e' = Veq.make y a' in
		(restrict y s, Veqs.add e' veqs)
	      else
		try
		  let y' = inv s a' in
		  let e' = Veq.make y y' in
		  let veqs' = Veqs.add e' veqs in
                  if eq y y' then
		    (s, veqs)
		  else if Term.(<<<) y y' then      (* eliminate [y'] *)
		    (union' y a' (restrict y' s), veqs')
		  else
		    (restrict y s, veqs')
		with
		    Not_found ->
		      (union' y a' s, veqs)
	    with
		Not_found ->
		  (s, veqs))
	 (use s x)
	 acc
   in
   changed := Set.empty;
   let (s', veqs') = List.fold_right propagate1 sl (s, Veqs.empty) in
   (s', veqs', !changed)

(*s Instantiation of variables [x] in [s] with [f x]. *)

  let inst f s =
    Map.fold
      (fun x y acc ->
	 let x' = f x and y' = Th.map f y in
	 union x' y' acc) 
      s.find
      empty

 (*s Fold over the [find] structure. *)

  let fold f s = Term.Map.fold f s.find

end


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

(*s Congruence-closed logical context for equalities over
 uninterpreded function symbols and constants. Equalities are stored 
 in the form of a finite functions with bindings such as [f(x) |-> c]
 or [c |-> d]. As an invariant we have that we never have bindings of
 the form [c |-> f(.)], and the domain is subterm-closed in the sense
 that if [a] is in the domain and [b] is a subterm of [a] then [b] is also 
 in the domain. *)

(*i*)
open Hashcons
(*i*)

(*s The [find] structure consists of bindings [x |-> y]. Notice that
 [x |-> y, y |-> z] is not simplified to [x |-> z, y |-> z].  For
 every term [x] which is an immediate subterm of a term [f(...,x,...)]
 in the domain of [s], the set [use s x]  contains [f(...,x,...)]. The
 [inv] map is just the converse of [find], and is used to recurse
 over all terms with the same [find]; it is not needed for the core
 congruence closure algorithm. *)

type t = {
  mutable find : Term.t Term.map; 
  mutable use : Term.set Term.map
} 

let eq s t = 
  s.find == t.find && 
  s.use == t.use

let subst_of s = s.find
let use_of s = s.use

let empty = {
  find = Ptmap.empty; 
  use = Ptmap.empty
}

let copy s = {find = s.find; use = s.use}               
       
let apply s a = Ptmap.find a s.find

let mem s a = Ptmap.mem a s.find

let rec find s a =
  try
    let b = Ptmap.find a s.find in
    if a === b then a else find s b
  with
      Not_found -> a

let use s a =
  try
    Ptmap.find a s.use
  with
      Not_found -> Ptset.empty

(*s Pretty-printing. *)

let pp fmt s =
  Pretty.tmap fmt (subst_of s)


(*s Destructively add binding [a |-> b], and make sure
 that [a] is larger than [b].  For example, [f(x) |-> c]
 is added if [c] is a constant.  Also, the use structures
 are updated so that [use s a] is always a subset 
 of [use s (find s a)] *)

let rec union s ((a,b) as e) =
  s.find <- Ptmap.add a b s.find;
  let ub = use s b in    
  let uab = Ptset.union (use s a) ub in
  if not(uab == ub) then
    s.use <- Ptmap.add b uab s.use;
  Pending.push "u" (Atom.mk_equal a b)


let add_use s a b =                (* add [a] to the use of [b] *)
  let useb = use s b in
  let useb' = Ptset.add a useb in
  if not(useb == useb') then 
    s.use <- Ptmap.add b useb' s.use


(*s [can s a] returns the canonical representative of [a] w.r.t to [s]. 
    In addition, [canm s a] modifies the [use] structures to make it
    subterm-closed w.r.t. to [a]. *)

let modify = ref false  (* determines if [can] is modifying [use] structure. *)

let rec can s a =
  modify := false;
  canon s a

and canm s a=
  modify := true;
  canon s a

and canon s a =
  let f,l = Term.destruct a in
  match l with
    | [] -> find s a
    | _ -> 
	let l' =  Term.mapl (canon s) l in
	let a' = if l == l' then a else Term.make(f,l') in
        lookup s a'
	
and lookup s a =
  let f, l = Term.destruct a in
  let eq x y = x === find s y in
  try
    find s (Ptset.choose 
	      (fun b ->
		 let g, m = Term.destruct b in
		 (f === g) && 
		 try List.for_all2 eq l m with Invalid_argument _ -> false)
	      (use s (List.hd l)))
  with
      Not_found ->        
	begin
	  if !modify then     (* Make sure that domain of [s] is subterm-closed. *)
	    List.iter (add_use s a) l; 
	  a
	end

(*s Fold functional over the extension of a term [a]. *)

let fold s f e a =
  let b = can s a in
  Ptmap.fold 
    (fun x _ acc ->
       if can s x === b then f x acc else acc)
    s.find
    e

(*s Apply [f] for each term in equivalence class of [a]. *)

let iter s f =
  fold s (fun x _ -> f x) ()


(*s [a] and [b] are congruent w.r.t to [s] if [a]
    if of the form [f(a1,...,an)], [b] is of the form [f(b1,...,bn)],
    and all [ai] and [bi] are in the same equivalence class. *)

let congruent s a b =
  let eq x y = find s x === find s y in
  let f,l = Term.destruct a in
  let g,m = Term.destruct b in
  f === g &&
  try List.for_all2 eq l m with Invalid_argument _ -> false


(*s Merging of equalities [(a,b)] and congruence-close. *)

let merge s e =
  let rec loop ((a,b) as e) =
    if a =/= b then
      begin
	union s e;
	Ptset.iter
	  (fun x ->
	     Ptset.iter
	       (fun y ->
		  if congruent s x y then   
		    begin
		      loop (find s x, find s y);
		      add_use s x b
		    end)
	       (use s b))
	  (use s a)
      end
  in
  loop e


(*s Merging in a new equality [(a,b)]. *)

let rec process (a,b) s = 
  Trace.msg 3 "Process(u)" (a,b) Pretty.eqn;
  let t = copy s in 
  merge t (canm t a, canm t b);
  if eq s t then s else t


(*s Extending the domain. *)

let extend (x,b) s =
  Trace.msg 3 "Extend(u)" (x,b) Pretty.eqn;
  assert (not (mem s x));
  assert (Term.args_of x = []);  (* otherwise term universe is not closed. *)
  let t = copy s in
  union s (x, canm t b);
  if eq s t then s else t

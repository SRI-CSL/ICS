
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
open Sym
open Term
open Hashcons
open Mpa
open Status
(*i*) 

(*s Known disequalities; [x |-> {y,z}] is interpreted as [x <> y & x <> z].
  The function is closed in that forall [x], [y] such that [x |-> {...,y,...} ]
  then also [y |-> {....,x,....}] *)

type t = Term.set Term.map  

let empty = Ptmap.empty

let deq_of s = s

(*s Cheap equality test. *)

let eq s t = (s == t)
 
(*s All terms known to be disequal to [a]. *)

let deq s a =
  try Ptmap.find a s with Not_found -> Ptset.empty

(*s Check if two terms are known to be disequal. *)

let is_diseq s a b =
  Ptset.mem b (deq s a)

(*s Adding a disequality over uninterpreted terms. *)

let rec process (x,y) s =
  Trace.msg 2 "Process(d)" (x,y) Pretty.diseq;
  let xd = deq s x in
  let yd = deq s y in
  let xd' = Ptset.add y xd in
  let yd' = Ptset.add x yd in
  match xd == xd', yd == yd' with
    | true, true -> s
    | true, false -> add y yd' s
    | false, true -> add x xd' s
    | false, false -> add x xd' (add y yd' s)

and add x xd s =
  match Type.d_enumerative (Cnstrnt.of_term0 x) with
    | Some(ns) ->
	let ms = Ptset.fold 
		   (fun x acc -> 
		      match Enum.d_enum x with
			| Some(ns',n) when Name.Set.equal ns ns' ->
			    Name.Set.remove n acc
			| _ ->
			    acc)
		   xd ns
	in
	if Name.Set.cardinal ms = 1 then
	  begin
	    let y = Enum.mk_enum ns (Name.Set.choose ms) in
	    Pending.push "d" (Atom.mk_equal x y)
	  end;
	Ptmap.add x xd s
	  
    | _ ->
	Ptmap.add x xd s


(*s Propagating an equality between uninterpreted terms. *)

let rec propagate (a,b) s =
  Trace.msg 2 "Prop(c)" (a,b) Pretty.eqn;
  match Linarith.d_num b with
    | Some(q) ->
	propagate_num (a,q) s 
    | None ->
	(let da = deq s a and db = deq s b in
	 if Ptset.mem a db || Ptset.mem b da then
	   raise Exc.Inconsistent
	 else
	   let dab = Ptset.union da db in
	   if db == dab then
	     Ptmap.remove a s
	   else
	     add b (Ptset.union da db) (Ptmap.remove a s))
	      
and propagate_num (a,q) s =
  let c = Number.mk_diseq q in
  remove a c s        


(*s Removing disequalities for [a] by recursively 
 removing [a |-> {...,x,...}] and all [x |-> ...]. 
 Also infer constraints [a in ((-inf..q) | (q..inf))]. *)

and remove a c s =
  let da = deq s a in
  Ptset.fold 
    (fun x acc ->
       let acc' = Ptmap.remove x acc in
       Pending.push "d" (Atom.mk_in c x);
       acc')
    da 
    (Ptmap.remove a s)

and infer b =
  Trace.msg 4 "Infer(c)" b Pretty.atom;
  Pending.push "d" b

(*
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
 *)

open Mpa


(** Index for equalities [x = c] *)
module Cnstnt = struct 
  let th = Th.bv      
  let is_diseq a b =
    try
      let c = Bitvector.d_const a
      and d = Bitvector.d_const b in
	not(Bitv.equal c d)
    with
	Not_found -> false
  let is_const a = 
    try let _ = Bitvector.d_const a in true 
    with Not_found -> false
end

module Eqs =
  Eqs.MakeCnstnt(
    struct
      let th = Th.bv
      let nickname = Th.to_string Th.bv
      let map = Bitvector.map
      let is_infeasible (x, b) =
	match Bitvector.width x, Bitvector.width b with
	  | Some(n), Some(m) when n <> m -> true
	  | _ -> false
    end)
    (Cnstnt)

type t = {eqs : Eqs.t}
    
let eq s1 s2 = Eqs.eq s1.eqs s2.eqs

let empty = {eqs = Eqs.empty}

let is_empty s = Eqs.is_empty s.eqs

let pp fmt s = Eqs.pp fmt s.eqs

let apply s = Eqs.apply s.eqs
let find s = Eqs.find s.eqs
let inv s = Eqs.inv s.eqs
let dep s = Eqs.dep s.eqs

let fold f s = Eqs.fold f s.eqs

let constant s x =
  let (a, rho) = apply s x in
  let b = Bitvector.d_const a in
    (b, rho)

let is_dependent s = Eqs.is_dependent s.eqs
let is_independent s = Eqs.is_independent s.eqs

let copy s = { eqs = Eqs.copy s.eqs }

let name (p, s) = Eqs.name (p, s.eqs)

(** Set of bitvector constants. *)
module Bitvs = Set.Make(
  struct
    type t = Bitv.t * Jst.t
    let compare (b1, _) (b2, _) = Pervasives.compare b1 b2
  end)

let mem c bs =
  let dummy = Jst.dep0 in
    Bitvs.mem (c, dummy) bs

(** Collect all constants of length [n] disequal to [x]. *)
let  diseqs (p, s) (x, n) =
  let ys = Partition.diseqs p x in
    D.Set.fold
      (fun (y, rho) acc ->   (* [rho |- x <> y] *)
	 try                 (* [tau |- y = b] *)
	   let (b, tau) = constant s y in
	     if Bitv.length b = n then
	       Bitvs.add (b, Jst.dep2 rho tau) acc
	     else 
	       acc
	 with
	     Not_found -> acc)
      ys Bitvs.empty

(** [replace s a] substitutes dependent variables [x]
  in [a] with their right hand side [b] if [x = b] in [s].
  The result is canonized using {!Arith.map}. *)
let replace s =
  Jst.Eqtrans.replace Bitvector.map (find s)


let uninterp (p, s) =
  Jst.Eqtrans.compose 
    (Partition.find p)
    (Jst.Eqtrans.totalize (inv s))
  
(** Two bitvector terms [a], [b] are diseq if either
  - the interpreted terms [S[a]] and [S[b]] are disequal in the bitvector theory or
  - [S^-1(a)], [S^-1(b)] are variables disequal in the partition. *)
let rec is_diseq cfg =
  Jst.Pred2.orelse
    (is_diseq1 cfg)
    (is_diseq2 cfg)

and is_diseq1 (_, s) =
  Jst.Pred2.apply
    (replace s)
    (Jst.Pred2.inj Bitvector.is_diseq)

and is_diseq2 ((p, _) as cfg) =
  Jst.Pred2.apply
    (uninterp cfg)
    (Partition.is_diseq p)
    
let solve = Fact.Equal.equivn Bitvector.solve

let merge ((p, s) as cfg) e = 
  let e' = Fact.Equal.map (replace s) e in
    Trace.msg "bv" "Process" e' Fact.Equal.pp;
    Eqs.compose (p, s.eqs) (solve e')

let rec dismerge ((p, s) as cfg) d =  
  Trace.msg "bv" "Process" d Fact.Diseq.pp;
  let (x, y, _) = Fact.Diseq.destruct d in
    dismerge1 cfg x;
    dismerge1 cfg y

(** If [x] is interpreted over bitvectors of length [n], then
  - if there are [2^n] different bitvector constants of length [n]
    disequal to [x], then infer inconsistency,
  - if there are [2^n-1] different bitvector constant of length [n]
    disequal to [x], then generate the appropriate equality. 
  Following code has potential for optimization. *)
and dismerge1 ((p, s) as cfg) x =
  match Bitvector.width x with
    | None -> ()
    | Some(n) -> 
	let m = two_to_the_n_sub_one n in  
	let ds = Partition.diseqs p x in       (* quick failure. *)
	  if Z.lt (Z.of_int (D.Set.cardinal ds)) m then
	    ()
	  else
	    let cs = diseqs cfg (x, n) in
	    let card = Z.of_int (Bitvs.cardinal cs) in
	    let cmp = Z.compare card m in
	      if cmp < 0 then      (* [|cs| < 2^n-1]. *)
		()
	      else 
		let tau =
		  Bitvs.fold 
		    (fun (_, rho) -> Jst.dep2 rho) 
		    cs Jst.dep0 
		in
		  if cmp = 0 then (* [|cs| = 2^n-1]. *)
		    let b = choose_not_in n cs in
		    let e = Fact.Equal.make (x, Bitvector.mk_const b, tau) in
		      Fact.Eqs.push (Th.inj Th.bv) e;
		  else            (* [|cs| > 2^n-1]. *)
		    raise(Jst.Inconsistent(tau))

and two_to_the_n_sub_one n = 
  let ht = Hashtbl.create 4 in
  let _ = Tools.add_at_reset (fun () -> Hashtbl.clear ht) in
    try
      Hashtbl.find ht n 
    with
	Not_found ->
	  let m = Z.sub (Z.expt Z.two n) Z.one in
	    Hashtbl.add ht n m; m

(** Choose a bitvector of length [n] not in [cs]. *)    
and choose_not_in n cs =
  let max = Z.to_int (two_to_the_n_sub_one n) in
  let rec loop i = 
    assert(i >= 0);
    let d = Bitvector.nat2bitv n i in
      if mem d cs then
	loop (i - 1) 
      else 
	d
  in
    loop max
	

 
  
  
 

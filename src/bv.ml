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

module Bitvs = Set.Make(
  struct
    type t = Bitv.t * Jst.t
    let compare (b, _) (c, _) = 
      if Bitv.equal b c then 0 else Pervasives.compare b c
  end)

(** An {i interpretation} map associates each dependent bitvector
  variable [x] with a pair [(n, {b1,...,bn})], where [n] is the
  width of the bitvector, and [b1] are bitvector constants known
  to be disequal to [x]. *)
module Interp = struct
  type t = (int * Bitvs.t) Term.Map.t
  let empty = Term.Map.empty
  let to_list m = Term.Map.fold (fun x cod acc -> ((x, cod) :: acc)) m []
  let pp fmt m =
    let pp_bitv fmt (b, _) = Format.fprintf fmt "%s" (Bitv.to_string b) in
    let pp_set fmt s = Pretty.set pp_bitv fmt (Bitvs.elements s) in
    let to_list m = Term.Map.fold (fun x (n, b) acc -> (x, (n, b)) :: acc) m [] in
      Pretty.map Term.pp (Pretty.pair Pretty.number pp_set) fmt (to_list m)
  let eq m1 m2 = (m1 == m2)
  let add_diseq x (b, rho) m =
    try
      let (n, bs) = Term.Map.find x m in
	if n <> Bitv.length b then
	  let sigma = Jst.oracle "bv" [] in
	    raise(Jst.Inconsistent(sigma))
	else
	  let bs' = Bitvs.add (b, rho) bs in
	  let card = Z.of_int (Bitvs.cardinal bs') in
	  let max = Z.expt Z.two n in
	    if Z.ge card max then
	      let rhol = Bitvs.fold (fun (_, rho) acc -> rho :: acc) bs' [] in 
	      let phi = Jst.oracle "bv" rhol in
		raise(Jst.Inconsistent(phi))
	    else if Z.equal card (Z.sub max Z.one) then
	      m (* to do: generate possible equality *)
	    else 
	      Term.Map.add x (n, bs') m
    with
	Not_found ->
	  Term.Map.add x (Bitv.length b, Bitvs.singleton (b, rho)) m
end 

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
      let apply = Bitvector.apply
      let is_infeasible _ = false
    end)
    (Cnstnt)

type t = {eqs : Eqs.t; mutable interp : Interp.t}
    
let eq s1 s2 = Eqs.eq s1.eqs s2.eqs

let empty = {eqs = Eqs.empty; interp = Interp.empty}

let is_empty s = Eqs.is_empty s.eqs

let pp fmt s = Eqs.pp fmt s.eqs

let apply s = Eqs.apply s.eqs
let find s = Eqs.find s.eqs
let inv s = Eqs.inv s.eqs
let dep s = Eqs.dep s.eqs

let is_dependent s = Eqs.is_dependent s.eqs
let is_independent s = Eqs.is_independent s.eqs

let copy s = { eqs = Eqs.copy s.eqs; interp = s.interp }

let name (p, s) = Eqs.name (p, s.eqs)


(** [replace s a] substitutes dependent variables [x]
  in [a] with their right hand side [b] if [x = b] in [s].
  The result is canonized using {!Arith.map}. *)
let replace s =
  Fact.Equal.Inj.replace Bitvector.map
    (find s)

let solve = Fact.Equal.Inj.solver Th.bv Bitvector.solve

let process_equal ((p, s) as cfg) e =  
  let e' = Fact.Equal.map (replace s) e in
    Trace.msg "bv" "Process" e' Fact.Equal.pp;
    let sl = solve e' in
      Eqs.compose (p, s.eqs) sl

let rec process_diseq ((p, s) as cfg) d =  
  Trace.msg "bv" "Process" d Fact.Diseq.pp;
(*
  let (x, y, rho) = Fact.Diseq.destruct d in
    match const_of cfg x, const_of cfg y with
      | None, None -> ()
      | Some(b, tau), Some(c, sigma) ->  (* [tau |- x = b] *)
	  if Bitv.equal b c then         (* [sigma |- y = c] *)
	    let phi = Jst.oracle "bv" [rho; tau; sigma] in
	      raise(Jst.Inconsistent(phi))
      | Some(b, tau), None ->
	  let phi = Jst.oracle "bv" [rho; tau] in
	    s.interp <- Interp.add_diseq x (b, phi) s.interp
      | None, Some(c, sigma) ->
	  let phi = Jst.oracle "bv" [ rho; sigma] in
	    s.interp <- Interp.add_diseq y (c, phi) s.interp
*)

(* [const_of (p, s) x] returns [tau |- x = b] for [b] a bitvector constant. *)
and const_of (p, s) x =
  try
    let (a, rho) = apply s x in
      Some(Bitvector.d_const a, rho)
  with
      Not_found -> None
  
      
      

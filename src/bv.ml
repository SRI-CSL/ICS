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

(** Inference system for the bitvector theory {!Th.bv}. *)


open Mpa


(** Definition of a Shostak theory for bitvectors. Currently,
  no branching is performed. *)
module T: Shostak.T = struct
  let th = Th.bv
  let map = Bitvector.map
  let solve e = 
    let (a, b, rho) = Fact.Equal.destruct e in
      try
	let sl = Bitvector.solve (a, b) in
	let inj (a, b) = Fact.Equal.make (a, b, rho) in
	  List.map inj sl
      with
	  Exc.Inconsistent -> raise(Jst.Inconsistent(rho))
  let disjunction _ = raise Not_found
end


module G = Fact.Input
module E = Shostak.E(T)
module P = Partition


(** Inference system as a variant of Shostak inference systems. *)
module Infsys0: (Infsys.IS with type e = E.t) = struct

  type e = E.t
  
  module I = Shostak.Make(T)

  let ths = I.ths
  let abstract = I.abstract
  let merge = I.merge
  let dismerge = I.dismerge
  let propagate = I.propagate
  let branch = I.branch
  let normalize = I.normalize


  (** If [x] is interpreted over bitvectors of length [n], then
    - if there are [2^n] different bitvector constants of length [n]
    disequal to [x], then infer inconsistency,
    - if there are [2^n-1] different bitvector constant of length [n]
    disequal to [x], then generate the appropriate equality. 
    Following code has potential for optimization. *)
  let propagate_diseq = 
    I.propagate_diseq

  (** [is_diseq (p, s) a b] iff adding [a = b] to [(p, s)] yields inconsistency. *)
  let is_diseq (p, s) a b =
    try
      let e = Fact.Equal.make (a, b, Jst.dep0) in
      let _ = merge Th.bv e (G.empty, p, s) in
	None
    with
	Jst.Inconsistent(sigma) -> Some(sigma)

  (** Check if [a] is disequal from the bitvector constant [b]
    with unsigned interpretation [n]. *)
  let is_const_diseq (p, s) a n =
    match Bitvector.width a with
      | Some(l) ->
	  let b = Bitvector.mk_const (Bitvector.nat2bitv l n) in
	    is_diseq (p, s) a b
      | None ->
	  None

  let rec process_diseq (p, s) d =
    assert(Fact.Diseq.is_var d);
    let to_option f a = try Some(f a) with Not_found -> None in
    let d = Fact.Diseq.map (E.can (p, s)) d in
    let (a, b, rho) = Fact.Diseq.destruct d in
      match to_option Bitvector.d_const a, to_option Bitvector.d_const b with
	| Some(c), Some(d) ->
	    if Bitv.equal c d then
	      raise(Jst.Inconsistent(rho))
	    else 
	      (p, s)
	| None, None ->
	    (p, s)
	| Some(c), None ->
	    let n = Bitvector.bitv2nat c in
	      process_const_diseq (p, s) (b, n, rho)
	| None, Some(d) ->
	    let m = Bitvector.bitv2nat d in
	      process_const_diseq (p, s) (a, m, rho)

  and two_to_the_n_sub_one n =
    let ht = Hashtbl.create 4 in
    let _ = Tools.add_at_reset (fun () -> Hashtbl.clear ht) in
      try
	Hashtbl.find ht n
      with
          Not_found ->
            let m = Z.sub (Z.expt Z.two n) Z.one in
              Hashtbl.add ht n m; m


  and process_const_diseq (p, s) (a, n, rho) =
    assert(n >= 0);
    match Bitvector.width a with
      | None ->	
	  (p, s)
      | Some(l) ->
	  let lo = Mpa.Z.zero
	  and hi = two_to_the_n_sub_one l in
	    (p, s)


end

(** Tracing inference system. *)
module Infsys: (Infsys.IS with type e = E.t) =
  Infsys.Trace(Infsys0)
    (struct
       type t = E.t
       let level = "bv"
       let eq = E.S.eq
       let diff = E.S.diff
       let pp = E.S.pp
     end)

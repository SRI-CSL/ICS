(*
 * The contents of this file are subject to the ICS(TM) Community Research
 * License Version 2.0 (the ``License''); you may not use this file except in
 * compliance with the License. You may obtain a copy of the License at
 * http://www.icansolve.com/license.html.  Software distributed under the
 * License is distributed on an ``AS IS'' basis, WITHOUT WARRANTY OF ANY
 * KIND, either express or implied. See the License for the specific language
 * governing rights and limitations under the License.  The Licensed Software
 * is Copyright (c) SRI International 2003, 2004  All rights reserved.
 * ``ICS'' is a trademark of SRI International, a California nonprofit public
 * benefit corporation.
 *)

module type SIG = sig
  val th : Theory.t
  val f : Funsym.t
end 

(** Term manipulations for pure AC terms. *)
module T(Sig: SIG) = struct

  let th = Sig.th

  let is_f = Funsym.eq Sig.f

  let is_interp t = 
    try
      let f = Term.sym_of t and a = Term.args_of t in
	is_f f && Term.Args.length a = 2
    with
	Not_found -> false

  let lhs t =
    assert(is_interp t);
    Term.arg1 t

  let rhs t = 
    assert(is_interp t);
    Term.arg2 t

  let rec is_pure t = 
    (Term.is_var t) ||
    try
      is_f (Term.sym_of t) &&
      is_pure (lhs t) &&
      is_pure (rhs t)
    with
	Not_found -> false

  (** Ordered right-associative applications of AC symbol [f] *)
  let rec make s t =
    try
      let s1 = lhs s and s2 = rhs s in
	make s1 (make s2 t)
    with
	Not_found -> 
	  try
	    let t1 = lhs t and t2 = rhs t in
	    let cmp = Term.compare s t1 in
	      if le s t1  then   (* case [a <= b1] *)
		mk_app s t
	      else               (* case [a > b1] *)
		make t1 (make s t2)
	  with
	      Not_found -> 
		if le s t then mk_app s t else mk_app t s

  and mk_app s t =
    Term.mk_binary Sig.f s t

  and le s t = 
    Term.compare s t <= 0

  let sigma f a =
    assert(is_f f);
    assert(Term.Args.length a = 2);
    make (Term.Args.get a 0) (Term.Args.get a 1)

  let can = sigma

  let pp fmt f al =
    assert(is_f f);
    let rec infix = function
      | [] -> ()
      | [a] -> Term.pp fmt a
      | a :: al -> Term.pp fmt a; Funsym.pp fmt f; infix al
    in
      Format.fprintf fmt "(";
      infix al;
      Format.fprintf fmt ")"
      

  (** Apply [f] to uninterpreted positions. *)
  let map f = 
    let rec mapf a = 
      try
	let a1 = lhs a and a2 = rhs a in
	let b1 = mapf a1 and b2 = mapf a2 in
	  if a1 == b1 && a2 == b2 then a else make b1 b2
      with
	  Not_found -> f a
    in
      mapf

  let is_diseq _ _ = false

  open Axioms

  (** [x * (y * z) = (x * y) * z]. *)
  let chains = 
    let x = Lterm.Var (Name.of_string "x") in
    let y = Lterm.Var (Name.of_string "y") in
    let z = Lterm.Var (Name.of_string "z") in
    let mk_lapp a b = Lterm.App(Sig.f, [a; b]) in
      [Chain.mk_equal 
	 (Name.of_string "A")
	 []
	 (mk_lapp x (mk_lapp y z)) 
	 (mk_lapp (mk_lapp x y) z)]

  let disjunction _ = 
    raise Not_found
	 
end 

(** Inference system for AC theories. *)
module Make(Sig: SIG): Can.COMPONENT =
  Can.Make(T(Sig))





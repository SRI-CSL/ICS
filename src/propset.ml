(*
 * The contents of this file are subject to the ICS(TM) Community Research
 * License Version 2.0 (the ``License''); you may not use this file except in
 * compliance with the License. You may obtain a copy of the License at
 * http://www.icansolve.com/license.html.  Software distributed under the
 * License is distributed on an ``AS IS'' basis, WITHOUT WARRANTY OF ANY
 * KIND, either express or implied. See the License for the specific language
 * governing rights and limitations under the License.  The Licensed Software
 * is Copyright (c) SRI International 2003, 2004.  All rights reserved.
 * ``ICS'' is a trademark of SRI International, a California nonprofit public
 * benefit corporation.
 *)


(** Theory definition. *)
let theory = Theory.create "set"

let is_theory = Theory.eq theory

let _ = 
  Theory.Description.add theory
    "Theory of propositional sets"


(** Interpreted operations. *)
module Sig = struct
  let th = theory
  type t = Empty | Full | Ite
  let name =
    let empty = Name.of_string "empty"
    and full = Name.of_string "full"
    and ite = Name.of_string "ite" in
      function 
	| Empty -> empty
	| Full -> full
	| Ite -> ite
end

module Op = Funsym.Make(Sig)

let op a = Op.out (Term.sym_of a)
let args a = Term.args_of a

let is_interp a =
  try Op.is_interp (Term.sym_of a) with Not_found -> false

let is_empty t =
  try 
    (match op t with Sig.Empty  -> true | _ -> false) 
  with
      Not_found -> false
   
let is_full t =
  try
    (match op t with Sig.Full -> true | _ -> false)
  with
      Not_found -> false

let is_ite t = 
 try
    (match op t with 
       | Sig.Ite -> assert(Term.Args.length (Term.args_of t) = 3); true
       | _ -> false)
  with
      Not_found -> false

let d_ite i t = 
  assert(0 <= i && i <= 2);
  match op t with
    | Sig.Ite -> Term.Args.get (Term.args_of t) i
    | _ -> raise Not_found

let d_cond = d_ite 0
let d_pos = d_ite 1
let d_neg = d_ite 2

let rec is_pure t =
  try
    (match op t with   
       | (Sig.Empty | Sig.Full) ->  
	   true
       | Sig.Ite -> 
	   Term.is_var (d_cond t) && 
	   is_pure (d_pos t) && is_pure (d_neg t))
  with
      Not_found ->  Term.is_var t


let is_const t = is_empty t || is_full t

let mk_empty () = Term.mk_const (Op.inj(Sig.Empty))

let mk_full () = Term.mk_const (Op.inj(Sig.Full))

let d_ite t = 
  match op t with
    | Sig.Ite -> (d_cond t, d_pos t, d_neg t)
    | _ -> raise Not_found

module Bdd = Bdds.Make(
  struct
    type t = Term.t
    let eq = Term.eq
    let compare = Term.fast_compare
    let mk_zero = Term.mk_const (Op.inj(Sig.Empty))
    let mk_one = Term.mk_const (Op.inj(Sig.Full))
    let mk_ite = 
      let ite = Op.inj(Sig.Ite) in
	fun a -> 
	  assert(Term.Args.length a = 3);
	  Term.mk_app ite a
    let is_ite = is_ite
    let d_ite = Term.args_of
    module Args = struct
      type triple = Term.Args.t
      let eq = Term.Args.eq
      let make = Term.Args.make3
      let hash = Term.Args.hash
      let get = Term.Args.get
      let set = Term.Args.set
    end 
    let mk_fresh () = Term.mk_fresh_var "s"
    module Subst = struct
      type map = Term.Subst.t
      let empty = Term.Subst.empty
      let compose = Term.Subst.compose
    end
  end) 

let mk_ite a b c = Bdd.drop (Bdd.mk_ite (Bdd.lift a) (Bdd.lift b) (Bdd.lift c))
let mk_inter a b = Bdd.drop (Bdd.mk_conj (Bdd.lift a) (Bdd.lift b))
let mk_union a b = Bdd.drop (Bdd.mk_disj (Bdd.lift a) (Bdd.lift b))
let mk_imp a b = Bdd.drop (Bdd.mk_imp (Bdd.lift a) (Bdd.lift b))
let mk_equiv a b = Bdd.drop (Bdd.mk_equiv (Bdd.lift a) (Bdd.lift b))
let mk_compl a = Bdd.drop (Bdd.mk_neg (Bdd.lift a))

(** Mapping over bitvector terms. *)
let map f =
  let rec mapf t = 
    if is_empty t || is_full t then t else
      if is_ite t then 
	let t1 = d_cond t and t2 = d_pos t and t3 = d_neg t in
	let t1' = mapf t1 and t2' = mapf t2 and t3' = mapf t3 in
	  if t1 == t1' && t2 == t2' && t3 == t3' then t else
	    mk_ite t1' t2' t3'
      else 
	f t
  in
    mapf

(** Replacing a variable with a term. *)
let apply (x, b) =
  let lookup y = if Term.eq x y then b else y in
    map lookup

(** Canonized set expressions. *)
let sigma f a =
  assert(Op.is_interp f);
  match Op.out f with
    | Sig.Empty -> mk_empty ()
    | Sig.Full -> mk_full ()
    | Sig.Ite -> mk_ite (Term.Args.get a 0) (Term.Args.get a 1) (Term.Args.get a 2)

let solve t1 t2 =
  let b = Bdd.lift (mk_equiv t1 t2) in
    assert(Bdd.is_bdd b);
    try 
      let xl, sl = Bdd.solve b in
	failwith "to do", sl
    with 
	Bdd.Inconsistent -> raise Exc.Inconsistent
	  
let is_diseq t1 t2 =
  Bdd.is_diseq (Bdd.lift t1) (Bdd.lift t2)

let _ = 
  let m = Term.Methods.empty() in
    m.Term.Methods.can <- Some(sigma);
    m.Term.Methods.is_diseq <- Some(is_diseq);
    m.Term.Methods.solve <- Some(fun t1 t2 -> snd(solve t1 t2));
    Term.Methods.register theory m
    
module T = struct
  let th = theory
  let can = sigma
  let solve = solve
  let disjunction _ = raise Not_found
end
  
module Component = Shostak.Make(T)

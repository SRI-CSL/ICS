(*
 * The contents of this file are subject to the ICS(TM) Community Research
 * License Version 1.0 (the ``License''); you may not use this file except in
 * compliance with the License. You may obtain a copy of the License at
 * http://www.icansolve.com/license.html.  Software distributed under the
 * License is distributed on an ``AS IS'' basis, WITHOUT WARRANTY OF ANY
 * KIND, either express or implied. See the License for the specific language
 * governing rights and limitations under the License.  The Licensed Software
 * is Copyright (c) SRI International 2001, 2002.  All rights reserved.
 * ``ICS'' is a Jtrademark of SRI International, a California nonprofit public
 * benefit corporation.
 *)

(** {6 Global Variables} *)

module Mode = struct
  
  type t = No | Dep | Yes

  let to_string = function
    | No -> "no"
    | Yes -> "yes"
    | Dep -> "dep"

  let of_string = function
    | "no" -> No
    | "yes" -> Yes
    | "dep" -> Dep
    | str -> raise (Invalid_argument(str ^ " : no such proof mode"))
      
 end 

open Mode

let proofmode = ref Dep


(** {6 Proof Rules} *)

module Rule = struct

  type t =           
    | Refl of Term.t             
    | Sym of Term.t * Term.t
    | Trans of Term.t * Term.t * Term.t
    | Subst of Atom.t
    | Contradiction
    | All of Atom.t
    | Sigma of Term.t * Term.t
    | Dom of Term.t * Dom.t
    | Solve of Term.t * Term.t
    | Extend of Term.t * Term.t
    | Weaken of Term.t
    | Zero of Term.t
    | Nonzero of Term.t
    | Inter of Term.t * Dom.t
    | Groebner of Term.t * Term.t
    | Equal of Term.t * Term.t
    | Diseq of Term.t * Term.t
    | Abstract of Term.t * Term.t
    | Const of Term.t * Term.t

  let justifies = function
    | Refl(a) -> Atom.Equal(a, a)
    | Sym(a, b) -> Atom.Equal(a, b)
    | Trans(a, b, c) -> Atom.Equal (a, c)
    | Subst(atm) -> atm
    | Contradiction -> Atom.False
    | All(atm) -> atm
    | Sigma(a, b) -> Atom.Equal (a, b)
    | Dom(a, d) -> failwith "to do"
    | Solve(a, b) -> Atom.Equal (a, b)
    | Extend(a, b) -> Atom.Equal (a, b)
    | Weaken(a) -> Atom.Pos a
    | Zero(a) -> Atom.Equal (a, Arith.mk_zero)
    | Nonzero(a) -> Atom.Diseq (a, Arith.mk_zero)
    | Inter(a, d) -> failwith "to do"
    | Groebner(a, b) -> Atom.Equal (a, b)
    | Equal(a, b) -> Atom.Equal (a, b)
    | Diseq(a, b) -> Atom.Diseq (a, b)
    | Abstract(a, b) -> Atom.Equal (a, b)
    | Const(a, b) -> Atom.Diseq (a, b)


  let pp fmt prf =
    match prf with
      | Refl(a) ->
	  Format.fprintf fmt "Refl(%s)" (Term.to_string a)
      | Sym(a, b) ->
	  Format.fprintf fmt "Sym(%s, %s)" (Term.to_string a) (Term.to_string b)
      | Trans(a, b, c) ->
	  Format.fprintf fmt "Trans(%s, %s, %s)" (Term.to_string a) (Term.to_string b) (Term.to_string c)
      | Subst(atm) ->
	  Format.fprintf fmt "Subst(%s)" (Pretty.to_string Atom.pp atm)
      | Contradiction -> 
	  Format.fprintf fmt "Contradiction"
      | All(atm) ->
	  Format.fprintf fmt "All"
      | Sigma(a, b) ->
	  Format.fprintf fmt "Sigma(%s, %s)" (Term.to_string a) (Term.to_string b)
      | Dom(a, b) ->
	  Format.fprintf fmt "Dom(%s, %s)" (Term.to_string a) (Dom.to_string b)
      | Solve(a, b) ->
	  Format.fprintf fmt "Solve(%s, %s)" (Term.to_string a) (Term.to_string b)
      | Extend(a, b) -> 
	  Format.fprintf fmt "Extend(%s, %s)" (Term.to_string a) (Term.to_string b)
      | Weaken(a) -> 
	  Format.fprintf fmt "Weaken(%s)" (Term.to_string a)
      | Zero(a) -> 
	  Format.fprintf fmt "Zero(%s)" (Term.to_string a)
      | Nonzero(a) -> 
	  Format.fprintf fmt "Nonzero(%s)" (Term.to_string a)
      | Inter(a, d) ->
	  Format.fprintf fmt "Inter(%s, %s)" (Term.to_string a) (Dom.to_string d)
      | Groebner(a, b) ->
	  Format.fprintf fmt "Groebner(%s, %s)" (Term.to_string a) (Term.to_string b)
      | Equal(a, b) ->
	  Format.fprintf fmt "Equal(%s, %s)" (Term.to_string a) (Term.to_string b)
      | Diseq(a, b) ->
	  Format.fprintf fmt "Diseq(%s, %s)" (Term.to_string a) (Term.to_string b)
      | Abstract(a, b) ->
	  Format.fprintf fmt "Abstract(%s, %s)" (Term.to_string a) (Term.to_string b)
      | Const(a, b) ->
	  Format.fprintf fmt "Const(%s, %s)" (Term.to_string a) (Term.to_string b)

  let to_string = Pretty.to_string pp

end

(** {6 Proofs} *)

module Proof = struct

  type t = 
    | Axiom of Atom.t
    | Apply of Rule.t * t list

  let rec pp fmt = function
    | Axiom(atm) -> 
	Format.fprintf fmt "Axiom(%s)" (Pretty.to_string Atom.pp atm)
    | Apply(rl, []) ->
	Rule.pp fmt rl
    | Apply(rl, jl) ->
	Rule.pp fmt rl;
	Format.fprintf fmt "(";
	Pretty.infixl pp ", " fmt jl;
	Format.fprintf fmt ")"

  let axioms_of =
    let rec collect acc = function
      | Axiom(atm) -> Atom.Set.add atm acc
      | Apply(_, prfs) -> List.fold_left collect acc prfs
    in 
      collect Atom.Set.empty

end 
	

(** {6 Justifications} *)

type t =
  | Unjustified
  | Dependency of Atom.Set.t
  | Proof of Proof.t

let axioms_of = function
  | Unjustified -> raise Not_found
  | Dependency(axms) -> axms
  | Proof(prf) -> Proof.axioms_of prf

let proof_of = function
  | Proof(prf) -> prf
  | _ -> raise Not_found

let pp fmt j =
  match !proofmode with
    | No -> Format.fprintf fmt "Unjustified"
    | Dep -> Pretty.set Atom.pp fmt (Atom.Set.elements (axioms_of j))
    | Yes ->
	(match j with
	   | Unjustified -> Format.fprintf fmt "Unjustified"
	   | Dependency(axms) -> Pretty.set Atom.pp fmt (Atom.Set.elements axms)
	   | Proof(prf) -> Proof.pp fmt prf)

exception Inconsistent of t
exception Valid of t

let is_none = function
  | Unjustified -> true
  | _ -> false

(** {6 Constructors} *)

let mk_none () =
  match !proofmode with
    | No -> Unjustified
    | _ -> Format.eprintf "Warning: empty justification"; Unjustified

let mk_dependency0 = Dependency(Atom.Set.empty)

let mk_dependency1 j =
  match j with
    | Unjustified -> mk_none()
    | Dependency _ -> j
    | Proof(prf) -> Dependency(Proof.axioms_of prf)

let mk_dependency2 j1 j2 =
  try
    let axms1 = axioms_of j1 and axms2 = axioms_of j2 in
      Dependency(Atom.Set.union axms1 axms2)
  with
      Not_found -> mk_none()

let mk_dependency jl =
  try
    let axms = 
      List.fold_left 
	(fun acc j -> Atom.Set.union (axioms_of j) acc) 
	Atom.Set.empty jl
    in
      Dependency(axms)
  with
      Not_found -> mk_none()

let mk_axiom a = Proof(Proof.Axiom(a))

let mk_apply0 rl = Proof(Proof.Apply(rl, []))

let mk_apply1 rl j =
  try
    let prf = proof_of j in
      Proof(Proof.Apply(rl, [prf]))
  with
      Not_found -> mk_dependency1 j

let mk_apply2 rl j1 j2 =
  try
    let prf1 = proof_of j1
    and prf2 = proof_of j2 in
      Proof(Proof.Apply(rl, [prf1; prf2]))
  with
      Not_found -> mk_dependency2 j1 j2

let mk_apply rl jl =
  try
    let prfl = List.map proof_of jl in
      Proof(Proof.Apply(rl, prfl))
  with
      Not_found -> mk_dependency jl



(** {6 Proof Rules} *)

let axiom atm = 
  match !proofmode with
    | No -> mk_none()
    | Dep -> Dependency(Atom.Set.singleton atm)
    | Yes -> mk_axiom atm

let refl a = 
  match !proofmode with
    | No -> mk_none()
    | Dep -> mk_dependency0
    | Yes ->  mk_apply0 (Rule.Refl(a))

let is_refl = function
  | Proof(Proof.Apply(Rule.Refl _, _)) -> true
  | _ -> false

let full = ref false

let sym (a, b) j =
  if Term.eq a b then j else 
    match !proofmode with
      | No -> mk_none()
      | Dep -> mk_dependency1 j
      | Yes -> if !full then mk_apply1 (Rule.Sym(a, b)) j else j
	  
let trans (a, b, c) j1 j2 = 
  if Term.eq a b then j2
  else if Term.eq b c then j1
  else match !proofmode with
    | No -> mk_none()
    | Dep -> mk_dependency2 j1 j2
    | Yes -> mk_apply2 (Rule.Trans(a, b, c)) j1 j2

let apply (a, b) jl =
  match !proofmode with
    | No -> mk_none()
    | Dep -> mk_dependency jl
    | Yes -> mk_apply (Rule.Subst(Atom.Equal(a, b))) jl

let apply1 (a, b) j =
  match !proofmode with
    | No -> mk_none()
    | Dep -> mk_dependency1 j
    | Yes -> mk_apply1 (Rule.Subst(Atom.Equal(a, b))) j

let contradiction j1 j2 =
  match !proofmode with
    | No -> mk_none()
    | Dep -> mk_dependency2 j1 j2
    | Yes -> mk_apply2 Rule.Contradiction j1 j2

let contradiction_star jl =
  match !proofmode with
    | No -> mk_none()
    | Dep -> mk_dependency jl
    | Yes -> mk_apply Rule.Contradiction jl


let all atm j =
  match !proofmode with
    | No -> mk_none()
    | Dep -> mk_dependency1 j
    | Yes -> mk_apply1 (Rule.All(atm)) j

let solve (x, a) j =
  match !proofmode with
    | No -> mk_none()
    | Dep -> mk_dependency1 j
    | Yes -> mk_apply1 (Rule.Solve(x, a)) j

let sigma ((f, al), b) jl =
  match !proofmode with
    | No -> mk_none()
    | Dep -> mk_dependency jl
    | Yes -> 
	let a = Term.App(f, al) in
	  if Term.eq a b then refl a else 
	    mk_apply (Rule.Sigma(a, b)) jl
  
let dom (a, d) jl =
  match !proofmode with
    | No -> mk_none()
    | Dep -> mk_dependency jl
    | Yes -> mk_apply (Rule.Dom(a, d)) jl
  
let extend (x, a) =
  match !proofmode with
    | No -> mk_none()
    | Dep -> mk_dependency0
    | Yes -> mk_apply0 (Rule.Extend(x, a))

let slackify (k, a) j =
  match !proofmode with
    | No -> mk_none()
    | Dep -> mk_dependency1 j
    | Yes -> mk_apply1 (Rule.Extend(k, a)) j

let abstract (a, b) jl =
    match !proofmode with
      | No -> mk_none()
      | Dep -> mk_dependency jl
      | Yes -> if Term.eq a b then refl a else mk_apply (Rule.Abstract(a, b)) jl

let nonzero a j =
  match !proofmode with
    | No -> mk_none()
    | Dep -> mk_dependency1 j
    | Yes -> mk_apply1 (Rule.Nonzero(a)) j

let weaken a j =
  match !proofmode with
    | No -> mk_none()
    | Dep -> mk_dependency1 j
    | Yes -> mk_apply1 (Rule.Weaken(a)) j

let zero a j1 j2 =
  match !proofmode with
    | No -> mk_none()
    | Dep -> mk_dependency2 j1 j2
    | Yes -> mk_apply2 (Rule.Zero(a)) j1 j2

let inter (a, d) j1 j2 =
  match !proofmode with
    | No -> mk_none()
    | Dep -> mk_dependency2 j1 j2
    | Yes -> mk_apply2 (Rule.Inter(a, d)) j1 j2

let groebner (a, b) j =
  match !proofmode with
    | No -> mk_none()
    | Dep -> mk_dependency1 j
    | Yes -> mk_apply1 (Rule.Groebner(a, b)) j

let equal0 (a, b) = 
  match !proofmode with
    | No -> mk_none()
    | Dep -> mk_dependency0
    | Yes -> mk_apply0 (Rule.Equal(a, b))

let diseq0 (a, b) = 
  match !proofmode with
    | No -> mk_none()
    | Dep -> mk_dependency0
    | Yes -> mk_apply0 (Rule.Diseq(a, b))

let dom0 (a, d) = 
  match !proofmode with
    | No -> mk_none()
    | Dep -> mk_dependency0
    | Yes -> mk_apply0 (failwith "Dom0: to do")

let cnstrnt0 (a, sgn) = 
  match !proofmode with
    | No -> mk_none()
    | Dep -> mk_dependency0
    | Yes -> mk_apply0 (failwith "Dom0: to do")





let notsign (a, sgn) j = 
  match !proofmode with
    | No -> mk_none()
    | Dep -> mk_dependency1 j
    | Yes -> mk_apply1 (failwith "Notsign: to do") j

let const (a, b) j1 j2 =
  match !proofmode with
    | No -> mk_none()
    | Dep -> mk_dependency2 j1 j2
    | Yes -> mk_apply2 (Rule.Const(a, b)) j1 j2

let posint a j1 j2 =
  match !proofmode with
    | No -> mk_none()
    | Dep -> mk_dependency2 j1 j2
    | Yes -> mk_none ()


let dependencies jl = 
  match !proofmode with
    | No -> mk_none()
    | Dep -> mk_dependency jl
    | Yes -> 
	Format.eprintf "Warning: constructing dependencies only@?";
	mk_dependency jl

let dependencies0 = mk_dependency []
	

(** {6 Derived Rules} *)

let trans3 (a, b, c, d) j1 j2 j3 =
  if a == b then trans (b, c, d) j2 j3
  else if b == c then trans (a, c, d) j1 j3
  else if c == d then trans (a, b, c) j1 j2
  else trans (a, b, d) j1 (trans (b, c, d) j2 j3)

let subst_equal (a, b) j jl =
  match !proofmode with
    | No -> mk_none()
    | Dep -> mk_dependency (j :: jl)
    | Yes -> mk_apply (Rule.Subst(Atom.Equal(a, b))) (j :: jl)

let subst_diseq (a, b) j jl =
  match !proofmode with
    | No -> mk_none()
    | Dep -> mk_dependency (j :: jl)
    | Yes -> mk_apply (Rule.Subst(Atom.Diseq (a, b))) (j :: jl)

let subst_in (a, d) j jl =
  match !proofmode with
    | No -> mk_none()
    | Dep -> mk_dependency (j :: jl)
    | Yes -> failwith "to do"

let subst_nonneg a j jl =
  match !proofmode with
    | No -> mk_none()
    | Dep -> mk_dependency (j :: jl)
    | Yes -> mk_apply (Rule.Subst(Atom.Nonneg(a))) (j :: jl)

let subst_pos a j jl =
  match !proofmode with
    | No -> mk_none()
    | Dep -> mk_dependency (j :: jl)
    | Yes -> mk_apply (Rule.Subst(Atom.Pos a)) (j :: jl)

    

(** {6 Justifying Relations} *)

type just = t

module Three = struct

  type t =
    | Yes of just
    | No of just    
    | X

  (** Accumulate facts in global variable [justs]. *)
  let to_three justs p a b =
    match p a b with
      | Yes(rho) -> 
	  justs := rho :: !justs; 
	  Three.Yes
      | No(rho) -> 
	  justs := rho :: !justs; 
	  Three.No
      | X -> 
	  Three.X
end





(** {6 Equality Transformers} *)

module Eqtrans = struct

  type t = Term.t -> Term.t * just

  let acc hyps f a =
    let (b, rho) = f a in
      if !proofmode <> No && not(is_refl rho) then
	hyps := rho :: !hyps;
      b

  let id a = (a, refl a)

  let compose f g a =
    let (b, rho) = g a in           (* [rho |- a = b] *)
    let (c, tau) = f b in           (* [tau |- b = c] *)
    let sigma = trans (a, b, c) rho tau in
      (c, sigma)

  let compose3 f g h = (compose f (compose g h))

  let compose_partial1 f g a =
    try
      let (b, rho) = g a in
	(try
	  let (c, tau) = f b in
	  let sigma = trans (a, b, c) rho tau in
	    (c, sigma)
	with
	    exc -> 
	      (b, rho))
    with
	exc ->
	  failwith "Fatal error: partial function"

  let trace lvl name =
    Trace.func lvl name Term.pp (Pretty.pair Term.pp pp)

end



module Pred = struct

  type t = Term.t -> just option

  (** Test predicate [p] on [f(a)]. *)
  let apply f p a =
    let (b, rho) = f a in   (* [rho |- a = b] *)
      match p b with
	| (Some(tau) as res) ->      (* [tau |- p(b)] *)
	    if a == b then res else 
	      let sigma = dependencies [rho; tau] in
		Some(sigma)
	| None ->
	    None

  (** Disjunction. *)
  let disj p q a =
    match p a with
      | None -> q a
      | res -> res

end

module Pred2 = struct

  type t = Term.t -> Term.t -> just option

  (** Test predicate [p] on [f(a)]. *)
  let apply f p a b =
    let (a', alpha') = f a in   (* [alpha' |- a = a'] *)
    let (b', beta') = f b in    (* [beta' |- b = b'] *)
      match p a' b' with
	| (Some(tau) as res) ->      (* [tau |- p(b)] *)
	    if a == a' && b == b' then res else 
	      let sigma = dependencies [alpha'; beta'; tau] in
		Some(sigma)
	| None ->
	    None

end



module Relation = struct

  type 'a t = 'a -> Three.t


end

module Rel1 = struct

  type t = Term.t Relation.t

  (** Test binary term relation [r] on [f a] and [f b]. *)
  let apply f r a =
    let (a', alpha') = f a in
      match r a' with
	| (Three.Yes(tau) as res) ->
	    if a == a' then res else 
	      let sigma = dependencies [alpha'; tau] in
		Three.Yes(sigma)
	| (Three.No(tau) as res) ->
	    if a == a' then res else
	      let sigma = dependencies [alpha'; tau] in
		Three.No(sigma)
	| Three.X ->
	    Three.X

end 

module Rel2 = struct

  type t = Term.t -> Term.t -> Three.t

  (** Test binary term relation [r] on [f a] and [f b]. *)
  let apply f r a b =
    let (a', alpha') = f a 
    and (b', beta') = f b in
      match r a' b' with
	| Three.Yes(tau) ->
	    let sigma = dependencies [alpha'; beta'; tau] in
	      Three.Yes(sigma)
	| Three.No(tau) ->
	    let sigma = dependencies [alpha'; beta'; tau] in
	      Three.No(sigma)
	| Three.X ->
	    Three.X

  let orelse r s a b =
    match r a b with
      | Three.X -> s a b
      | res -> res

  let of_preds yes no a b =
    match yes a b with
      | Some(rho) -> 
	  Three.Yes(rho)
      | None -> 
	  (match no a b with
	     | Some(rho) -> 
		 Three.No(rho) 
	     | None ->
		 Three.X)

end


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
    | Refl     (* no argument for optimization reasons. *)    
    | Trans of Term.t * Term.t * Term.t
    | Subst of Th.t option * Atom.t
    | Contradiction
    | Equiv of Atom.t
    | Solve of Th.t * Term.t * Term.t
    | Implied of Atom.t
    | Nonzero of Term.t
    | Groebner of Term.t * Term.t
    | Gomory of Term.t
    | Abstract of Term.t * Term.t
    | Const of Term.t * Term.t
    | Array of int * Term.t * Term.t
    | Negation of Atom.t
    | Oracle of string * Atom.t

  let justifies = function
    | Refl  -> Atom.mk_true
    | Trans(a, b, c) -> Atom.mk_equal (a, c)
    | Subst(_, atm) -> atm
    | Contradiction -> Atom.mk_false
    | Equiv(atm) -> atm
    | Solve(_, a, b) -> Atom.mk_equal (a, b)
    | Implied(atm) -> atm
    | Nonzero(a) -> Atom.mk_diseq (a, Arith.mk_zero)
    | Groebner(a, b) -> Atom.mk_equal (a, b)
    | Gomory(a) -> Atom.mk_nonneg (a)
    | Abstract(a, b) -> Atom.mk_equal (a, b)
    | Const(a, b) -> Atom.mk_diseq (a, b)
    | Array(_, a, b) -> Atom.mk_equal (a, b)
    | Negation(atm) -> atm
    | Oracle(_, atm) -> atm

  let pp fmt prf =
    let apply op al = Pretty.apply Term.pp fmt (op, al) in
    let apply_to_atom op atm = Pretty.apply Atom.pp fmt (op, [atm]) in
    match prf with
      | Refl -> apply "Refl" []
      | Trans(a, b, c) -> apply "Trans" [a; b; c]
      | Subst(None, atm) ->
	  Format.fprintf fmt "Subst(%s)" (Atom.to_string atm)
      | Subst(Some(i), atm) ->
	  Format.fprintf fmt "Subst(%s, %s)" (Th.to_string i) (Atom.to_string atm)
      | Contradiction -> apply "Contradiction" []
      | Equiv(atm) -> 
	  Format.fprintf fmt "Equiv(%s)" (Pretty.to_string Atom.pp atm)
      | Solve(th, a, b) -> apply (Format.sprintf "Solve(%s)" (Th.to_string th)) [a; b]
      | Implied(atm) -> apply_to_atom "Implied" atm
      | Nonzero(a) -> apply "Nonzero" [a]
      | Groebner(a, b) -> apply "Groebner" [a; b]
      | Gomory(a) -> apply "Gomory" [a]
      | Abstract(a, b) -> apply "Abstract" [a; b]
      | Const(a, b) -> apply "Const" [a; b]
      | Array(i, a, b) -> apply (Format.sprintf "Array(%d)" i) [a; b]
      | Negation(atm) -> apply_to_atom "negation" atm
      | Oracle(str, atm) -> apply_to_atom str atm

  let to_string = Pretty.to_string pp

end


(** {6 Dependencies} *)

module Dep = struct
  type t = Atom.Set.t
  let elements = Atom.Set.elements	   
  let pp fmt ds = Pretty.set Atom.pp fmt (elements ds)
  let empty = Atom.Set.empty
  let is_empty = Atom.Set.is_empty
  let singleton = Atom.Set.singleton
  let of_atom = singleton
  let add = Atom.Set.add
  let union = Atom.Set.union
  let to_list ds =  Atom.Set.fold (fun d acc -> d :: acc) ds []
end 


(** {6 Proofs} *)

module Proof = struct

  type t = 
    | Axiom of Atom.t
    | Apply of Rule.t * t list

  let rec pp fmt = function
    | Axiom(h) -> 
	let name = Pretty.to_string Atom.pp h in
	  Format.fprintf fmt "Axiom(%s)" name
    | Apply(rl, []) ->
	Rule.pp fmt rl
    | Apply(rl, jl) ->
	Pretty.apply pp fmt (Rule.to_string rl, jl)
	
  let axioms_of =
    let rec collect acc = function
      | Axiom(a) -> Dep.add a acc
      | Apply(_, prfs) -> List.fold_left collect acc prfs
    in 
      collect Dep.empty

  let justifies = function
    | Axiom(a) -> a
    | Apply(rl, _) -> Rule.justifies rl

end 

	

(** {6 Justifications} *)


type t =
  | Unjustified
  | Dependency of Dep.t
  | Proof of Proof.t

 
let axioms_of = function
  | Unjustified -> raise Not_found
  | Dependency(hyps) -> hyps
  | Proof(prf) -> Proof.axioms_of prf

let proof_of = function
  | Proof(prf) -> prf
  | _ -> raise Not_found

let justifies = function
  | Unjustified -> raise Not_found
  | Dependency _ -> raise Not_found
  | Proof(prf) -> Proof.justifies prf

let pp fmt j =
  match !proofmode with
    | No -> 
	Pretty.string fmt "Unjustified"
    | Dep ->
	Dep.pp fmt (axioms_of j)
    | Yes ->
	(match j with
	   | Unjustified -> 
	       Pretty.string fmt "Unjustified"
	   | Dependency(deps) ->
	       Dep.pp fmt deps
	   | Proof(prf) -> 
	       Proof.pp fmt prf)

exception Inconsistent of t
exception Valid of t

let inconsistent rho =
  raise(Inconsistent(rho))


let is_none = function
  | Unjustified -> true
  | _ -> false

(** {6 Constructors} *)

let mk_none () =
  match !proofmode with
    | No -> Unjustified
    | _ -> Format.eprintf "Warning: empty justification"; Unjustified

let mk_dependency0 = Dependency(Dep.empty)

let mk_dependency1 j =
  match j with
    | Unjustified -> j
    | Dependency _ -> j
    | Proof(prf) -> 
	let deps = Proof.axioms_of prf in
	  if Dep.is_empty deps then mk_dependency0 else
	    Dependency(deps)

let mk_dependency2 j1 j2 =
  if j1 == mk_dependency0 then j2
  else if j2 = mk_dependency0 then j1
  else 
    try
      let deps1 = axioms_of j1
      and deps2 = axioms_of j2 in
      let deps = Dep.union deps1 deps2 in
	if deps == deps1 then j1
	else if deps == deps2 then j2 else
	  Dependency(deps)
    with
	Not_found -> mk_none()

let mk_dependency3 j1 j2 j3 =
  mk_dependency2 j1 (mk_dependency2 j2 j3)

let mk_dependency =
  let rec loop acc = function
    | [] -> acc
    | [j] -> mk_dependency2 acc j
    | [j1; j2] -> mk_dependency3 j1 j2 acc
    | j :: jl -> loop (mk_dependency2 acc j) jl
  in
    loop mk_dependency0 


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
    | Dep -> Dependency(Dep.of_atom atm)
    | Yes -> mk_axiom atm

let refl = mk_apply0 Rule.Refl

	  
let trans a b c j1 j2 = 
  if a == b then j2       (* Cheap but incomplete test *)
  else if b == c then j1
  else match !proofmode with
    | No -> mk_none()
    | Dep -> mk_dependency2 j1 j2
    | Yes -> mk_apply2 (Rule.Trans(a, b, c)) j1 j2

let apply (a, b) jl =
  match !proofmode with
    | No -> mk_none()
    | Dep -> mk_dependency jl
    | Yes -> mk_apply (Rule.Subst(None, Atom.mk_equal(a, b))) jl

let apply1 (a, b) j =
  match !proofmode with
    | No -> mk_none()
    | Dep -> mk_dependency1 j
    | Yes -> mk_apply1 (Rule.Subst(None, Atom.mk_equal(a, b))) j

let equiv atm j1 j2 =
  match !proofmode with
    | No -> mk_none()
    | Dep -> mk_dependency2 j1 j2
    | Yes -> mk_apply2 (Rule.Equiv(atm)) j1 j2

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

let contradiction1 j =
  match !proofmode with
    | No -> mk_none()
    | Dep -> mk_dependency1 j
    | Yes -> mk_apply1 Rule.Contradiction j

let inconsistent2 j1 j2 =
  inconsistent (contradiction j1 j2)

let inconsistent3 j1 j2 j3 =
  inconsistent (contradiction_star [j1; j2; j3])

let inconsistent_star jl =
  inconsistent (contradiction_star jl)

let valid j1 j2 =
  match !proofmode with
    | No -> mk_none()
    | Dep -> mk_dependency2 j1 j2
    | Yes -> mk_apply2 (Rule.Equiv(Atom.mk_true)) j1 j2

let invalid j1 j2 =
  match !proofmode with
    | No -> mk_none()
    | Dep -> mk_dependency2 j1 j2
    | Yes -> mk_apply2 (Rule.Equiv(Atom.mk_false)) j1 j2



let solve th (x, a) j =
  match !proofmode with
    | No -> mk_none()
    | Dep -> mk_dependency1 j
    | Yes -> mk_apply1 (Rule.Solve(th, x, a)) j

let sigma ((f, al), b) jl =
  match !proofmode with
    | No -> mk_none()
    | Dep -> mk_dependency jl
    | Yes -> 
	let a = Term.App.mk_app f al in
	  if Term.eq a b then refl else 
	    mk_apply (Rule.Subst(None, (Atom.mk_equal(a, b)))) jl
  
let extend (x, a) =
  match !proofmode with
    | No -> mk_none()
    | Dep -> mk_dependency0
    | Yes -> mk_apply0 (Rule.Abstract(x, a))

let slackify (k, a) j =
  match !proofmode with
    | No -> mk_none()
    | Dep -> mk_dependency1 j
    | Yes -> mk_apply1 (Rule.Abstract(k, a)) j

let abstract (a, b) jl =
    match !proofmode with
      | No -> mk_none()
      | Dep -> mk_dependency jl
      | Yes -> if a == b then refl else mk_apply (Rule.Abstract(a, b)) jl

let nonzero a j =
  match !proofmode with
    | No -> mk_none()
    | Dep -> mk_dependency1 j
    | Yes -> mk_apply1 (Rule.Implied(Atom.mk_diseq (a, Arith.mk_zero))) j

let implied atm j =
  match !proofmode with
    | No -> mk_none()
    | Dep -> mk_dependency1 j
    | Yes -> mk_apply1 (Rule.Implied(atm)) j

let implied_equal a b j =
  match !proofmode with
    | No -> mk_none()
    | Dep -> mk_dependency1 j
    | Yes -> mk_apply1 (Rule.Implied(Atom.mk_equal(a, b))) j

let weaken a j =
  match !proofmode with
    | No -> mk_none()
    | Dep -> mk_dependency1 j
    | Yes -> mk_apply1 (Rule.Implied(Atom.mk_pos a)) j

let zero a j1 j2 =
  match !proofmode with
    | No -> mk_none()
    | Dep -> mk_dependency2 j1 j2
    | Yes -> mk_apply2 (Rule.Implied(Atom.mk_equal (a, Arith.mk_zero))) j1 j2

let groebner (a, b) j =
  match !proofmode with
    | No -> mk_none()
    | Dep -> mk_dependency1 j
    | Yes -> mk_apply1 (Rule.Groebner(a, b)) j

let gomory a j =
  match !proofmode with
    | No -> mk_none()
    | Dep -> mk_dependency1 j
    | Yes -> mk_apply1 (Rule.Gomory(a)) j

let oracle str jl =
  match !proofmode with
    | No -> mk_none()
    | Dep -> mk_dependency jl
    | Yes -> mk_apply (Rule.Oracle(str, Atom.mk_true)) jl

let negation atm j =
  match !proofmode with
    | No -> mk_none()
    | Dep -> mk_dependency1 j
    | Yes -> mk_apply1 (Rule.Negation(atm)) j

let negation_equal a b j =
  match !proofmode with
    | No -> mk_none()
    | Dep -> mk_dependency1 j
    | Yes -> mk_apply1 (Rule.Negation(Atom.mk_diseq(a, b))) j

let negation_nonneg a j =
  match !proofmode with
    | No -> mk_none()
    | Dep -> mk_dependency1 j
    | Yes -> mk_apply1 (Rule.Negation(Atom.mk_pos a)) j


let const (a, b) j1 j2 =
  match !proofmode with
    | No -> mk_none()
    | Dep -> mk_dependency2 j1 j2
    | Yes -> mk_apply2 (Rule.Const(a, b)) j1 j2

let posint a j1 j2 =
  match !proofmode with
    | No -> mk_none()
    | Dep -> mk_dependency2 j1 j2
    | Yes -> mk_none()

let array i a b jl =
  match !proofmode with
    | No -> mk_none()
    | Dep -> mk_dependency jl
    | Yes -> mk_apply (Rule.Array(i, a, b)) jl

let dependencies jl = 
  match !proofmode with
    | No -> mk_none()
    | Dep -> mk_dependency jl
    | Yes -> 
	Format.eprintf "Warning: constructing dependencies only@?";
	mk_dependency jl

let dependencies0 = mk_dependency []

let dependencies1 j1 = 
  match !proofmode with
    | No -> mk_none()
    | Dep -> mk_dependency1 j1
    | Yes -> 
	Format.eprintf "Warning: constructing dependencies only@?";
	mk_dependency1 j1

let dependencies2 j1 j2 = 
  match !proofmode with
    | No -> mk_none()
    | Dep -> mk_dependency2 j1 j2
    | Yes -> 
	Format.eprintf "Warning: constructing dependencies only@?";
	mk_dependency2 j1 j2
	

(** {6 Derived Rules} *)

let trans3 (a, b, c, d) j1 j2 j3 =
  if a == b then trans b c d j2 j3
  else if b == c then trans a c d j1 j3
  else if c == d then trans a b c j1 j2
  else trans a b d j1 (trans b c d j2 j3)

let subst_equal (a, b) j jl =
  match !proofmode with
    | No -> mk_none()
    | Dep -> mk_dependency (j :: jl)
    | Yes -> mk_apply (Rule.Subst(None, Atom.mk_equal(a, b))) (j :: jl)

let subst_equal1 a b j j1  =
  match !proofmode with
    | No -> mk_none()
    | Dep -> mk_dependency2 j j1
    | Yes -> mk_apply2 (Rule.Subst(None, Atom.mk_equal(a, b))) j j1

let subst_equal2 a b j j1 j2 =
  match !proofmode with
    | No -> mk_none()
    | Dep -> mk_dependency3 j j1 j2
    | Yes -> mk_apply (Rule.Subst(None, Atom.mk_equal(a, b))) [j; j1; j2]


let subst_diseq (a, b) j jl =
  match !proofmode with
    | No -> mk_none()
    | Dep -> mk_dependency (j :: jl)
    | Yes -> mk_apply (Rule.Subst(None, Atom.mk_diseq(a, b))) (j :: jl)


let subst_nonneg a j jl =
  match !proofmode with
    | No -> mk_none()
    | Dep -> mk_dependency (j :: jl)
    | Yes -> mk_apply (Rule.Subst(None, Atom.mk_nonneg(a))) (j :: jl)

let subst_pos a j jl =
  match !proofmode with
    | No -> mk_none()
    | Dep -> mk_dependency (j :: jl)
    | Yes -> mk_apply (Rule.Subst(None, Atom.mk_pos a)) (j :: jl)

let subst_in j jl =
  match !proofmode with
    | No -> mk_none()
    | Dep -> mk_dependency (j :: jl)
    | Yes ->
	(try 
	   mk_apply (Rule.Subst(None, justifies j)) (j :: jl)
	 with 
	     Not_found ->
	       mk_dependency (j :: jl))

let subst_in1 j j1 =
  match !proofmode with
    | No -> mk_none()
    | Dep -> mk_dependency2 j j1
    | Yes ->
	(try 
	   mk_apply (Rule.Subst(None, justifies j)) [j1]
	 with 
	     Not_found ->
	       mk_dependency2 j j1)

let subst_in2 j j1 j2 =
  match !proofmode with
    | No -> mk_none()
    | Dep -> mk_dependency3 j j1 j2
    | Yes ->
	(try 
	   mk_apply (Rule.Subst(None, justifies j)) [j1; j2]
	 with 
	     Not_found ->
	       mk_dependency3 j j1 j2)
	
 

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
      if !proofmode <> No then
	hyps := rho :: !hyps;
      b

  let id a = (a, refl)

  let compose f g a =
    let (b, rho) = g a in           (* [rho |- a = b] *)
    let (c, tau) = f b in           (* [tau |- b = c] *)
      (c, trans a b c rho tau)

  let compose3 f g h = (compose f (compose g h))

  let totalize f a =
    try f a with Not_found -> id a

  let compose_partial1 f g a =
    try
      let (b, rho) = g a in
	(try
	   let (c, tau) = f b in
	     (c, trans a b c rho tau )
	 with
	     Not_found -> (b, rho))
    with
	exc ->
	  Format.eprintf "%s@." (Printexc.to_string exc);
	  raise exc

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
	      Some(subst_in1 tau rho)
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
    let (b', beta') = f b in     (* [beta' |- b = b'] *)
      match p a' b' with
	| None -> None
	| (Some(tau) as res) ->      (* [tau |- p a' b'] *)
	    (match a == a', b == b' with
	       | true, true -> res
	       | true, false -> Some(subst_in1 tau beta')
	       | false, true -> Some(subst_in1 tau alpha')
	       | false, false -> Some(subst_in2 tau alpha' beta'))

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
	      Three.Yes(subst_in1 tau alpha')
	| (Three.No(tau) as res) ->
	    if a == a' then res else
	      Three.No(subst_in1 tau alpha')
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
	| (Three.Yes(tau) as res) ->
	    (match a == a', b == b' with
	       | true, true -> res
	       | true, false -> Three.Yes(subst_in1 tau beta')
	       | false, true -> Three.Yes(subst_in1 tau alpha')
	       | false, false -> Three.Yes(subst_in2 tau alpha' beta'))
	| (Three.No(tau) as res) ->
	    (match a == a', b == b' with
	       | true, true -> res
	       | true, false -> Three.No(subst_in1 tau beta')
	       | false, true -> Three.No(subst_in1 tau alpha')
	       | false, false -> Three.No(subst_in2 tau alpha' beta'))
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


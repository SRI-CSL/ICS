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

module Mode = struct
  
  type t = No | Dep

  let to_string = function
    | No -> "no"
    | Dep -> "dep"

  let of_string = function
    | "no" -> No
    | "dep" -> Dep
    | str -> raise (Invalid_argument(str ^ " : no such proof mode"))
 
  let proofmode = ref Dep

  let get () = !proofmode

  let set = function
    | Dep -> proofmode := Dep
    | No -> 
	if !proofmode = Dep then
	  invalid_arg "Illegal operation: setting proof mode from [No] to [Dep]"
	else 
	  proofmode := No

  let is_none () = (!proofmode = No)

 end 

open Mode

type t = Atom.Set.t
 
let axioms_of j = j

let pp fmt j =
  match !proofmode with
    | No -> Pretty.string fmt "Unjustified"
    | Dep -> Pretty.set Atom.pp fmt (Atom.Set.elements j)

let to_string = Pretty.to_string pp 
 
exception Inconsistent of t

let inconsistent rho =
  raise(Inconsistent(rho))

let axiom atm = 
  match !proofmode with
    | No -> Atom.Set.empty
    | Dep -> Atom.Set.singleton atm

let dep0 = Atom.Set.empty

let dep1 j =
  match !proofmode with
    | No -> Atom.Set.empty
    | Dep -> j

let dep2 j1 j2 = 
  match !proofmode with
    | No -> Atom.Set.empty
    | Dep -> Atom.Set.union j1 j2

let dep3 j1 j2 j3 = 
  match !proofmode with
    | No -> Atom.Set.empty
    | Dep -> Atom.Set.union j1 (Atom.Set.union j2 j3)

let dep jl = 
  match !proofmode with
    | No -> Atom.Set.empty
    | Dep -> List.fold_left Atom.Set.union Atom.Set.empty jl


type jst = t

module Three = struct
  type t =
    | Yes of jst
    | No of jst    
    | X

  (** Accumulate facts in global variable [justs]. *)
  let to_three justs p a b =
    match p a b with
      | Yes(rho) -> justs := rho :: !justs; Three.Yes
      | No(rho) -> justs := rho :: !justs; Three.No
      | X ->  Three.X

  let of_three =
    let yes = Yes(dep0)
    and no = No(dep0) in
      fun p a ->
	match p a with
	  | Three.Yes -> yes
	  | Three.No -> no
	  | Three.X -> X

  let pp fmt = function
    | Yes(rho) -> Pretty.apply pp fmt ("yes", [rho])
    | No(rho) -> Pretty.apply pp fmt ("no", [rho])
    | X -> Pretty.string fmt "x"
end


module Eqtrans = struct
  type t = Term.t -> Term.t * jst

  let id a = (a, dep0)

  let compose f g a =
    let (b, rho) = g a in           (* [rho |- a = b] *)
    let (c, tau) = f b in           (* [tau |- b = c] *)
      (c, dep2 rho tau)

  let compose3 f g h = (compose f (compose g h))

  let totalize f a =
    try f a with Not_found -> id a

  let compose_partial1 f g a =
    try
      let (b, rho) = g a in
	(try
	   let (c, tau) = f b in
	     (c, dep2 rho tau )
	 with
	     Not_found -> (b, rho))
    with
	exc ->
	  Format.eprintf "%s@." (Printexc.to_string exc);
	  raise exc

  let trace lvl name =
    Trace.func lvl name Term.pp (Pretty.pair Term.pp pp) 

  let replace map f a =
    let tau = ref dep0 in
    let lookup y = 
      try
	let (b, rho) = f y in
	  tau := dep2 rho !tau;
	  b
      with
	  Not_found -> y
    in
    let b = map lookup a in
      (b, !tau)

  let apply app (e, rho) a =
    let b = app (Atom.Equal.destruct e) a in
      if a == b then id a else
	(b, dep1 rho)
	
  let pointwise f al =
    let rho = ref dep0 in      
    let f' a = 
      let (b, tau) = f a in
	rho := dep2 tau !rho; b
    in
    let bl = Term.mapl f' al in 
      (bl, !rho)

  
  let mapargs mk_app f a =
    let (op, al) = Term.App.destruct a in
      match al with
	| [] -> 
	    id a
	| [a1] ->
	    let (b1, rho1) = f op a1 in
	      if a1 == b1 then id a else 
		let (b, tau) = mk_app op [b1] in
		  (b, dep2 tau rho1)
	| _ ->
	    let rho = ref dep0 in                (* [rhoi |- bi = ai] *)
	    let f' a = 
	      let (b, rhoi) = f op a in
		rho := dep2 rhoi !rho; b
	    in
	    let bl = Term.mapl f' al in                                
	      if al == bl then id a else 
		let (b, tau) = mk_app op bl in   (* [tau |- b = op(bl)] *)
		  (b, dep2 tau !rho)       	  
end


module Pred = struct
  type t = Term.t -> jst option

  (** Test predicate [p] on [f(a)]. *)
  let apply f p a =
    let (b, rho) = f a in   (* [rho |- a = b] *)
      match p b with
	| (Some(tau) as res) ->      (* [tau |- p(b)] *)
	    if a == b then res else 
	      Some(dep2 tau rho)
	| None ->
	    None

  (** Disjunction. *)
  let disj p q a =
    match p a with
      | None -> q a
      | res -> res

  let trace lvl name =
    Trace.func lvl name Term.pp (Pretty.option pp)
end

module Pred2 = struct
  type t = Term.t -> Term.t -> jst option

  (** Test predicate [p] on [f(a)]. *)
  let apply f p a b =
    let (a', alpha') = f a in   (* [alpha' |- a = a'] *)
    let (b', beta') = f b in     (* [beta' |- b = b'] *)
      match p a' b' with
	| None -> None
	| (Some(tau) as res) ->      (* [tau |- p a' b'] *)
	    if a == a' && b == b' then res else
	      Some(dep3 tau alpha' beta')
		
  let trace lvl name =
    Trace.func2 lvl name Term.pp Term.pp (Pretty.option pp)
end


module Rel1 = struct
  type t = Term.t -> Three.t

  (** Test binary term relation [r] on [f a] and [f b]. *)
  let apply f r a =
    let (a', alpha') = f a in
      match r a' with
	| (Three.Yes(tau) as res) ->
	    if a == a' then res else Three.Yes(dep2 tau alpha')
	| (Three.No(tau) as res) ->
	    if a == a' then res else Three.No(dep2 tau alpha')
	| Three.X ->
	    Three.X

  let orelse p q a =
    match p a with
      | Three.X -> q a
      | res -> res

  let yes_or_no yes no a =
    match yes a with
      | Some(rho) -> 
	  Three.Yes(rho)
      | None -> 
	  (match no a with
	     | Some(rho) -> 
		 Three.No(rho) 
	     | None ->
		 Three.X)

  let trace lvl name = Trace.func lvl name Term.pp Three.pp
end 

module Rel2 = struct
  type t = Term.t -> Term.t -> Three.t

  (** Test binary term relation [r] on [f a] and [f b]. *)
  let apply f r a b =
    let (a', alpha') = f a 
    and (b', beta') = f b in
      match r a' b' with
	| (Three.Yes(tau) as res) ->
	    if a == a' && b == b' then res else Three.Yes(dep3 tau alpha' beta')
	| (Three.No(tau) as res) ->
	    if a == a' && b == b' then res else Three.Yes(dep3 tau alpha' beta')
	| Three.X ->
	    Three.X

  let orelse r s a b =
    match r a b with
      | Three.X -> s a b
      | res -> res

  let yes p a b =
    match p a b with
      | Some(rho) -> Three.Yes(rho)
      | None -> Three.X

  let yes_or_no yes no a b =
    match yes a b with
      | Some(rho) -> 
	  Three.Yes(rho)
      | None -> 
	  (match no a b with
	     | Some(rho) -> 
		 Three.No(rho) 
	     | None ->
		 Three.X)

  let trace lvl name =
    Trace.func2 lvl name Term.pp Term.pp Three.pp
end


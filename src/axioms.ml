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

(** Set of logical variables. *)
module Vars = Name.Set

(** Substitution with bindings [x |-> a], [x] a name and [a] a term. *)
module Subst = Name.Map

type subst = Term.t Subst.t

exception Bot

(** Terms with logical variables. *)
module Lterm = struct
  
  type t = 
    | Var of Name.t
    | App of Funsym.t * t list
	
  let is_lvar = function
    | Var _ -> true
    | App _ -> false

  let mk_var x = Var(Name.of_string x)
  let mk_app f al = App(f, al)
	
  let rec pp fmt = function
      | Var(x) -> 
	  Name.pp fmt x
      | App(f, al) -> 
	  Funsym.pp fmt f;
	  Format.fprintf fmt "(";
	  Pretty.infixl pp ", " fmt al;
	  Format.fprintf fmt ")"
	    
  let vars =
    let rec of_lterm acc = function
      | Var(x) -> Vars.add x acc
      | App(_, al) -> of_args acc al
    and of_args acc = function
	| [] -> acc
	| a :: al -> of_lterm (of_args acc al) a
    in
      of_lterm Vars.empty
	
  (** Matching a pattern with a term:
    - [match ?x b rho = rho] if [rho(?x) = b]
      - [match ?x b rho = bot] if [rho(?x) <> b]
    - [match f(a{1},...,a{n}) f(b{1},...,b{n}) rho = rho{n}]
      where [rho{0} = rho], [rho{i+1} = match a{i} b{i} rho{i}].
    - [match f(a{1},...,a{n}) g(b{1},...,b{m}) rho = bot] if [f <> g] or [n <> m].  *)
  let rec matcher pat (b: Term.t) rho = 
    match pat with
      | Var(x) -> matchv x b rho
      | App(f, al) ->
	  if Term.is_var b then raise Bot else
	    let g = Term.sym_of b in
	      if Funsym.eq f g then 
		matchl al (Term.args_of b) rho
	      else 
		raise Bot
		  
  and matchv x b rho =
    try
      let rho_x  = Subst.find x rho in
	if Term.eq rho_x b then rho else raise Bot
    with
	Not_found ->
	  Subst.add x b rho
	    
  and matchl al (bl: Term.Args.t) rho = 
    if List.length al <> Term.Args.length bl then 
      raise Bot 
    else
      List.fold_right2 matcher al (Term.Args.to_list bl) rho
	
  (** Apply term substitution. *)
  let apply can rho =
    let rec app = function
      | Var(x) -> Subst.find x rho
      | App(f, al) -> can f (appl al)
    and appl al =
      Term.Args.of_list (List.map app al)
    in
      app 

  let is_equal can rho a b = 
    try
      let a' = apply can a rho 
      and b' = apply can b rho in
	Term.eq a' b'
    with
	Not_found -> false
	
  let is_diseq can rho a b =
    try
      let a' = apply can a rho 
      and b' = apply can b rho in
	(match Term.is_equal a' b' with
	  | Three.No -> true
	  | _ -> false)
    with
	Not_found -> false

end

module Atom = struct
    
  type t = 
    | Equal of Lterm.t * Lterm.t
    | Diseq of Lterm.t * Lterm.t
	  
  let mk_equal a b = Equal(a, b)
  let mk_diseq a b = Diseq(a, b)
			 
  let pp fmt = function
    | Equal(a, b) -> 
	Pretty.infix Lterm.pp " = " Lterm.pp fmt (a, b)
    | Diseq(a, b) -> 
	Pretty.infix Lterm.pp " <> " Lterm.pp fmt (a, b)
	
  let ppl fmt = Pretty.list pp fmt
		  
  let vars = function
    | Equal(a, b) -> Vars.union (Lterm.vars a) (Lterm.vars b)
    | Diseq(a, b) -> Vars.union (Lterm.vars a) (Lterm.vars b)
	
  let matcher pat atm rho =
    match pat, atm with
      | Equal(p, q), Atom.Equal(a, b) -> 
	  Lterm.matcher q b (Lterm.matcher p a rho)
      | Diseq(p, q), Atom.Diseq(a, b) -> 
	  Lterm.matcher q b (Lterm.matcher p a rho)
      | _ -> 
	  raise Bot

  (** Apply term substitution. *)
  let apply can rho = 
    let inst = Lterm.apply can rho in
      function
	| Equal(a, b) ->
	    Atom.mk_equal (inst a) (inst b)
	| Diseq(a, b) ->
	    Atom.mk_diseq (inst a) (inst b)
	    
end
  
module Chain = struct 
  
  type t = {
    name : Name.t;
    hyps : Atom.t list;
    concl : Atom.t
  }
      
  let hyps ch = ch.hyps
  let concl ch = ch.concl
		   
  let make name hyps concl =
    { name = name; hyps = hyps; concl = concl}
    
  let mk_equal name hyps a b = make name hyps (Atom.mk_equal a b)
			    
  let mk_diseq name hyps a b = make name hyps (Atom.mk_diseq a b)
			    
  let rec pp fmt ch =
    if Name.to_string ch.name <> "" then
      begin
	Name.pp fmt ch.name;
	Format.fprintf fmt ": ";
      end;
    Atom.ppl fmt ch.hyps;
    Format.fprintf fmt " ==> ";
    Atom.pp fmt ch.concl;
    Format.fprintf fmt "@;"
      
end

module Rewrite = struct
   
  type t = {
    name : Name.t;
    hyps : Atom.t list;
    lhs : Funsym.t * Lterm.t list;
    rhs : Lterm.t;
  }
      
  let hyps r = r.hyps
  let lhs r = let f, al = r.lhs in Lterm.App(f, al)
  let rhs r = r.rhs
		
  let pp fmt r =
    Atom.ppl fmt r.hyps;
    Format.fprintf fmt " --> ";
    let f, al = r.lhs in
      Atom.pp fmt (Atom.mk_equal (Lterm.App(f, al)) r.rhs);
      Format.fprintf fmt "@;"
	  
  let make name hyps (f, al) b =
    { name = name; hyps = hyps; lhs = (f, al); rhs = b }
      
  let rec pp fmt r =   
    if Name.to_string r.name <> "" then
      begin
	Name.pp fmt r.name;
	Format.fprintf fmt ": ";
      end;
    Atom.ppl fmt r.hyps;
    Format.fprintf fmt " --> ";
    Atom.pp fmt (Atom.Equal(lhs r, rhs r));
    Format.fprintf fmt "@;"
	
  let to_chain r = 
    Chain.make r.name r.hyps (Atom.Equal(lhs r, rhs r))
  
end


module type AXIOMS = sig
  val rewrites : Rewrite.t list
  val chains : Chain.t list
end


(** {6 Normalization} *)

module Compile(A: AXIOMS) = struct

  (** Indexes off rewrites by means of the toplevel function symbol [f]
    in a rewrite rule [hyps --> f(a1,...,an) = b]. *)  
  let rewrite_idx =
    List.fold_left
      (fun idx r -> 
	 let (f, _) = r.Rewrite.lhs in
	 let cod = try Funsym.Map.find f idx with Not_found -> [] in
	   Funsym.Map.add f (r :: cod) idx)
      Funsym.Map.empty A.rewrites

   (** List of chains with disequalities in conclusion. *)
   let diseq_chains =
     List.fold_left
       (fun idx ({Chain.hyps = hyps; Chain.concl = concl}) ->
	  match concl with
	    | Atom.Diseq(a, b) ->
		(hyps, a, b) :: idx
	    | _ ->
		idx)
       [] A.chains

   exception Not_applicable

   module Table = Hashtbl.Make(
     struct
       type t = Funsym.t * Term.Args.t
       let equal (f, a) (g, b) = Funsym.eq f g && Term.Args.eq a b
       let hash = Hashtbl.hash_param 3 3
     end)

   let memoize = Table.create 17
   let _ = Tools.add_at_reset (fun () -> Table.clear memoize)

   let id r = 
     Format.sprintf "Rewrite[%s]" 
       (Name.to_string r.Rewrite.name)

   let rec normalize f a =
     try
       Table.find memoize (f, a) 
     with
	 Not_found -> 
	   let t = norm f a in
	     Table.add memoize (f, a) t; t
	       
   and norm f =
     try
       let rl = Funsym.Map.find f rewrite_idx in
	 fun (a: Term.Args.t) -> 
	   let rec orelse = function
	     | [] -> 
		 Term.mk_app f a
	     | r :: rl' -> 
		 (try rewrite1 r f a with Not_applicable -> orelse rl')
	   in
	     orelse rl
     with
	 Not_found -> Term.mk_app f

   and rewrite1 r = 
    let g, patl = r.Rewrite.lhs in
      fun f bl -> 
	assert(Funsym.eq f g);
	try
	  let rho = Lterm.matchl patl bl Subst.empty in
	    if List.for_all (valid rho) r.Rewrite.hyps then
	      begin
		Trace.call 6 (id r) (Term.mk_app f bl) Term.pp;
		let b = Lterm.apply norm rho r.Rewrite.rhs in
		  Trace.exit 6 (id r) b Term.pp;
		  b
	      end
	    else 
	      raise Not_applicable
	with
	    Bot -> raise Not_applicable
	      
  and valid rho = function
    | Atom.Equal(a, b) -> 
	is_lterm_equal rho a b
    | Atom.Diseq(a, b) -> 
	is_lterm_diseq rho a b

  and is_lterm_equal rho a b =
    try
      let a' = apply rho a
      and b' = apply rho b in
	Term.eq a' b'
    with
	Not_found ->false

  and apply rho = 
    Lterm.apply norm rho
	
  and is_lterm_diseq rho a b =
    try
      let a' = apply rho a
      and b' = apply rho b in
	is_diseq a' b'
    with
	Not_found -> false

  and is_diseq a b = 
    let is_deq c = is_diseq1 c a b in
      List.exists is_deq diseq_chains

  and is_diseq1 (hyps, q, p) =
    fun a b ->
      try
	let rho = 
	  Lterm.matcher q a 
	    (Lterm.matcher p b Subst.empty) 
	in
	  List.for_all (valid rho) hyps
      with
	  Bot ->
	    (try
	       let tau = 
		 Lterm.matcher q b 
		   (Lterm.matcher p a Subst.empty) 
	       in
		 List.for_all (valid tau) hyps
	     with
		 Bot -> false)
		   
end 


(** Compiled representation of a forward chaining rule. *)
module FlatChain = struct 
      
  type t = Name.t * hyps * concl
  and hyps = hyp list
  and hyp = 
    | HypEqual of var * term
    | HypDiseq of var * var  
  and concl = 
    | ConclEqual of term * term
    | ConclDiseq of term * term  
  and var = Name.t
  and term =
    | Var of var
    | App of Funsym.t * var list

  let to_chain (id, hl, c) =
    let rec to_hyp = function
      | HypEqual(x, Var(y)) -> 
	  Atom.mk_equal (to_lvar x) (to_lvar y)
      | HypEqual(x, a) -> 
	  Atom.mk_equal (to_lvar x) (to_lterm a)
      | HypDiseq(x, y) -> 
	  Atom.mk_diseq (to_lvar x) (to_lvar y)
    and to_concl = function
      | ConclEqual(a, b) -> Atom.mk_equal (to_lterm a) (to_lterm b)
      | ConclDiseq(a, b) -> Atom.mk_diseq (to_lterm a) (to_lterm b)
    and to_lterm = function
      | Var(x) -> to_lvar x
      | App(f, xl) ->
	    Lterm.App(f, List.map to_lvar xl)
    and to_lvar x =
      Lterm.Var(x)
    in
      Chain.make id (List.map to_hyp hl) (to_concl c)

  let pp fmt ch =
    Chain.pp fmt (to_chain ch)

  let of_chain {Chain.name = id; Chain.hyps = h; Chain.concl = c} =
    let acc = ref [] in
    let rec of_hyp h =
      match h with
	| Atom.Equal(a, b) ->
	    HypEqual(var_of_term a, flat_of_term b)
	| Atom.Diseq(a, b) -> 
	    HypDiseq(var_of_term a, var_of_term b)
    and of_concl c =
      match c with
	| Atom.Equal(a, b) ->
	    ConclEqual(flat_of_term a, flat_of_term b)
	| Atom.Diseq(a, b) -> 
	    ConclDiseq(flat_of_term a, flat_of_term b)
    and flat_of_term a =
      match a with
	| Lterm.Var(x) ->  Var(x)
	| Lterm.App(f, al) -> 
	    let xl = List.map var_of_term al in
	      App(f, xl)
    and var_of_term a =
      match a with
	| Lterm.Var(x)-> x
	| Lterm.App(f, al) ->
	    let xl = List.map var_of_term al in
	    let z = mk_fresh () in
	    let b = App(f, xl) in
	    let eq = HypEqual(z, b) in
	      acc := eq :: !acc; z
    and mk_fresh () =
      Name.fresh "v"
    in
    let h' = List.map of_hyp h 
    and c' = of_concl c in
      (id, h' @ !acc, c')
      
  let of_chains = List.map of_chain

  (** Order a list of hypothesis such that the result is connected. *)
  let reorder xs hl =
    let flats = ref []
    and vars = ref [] in
      List.iter
	(fun h -> 
	   match h with
	     | HypEqual(x, App _)  -> 
		 flats := h :: !flats
	     | _ -> 
		 vars := h :: !vars)
	hl;
      !flats @ !vars

	    
end 

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

let debug = Version.debug()

type iter = (Judgement.equal -> unit) -> unit

(** A {i canonizable} theory [th] is specified by means of a
  - canonizer [can],
  - forward chaining rules [on_vareq], [on_vardiseq], [on_flateq], and
  - a branching function [disjunction]. *)
module type T = sig
  val th : Theory.t
  val can : Funsym.t -> Term.Args.t -> Term.t
  val is_diseq : Term.t -> Term.t -> bool
  val chains : Axioms.Chain.t list
  val disjunction : iter -> Judgement.disjunction
end

module type CONFIG = sig
  type t
  val is_empty : t -> bool
  val equalities : t -> Judgement.Equals.t
  val pp : Format.formatter -> t -> unit
  val iter : (Judgement.equal -> unit) -> t -> unit
  val iter_on : Term.t -> (Judgement.equal -> unit) -> t -> unit
  val fold : (Judgement.equal -> 'a -> 'a) -> t -> 'a -> 'a
  val mem : Term.t -> t -> bool
  exception Empty  
  val dep : t -> Term.t -> Dep.Set.t
  val occ : Term.t -> t -> bool
  val in_dom : t -> Term.t -> bool 
  val in_cod : t -> Term.t -> bool
  val apply : t -> Term.t -> Term.t * Judgement.equal
  val inv : t -> Term.t -> Term.t * Judgement.equal
  val empty : unit -> t  
  val interp : t -> Term.Interp.t  
  val model : t -> Term.Model.t
  module Apply : sig
    val get : t -> Term.t -> Term.t
    val justify : t -> Term.t -> Judgement.equal
  end 
  module Inv : sig
    val get : t -> Term.t -> Term.t
    val justify : t -> Term.t -> Judgement.equal
  end 
  module Replace : sig
    val get : t -> Term.t -> Term.t
    val justify : t -> Term.t -> Judgement.equal
  end 
  module Diseq : sig
    val test : t -> Term.t -> Term.t -> bool
    val justify : t -> Term.t -> Term.t -> Judgement.diseq
  end
  module Equal : sig
    val test : t -> Term.t -> Term.t -> bool
    val justify : t -> Term.t -> Term.t -> Judgement.equal
  end 
end

module type INFSYS =
sig
  type config 
  val current : unit -> config
  val finalize : unit -> config
  val reset : unit -> unit
  val initialize : config -> unit
  val is_unchanged : unit -> bool
  val abstract : Term.t -> Judgement.atom -> unit
  val process_equal : Judgement.equal -> unit
  val propagate_equal : Term.t -> unit
  val process_diseq : Judgement.equal -> unit
  val propagate_diseq : Judgement.diseq -> unit
  val branch : unit -> Judgement.disjunction option
  val normalize : unit -> unit
end

module type COMPONENT = sig
  val th : Theory.t
  module Config : CONFIG
  module Infsys : (INFSYS with type config = Config.t)
end

module Make(Can: T) = struct

  let th = Can.th

  (** A {i pure} term is built up with function symbols from theory [Can.th]. *)
  let is_pure = Term.is_pure th

  (** A {i flat} term is of the form [f(x{1},...,x{n})] with [f] a function symbol
    in theory [th] and the [x{i}] are variables. *)
  let is_flat t =
    not (Term.is_var t) &&
    ((Term.theory_of t = th) && 
     Term.Args.for_all Term.is_var (Term.args_of t)) 

  module J = Judgement
    
  (** Representation of a set of equalities [{x{1} = a{1},...,x{n} = a{n}}] with
    distinct [a{i}] {i flat terms} but the [x{i}] are not necessarily pairwise disjoint. *)
  module Config = struct
    
    exception Found
    
    module Map = Term.Map
    module Set = Term.Set
      
    (** Bindings [x |-> {e{1},...,e{m}}] with 
      - [e{i} |- x{i} = t{i}] where [t{i}] is flat, 
      - [x] is canonical,
      - [x = x{i}] is valid, and
      - [x{i}] in [dep(y)] if [y in vars(t{i})]. *)
    type t = {
      mutable find : J.Equals.t Map.t;
      mutable dep : Dep.t;
    }
	
    let is_empty s = Map.is_empty s.find
		       
    let equalities s =
      let acc = J.Equals.empty() in
      let add e = J.Equals.add e acc in
	Map.iter (fun x es -> J.Equals.iter add es) s.find;
	acc
	  
    let pp fmt s =
      J.Equals.pp fmt (equalities s);
      if debug >= 1 then (Format.fprintf fmt "\ndep: "; Dep.pp fmt s.dep)

    let lookup s x = Map.find x s.find

    module Apply = struct
      let get s x =
	let es = lookup s x in
	let e = J.Equals.choose es in
	  e#rhs

      let justify s x =
	J.Equals.choose (Map.find x s.find)
    end 

    module Find = struct
      let get s x = try Apply.get s x with Not_found -> x
      let justify s x = try Apply.justify s x with Not_found -> J.mk_refl x
    end 
 
    let apply s x = (Apply.get s x, Apply.justify s x)
    let find s x = (Find.get s x, Find.justify s x)

    let map f t = failwith "to do"

    module Instantiate = struct
      let get e =
	let x = e#rhs and s = e#lhs in
	let lookup y = if Term.eq x y then s else y in
	  map lookup 

      (** If [e |- x = s] and [t[x := s] = t'], then [instantiate e t t' |- t = t']. *)
      class instantiate e t t' = 
	(object
	   inherit Judgement.Top.equal
	   method lhs = t
           method rhs = t'
           method name = Format.sprintf "instantiate[%s]" (Theory.to_string th)
           method hyps = J.mk_singleton (e:>J.atom)
	 end: Judgement.equal)

      let justify e t =
	let t' = get e t in
	  if Term.eq t t' then J.mk_refl t else
	    new instantiate e t t'

    end 

    module Replace = struct

      let get s =
	let rec repl t = 
	  if Term.is_var t then Apply.get s t else
	    let f = Term.sym_of t and a = Term.args_of t in
	    let a' = Term.Args.map repl a in
	      if a == a' then t else Can.can f a'
	in
	  repl

      let iter_on_hyps f s = 
	let rec iter t = 
	  if Term.is_var t then f (Apply.justify s t) else
	    Term.Args.iter iter (Term.args_of t)
	in
	  iter

      let hyps s t =
	let es = J.Equals.empty() in
	  iter_on_hyps (fun e -> J.Equals.add e es) s t;
	  es

      class replace es t t' = 
	(object
	   inherit Judgement.Top.equal
	   method lhs = t
           method rhs = t'
           method name = Format.sprintf "replace[%s]" (Theory.to_string th)
           method hyps = Judgement.equals_to_atoms es
	 end: Judgement.equal)
	
      let justify s t = 
	let t' = get s t in
	  if Term.eq t t' then J.mk_refl t else
	    new replace (hyps s t) t t'

    end 
	
    let iter f s =
      let setiter _ es = J.Equals.iter f es in
	Map.iter setiter s.find
	  
    let iter_on x f s = 
      Map.iter 
	(fun y es -> 
	   if Term.eq x y then J.Equals.iter f es else
	     J.Equals.iter
	       (fun e -> 
		  assert(Term.eq x e#lhs);
		  if Term.occurs x e#rhs then f e)
	     es)
	s.find
	
    let fold f s acc0 =
      let acc = ref acc0 in
      let g e = acc := f e !acc in
	iter g s;
	!acc
	  
    let mem x s = Map.mem x s.find
	
    exception Empty
      
    let dep s x =
      assert(Term.is_var x);
      try Dep.find s.dep x with Not_found -> raise Empty
	
    let occ x s =
      mem x s || Dep.mem x s.dep
	
    let in_dom s x = 
      assert(Term.is_var x);
      Map.mem x s.find
	
    let in_cod s x = 
      assert(Term.is_var x);
      Dep.mem x s.dep 
    
    (** Return [(x, rho)] if [rho |- x = a] is in [s]. 
      If [a] is a constant, then traverse the data structure
      to find the inverse.  Otherwise, the dependency index
      on some variable [y] in [a] is used to find [x = a]. *)
    module Inv = struct

      exception FoundEqual of J.equal

      let rec get s t =   
	if not(is_flat t) then raise Not_found else
	  try noncnstnt (Term.choose Term.is_var t) s t with Not_found -> 
	    cnstnt s t
	  
      and cnstnt s t =
	let test e =  if Term.eq t e#rhs then raise(FoundEqual(e)) in
	  try iter test s; raise Not_found with FoundEqual(e) -> e#lhs
	    
      and noncnstnt y s t =
	let test e = if Term.eq t e#rhs then raise(FoundEqual(e)) in
	let tests es = J.Equals.iter test es in
	try
	  Dep.Set.iter
	    (fun x -> 
	       try tests (Map.find x s.find) with Not_found -> 
		 invalid_arg "Imprecise dependency")
	    (dep s y);
	  raise Not_found
	with
	    FoundEqual(e) -> e#lhs

      let justify s t =
	let x = get s t in
	let es = lookup s x in
	  J.Equals.choose_if (fun e -> Term.eq t e#rhs) es

    end 

    let inv s t = (Inv.get s t, Inv.justify s t)

    module Equal = struct
      let test s t1 t2 =
	Term.eq (Replace.get s t1) (Replace.get s t2)

      let justify s t1 t2 =
	assert(test s t1 t2);
	let e1 = Replace.justify s t1 
	and e2 = Replace.justify s t2 in 
	  Judgement.mk_join e1 e2
	    
    end 

    module Diseq = struct

      let test s t1 t2 =
	Can.is_diseq (Replace.get s t1) (Replace.get s t2)

      class diseq es t1 t2 = 
	(object
           inherit Judgement.Top.unsat
	   method lhs = t1
	   method rhs = t2
	   method name = Format.sprintf "diseq[%s]" (Theory.to_string th)
           method hyps = Judgement.equals_to_atoms es
	 end : Judgement.diseq)

      let justify s t1 t2 =
	assert(test s t1 t2);
	let es1 = Replace.hyps s t1 
	and es2 = Replace.hyps s t2 in 
	  J.Equals.union es1 es2;
	  new diseq es2 t1 t2
	  
    end 
	      
    let empty () = {
      find = Map.empty();
      dep = Dep.empty();
    }
		     
    let is_canonical v s =
      let is_canonical_var x = 
	Term.is_var x && V.Config.is_canonical v x 
      in
      let is_canonical_flat a =
	is_flat a && 
	Term.Args.for_all is_canonical_var (Term.args_of a)
      in
	Map.for_all
	  (fun x es ->
	     is_canonical_var x && 
	     (J.Equals.for_all
		(fun e -> 
		   Term.eq x e#lhs &&
		   is_canonical_var x &&
		   is_canonical_flat e#rhs)
		es))
	  s.find
	  
    let interp s =
      fold 
	(fun e acc -> 
	   let x = e#lhs and t = e#rhs in
	   let f, a = Term.destruct t in
	     Term.Interp.update f a x acc)
	s Term.Interp.empty
	
    let model s =
      let id = Term.Assign.empty in
	(interp s, id)
	
    let copy s = {
      find = Term.Map.copy s.find;   
      dep = Dep.copy s.dep
    }
		   
    let restrict s x =
      assert(Term.is_var x);
      try
	let es = Map.find x s.find in
	  Map.remove x s.find;
	  J.Equals.iter
	    (fun e -> 
	       let x' = e#lhs and t = e#rhs in
		 Term.iter (fun y -> Dep.remove y x' s.dep) t)
	    es
      with
	  Not_found -> ()

    let remove s e =
      let x = e#lhs and a = e#rhs in
	assert(Term.is_var x && is_flat a);
	try
	  let es = lookup s x in
	    if J.Equals.mem e es then
	      begin
		J.Equals.remove e es;
		Map.set x es s.find;
		failwith "remove.dep: To do"
	      end
	with
	    Not_found -> ()
	    
    let extend s e =
      let x = e#lhs and t = e#rhs in
	assert(not(in_dom s x));
	Map.set x (J.Equals.singleton e) s.find;
	Term.iter (fun y -> Dep.add x y s.dep) t

    module Varify = struct

      let get s t = 
	assert(is_pure t);
	let rec varify t = 
	  if Term.is_var t then t else
	    try Inv.get s t with Not_found -> 
	      let f = Term.sym_of t and a = Term.args_of t in
	      let a' = Term.Args.map varify a in
	      let u' = Term.mk_fresh_var "u" in
	      let t' = Can.can f a' in
		assert(is_flat t');
		extend s (J.mk_alias u' t');
		u'
	in
	  varify t
	
     (** If [e{i} |- x{i} = t{i}],
       then [varify {e{1},...,e{n}} t t' |- t = x]. in the given theory. *)
      class varify es t x = 
	(object
	   inherit Judgement.Top.equal
	   method lhs = t
           method rhs = x
           method name = Format.sprintf "varify[%s]" (Theory.to_string th)
           method hyps = Judgement.equals_to_atoms es
	   method validate = Term.is_var x
	 end: Judgement.equal)
	
      let justify s t =
	assert(is_pure t);      (* collect equalities immediately, since *)
	let acc = J.Equals.empty () in            (* states are updated. *)
	let rec varify t = 
	  if Term.is_var t then t else
	    try 
	      let e = Inv.justify s t in
		assert(Term.is_var e#lhs);
		J.Equals.add e acc; e#lhs
	    with 
		Not_found -> 
		  let f = Term.sym_of t and a = Term.args_of t in
		  let a' = Term.Args.map varify a in
		  let u' = Term.mk_fresh_var "u" in
		  let t' = Can.can f a' in
		    assert(is_flat t');
		    extend s (J.mk_alias u' t');
		    u'
	in
	let x = varify t in
	  if Term.eq t x then J.mk_refl t else
	    new varify acc t x
    end 
      
  end 

  module type CLOSE = sig
    val on_vareq : Judgement.equal -> iter -> unit
    val on_vardiseq : Judgement.diseq -> iter -> unit
    val on_flateq : Judgement.equal -> iter -> unit
  end 
    
  (** Inference system from a canonizable theory. *)
  module Infsys =  struct

    type config = Config.t
    
    let th = Can.th
	
    (** Current configuration. *)
    let s = ref (Config.empty())
    let changed = ref (Term.Set.empty())
    let unchanged = ref true
		      
    let current () = !s
		       
    let is_unchanged () = Term.Set.is_empty !changed
			    
    let initialize s0 =
      changed := Term.Set.empty();
      unchanged := true;
      s := s0 
	
    let reset () = 
      initialize (Config.empty())
	
    let finalize () = 
      if is_unchanged () then !s else Config.copy !s

    let replace t = 
      Config.Replace.justify !s t
    
    let varify t = 
      Config.Varify.justify !s t

    let iter f =
      let setiter _ es = J.Equals.iter f es in
	Config.Map.iter setiter !s.Config.find

    let process_equal1 e =             
      assert(is_pure e#lhs && is_pure e#rhs);
      let e = J.mk_transform_equal replace e in
	if Term.eq e#lhs e#rhs then () else
	  let e = J.mk_transform_equal varify e in  
	    V.Infsys.process_equal e
	      
    let rec process_diseq1 d =
      let d = J.mk_transform_diseq replace d in
	if Term.eq d#lhs d#rhs then raise(J.Unsat(J.mk_bot d)) else
	V.Infsys.process_diseq (J.mk_transform_diseq varify d)

    (** Facts deduced by forward chaining. *)
    module Derived = struct
      let equals = Stacks.create ()
      let diseqs = Stacks.create ()
		     
      let init () = Stacks.clear equals; Stacks.clear diseqs
	  
      let add_equal e = Stacks.push e equals
	  
      let add_diseq d = Stacks.push d diseqs
	  
      (** Process derived facts until completion. *)
      let close () =
	let rec cl () = 
	  try process_equal1 (Stacks.pop equals); cl () with Stacks.Empty -> 
	    (try process_diseq1 (Stacks.pop diseqs); cl () with Stacks.Empty -> ())
	in
	  cl ()    
    end

    (** Compilation of forward chaining rules. *)
    module Chain: CLOSE = struct
      
      module D = struct
	
	class equalchain (id : Name.t) (subst : Term.t Name.Map.t) (hyps: Judgement.atoms) s t = 
	  (object
	     inherit Judgement.Top.equal
	     method lhs = s
	     method rhs = t
	     method hyps = hyps
	     method name =  Format.sprintf "chain[%s]" (Name.to_string id)
	     initializer 
	       if debug >= 4 then
		 Format.printf "\nchain[%s] ==> %s = %s " 
		   (Name.to_string id) (Term.to_string s) (Term.to_string t)
	   end: Judgement.equal)
    					  
	class diseqchain (id : Name.t) (subst : Term.t Name.Map.t) (hyps: Judgement.atoms) s t = 
	  (object (self)
	     inherit Judgement.Top.diseq
	     method lhs = s
	     method rhs = t
	     method hyps = hyps
	     method name = Format.sprintf "chain[%s]" (Name.to_string id)
	     initializer 
	       if debug >= 4 then
		 Format.printf "\nchain[%s] ==> %s <> %s " 
		   (Name.to_string id) (Term.to_string s) (Term.to_string t)
	   end: Judgement.diseq)
	  
	let mk_equalchain id subst hyps a b = new equalchain id subst hyps a b   
	let mk_diseqchain id subst hyps a b = new diseqchain id subst hyps a b
						
      end
	
      open Axioms
      module Vars = Name.Set   (* Set of logical variables. *)
      module Subst = Name.Map  (* Bindings [x |-> a], [x] a name and [a] a term. *)
	
      let on_vareq = ref []
      let on_vardiseq = ref []
      let on_flateq = ref []
			
      let is_flat t =
	not(Term.is_var t) &&
	(Term.Args.for_all Term.is_var (Term.args_of t))
	  
      let iterate_on_flateqs: ((Judgement.equal -> unit) -> unit) ref = 
	let donothing _ = () in
	  ref donothing
	    
      (** Compile a flat chaining rule into a sensor.
	For each hypothesis there is one such sensor function. *)
      let rec compile (id, hl, c) =
	let rec loop pre = function
	  | [] -> ()
	  | h :: post -> 
	      compile_hyp id h (pre @ post) c;
	      loop (h :: pre) post
	in
	  loop [] hl 
	    
      and compile_hyp id h hl c =
	match h with
	  | FlatChain.HypEqual(x, FlatChain.Var(y)) ->
	      on_vareq := compile_vareq id (x, y) hl c :: !on_vareq
	  | FlatChain.HypEqual(x, FlatChain.App(f, yl)) -> 
	      on_flateq := compile_flateq id (x, f, yl) hl c :: !on_flateq
	  | FlatChain.HypDiseq(x, y) ->
	      on_vardiseq :=  compile_diseq id (x, y) hl c :: !on_vardiseq
	      
      (** Returns sensor for a variable equality. *)
      and compile_vareq id (x, y) hl c =
	let hl' = FlatChain.reorder [x; y] hl in
	let on_vareq e =
	  let i = e#lhs and j = e#rhs in
	    assert(Term.is_var i && Term.is_var j);
	    let subst0 = Subst.add x i (Subst.add y j Subst.empty) 
	    in
	      on_satisfying id hl' c subst0 (J.mk_singleton (e:>Judgement.atom))
	in
	  on_vareq
	    
      (** Returns sensor for a flat equality [i = f(j1,...,jn)]. *)
      and compile_flateq id (x, f, yl) hl c =  
	let hl' = FlatChain.reorder (x :: yl) hl in 
	let on_flateq e =
	  let i = e#lhs and a = e#rhs in
	    assert(Term.is_var i && is_flat a);
	    let g = Term.sym_of a and jl = (Term.Args.to_list (Term.args_of a)) in
	      if Funsym.eq f g && 
		List.length yl = List.length jl 
	      then 
		let subst0 = 
		  Subst.add x i
		    (List.fold_right2 Subst.add yl jl Subst.empty)
		in
		  on_satisfying id hl' c subst0 (Judgement.mk_singleton (e:>Judgement.atom))
	in
	  on_flateq
	    
      (** Returns sensor for a variable disequality. *)
      and compile_diseq id (x, y) hl c =
	let hl' = FlatChain.reorder [x; y] hl in 
	let on_vardiseq d =
	  let i = d#lhs and j = d#rhs in
	    assert(Term.is_var i && Term.is_var j);
	    let subst0 = 
	      Subst.add x i 
		(Subst.add y j Subst.empty) 
	    in
	      on_satisfying id hl' c subst0 (Judgement.mk_singleton (d:>Judgement.atom));
	in
	  on_vardiseq
	    
      and on_satisfying id hl c subst hyps =
	match hl with
	  | [] -> 
	      deduce id c subst hyps
	  | FlatChain.HypEqual(x, FlatChain.Var(y)) :: hl' ->
	      on_satisfying_vareq id (x, y) hl' c subst hyps
	  | FlatChain.HypDiseq(x, y) :: hl' ->
	      on_satisfying_vardiseq id (x, y) hl' c subst hyps
	  | FlatChain.HypEqual(x, FlatChain.App(f, yl)) :: hl' -> 
	      on_satisfying_flateq id (x, f, yl) hl' c subst hyps
	    
      and on_satisfying_flateq id ((x, f, yl) as pat) hl c subst hyps = 
	!iterate_on_flateqs
	  (fun e -> 
	     let i = e#lhs and a = e#rhs in
	       assert(is_flat a);
	       let g = Term.sym_of a in
		 if Funsym.eq f g then
		   let jl = Term.Args.to_list (Term.args_of a) in
		     assert(List.for_all Term.is_var jl);
		     if List.length yl = List.length jl then
		       try
			 let subst' = match_flat pat (i, g, jl) subst in
			   on_satisfying id hl c subst' (Judgement.mk_add_equal e hyps)
		       with
			   Axioms.Bot -> ())
	  
      and match_flat ((x, f, yl) as pat) ((i, g, jl) as a) subst =
	let subst' = match_var x i subst in
	  match_vars yl jl subst'
	    
      and match_var x i subst =
	assert(Term.is_var i);
	try
	  let j = Subst.find x subst in
	    if Term.eq i j then subst else raise Bot
	with
	    Not_found -> Subst.add x i subst
	      
      and match_vars xl jl subst = 
	List.fold_right2 match_var xl jl subst
	  
      and on_satisfying_vareq id (x, y) hl c subst hyps =
	try
	  let i = Subst.find x subst and j = Subst.find y subst in
	    assert(Term.is_var i && Term.is_var j);
	    match V.Infsys.is_equal i j with
	      | Some(e) ->
		  on_satisfying id hl c subst (Judgement.mk_add_equal e hyps)
	      | None -> 
		  ()
	    with
		Not_found -> ()
		  
      and on_satisfying_vardiseq id (x, y) hl c subst hyps =
	try
	  let i = Subst.find x subst and j = Subst.find y subst in
	    assert(Term.is_var i && Term.is_var j);
	    (match V.Infsys.is_diseq i j with
	       | Some(d) -> on_satisfying id hl c subst (Judgement.mk_add_diseq d hyps)
	       | None -> ())
	with
	    Not_found -> ()
	      
      and deduce id c subst hyps =
	match c with
	  | FlatChain.ConclEqual(p, q) -> 
	      deduce_equal id (p, q) subst hyps
	  | FlatChain.ConclDiseq(p, q) ->  
	      deduce_diseq id (p, q) subst hyps
	      
      and deduce_equal id (p, q) subst hyps =
	try
	  let t1 = apply p subst and t2 = apply q subst in
	  let e = D.mk_equalchain id subst hyps t1 t2 in
	    Derived.add_equal e;
	with
	    Not_found -> ()
	      
      and deduce_diseq id (p, q) subst hyps =
	try
	  let x = apply p subst and y = apply q subst in
	    assert(Term.is_var x && Term.is_var y);
	    let d = D.mk_diseqchain id subst hyps x y in
	      Derived.add_diseq d
	with
	    Not_found -> ()
	      
      and apply p subst =
	let inst x = Subst.find x subst in
	  match p with
	    | FlatChain.Var(x) -> 
		inst x
	    | FlatChain.App(f, xl) -> 
		(* let il = List.map inst xl in *)
		let il = failwith "can: to do" in
		  Can.can f il
		    
      (** Do compilation for all chains *)
      let _ =
	List.iter
	  (fun ch -> 
	     Trace.call 10 "Compiling " ch Chain.pp;
	     let (id, hl, c) = FlatChain.of_chain ch in
	       compile (id, hl, c);
	       Trace.exit 10 "Compiling" (id, hl, c) FlatChain.pp)
	  Can.chains
	  
      let on_vareq e iter = 
	iterate_on_flateqs := iter;
	let apply f = f e in
	  List.iter apply !on_vareq
	    
      let on_vardiseq d iter = 
	iterate_on_flateqs := iter;
	let apply f = f d in
	  List.iter apply !on_vardiseq
	    
      let on_flateq e iter = 
	iterate_on_flateqs := iter;
	let apply f = f e in
	  List.iter apply !on_flateq
	    
    end
      
    let toplevel f a =
      Derived.init (); f a; Derived.close ()
	  
    let abstract t a =
      assert(is_pure t);
      let e = varify t in     (* [e |- y = t] for some variable [y]. *)
	assert(Term.is_var e#lhs);
	G.Infsys.put (J.mk_replace_in_atom (J.mk_sym e) a)
	  
    let process_equal e = 
      toplevel process_equal1 e
	
    let process_diseq d = 
      toplevel process_diseq1 d
	
    let rec propagate_equal x =  
      toplevel propagate_equal1 x
	
    and propagate_equal1 x =
      assert(Term.is_var x);
      if Config.occ x !s then
	let _, e = V.Infsys.can x in
	  fuse e;
	  collapse e;
	  Chain.on_vareq e iter
	
    and fuse e =
      let x = e#lhs and y = e#rhs in
	assert(Term.is_var x && Term.is_var y && not(Term.eq x y));
	try 
	  let zs = Config.dep !s x in
	    unchanged := false;
	    Dep.Set.iter
	      (fun z -> 
		 try
		   let es = Config.lookup !s z in
		     J.Equals.iter 
		       (fun e1 ->
			  let t = e1#rhs in
			    if Term.occurs x t then
			      let e2 = Config.Instantiate.justify e t in
				failwith "can.fuse: to do")	
		       es;
		     Dep.remove z x !s.Config.dep;
		     Dep.add z y !s.Config.dep
		 with
		     Not_found -> ())  (* dependency might be over-approximating. *)
	      zs;
	with
	    Not_found -> ()
	
    and collapse e =
      let x = e#lhs and y = e#rhs in
	assert(Term.is_var x && Term.is_var y && not(Term.eq x y));
	try
	  let ex = Config.lookup !s x 
	  and ey = try Config.Map.find y !s.Config.find with Not_found -> J.Equals.empty() in
	  let exy = J.Equals.copy ex in
	    J.Equals.union ey exy;
	    unchanged := false;
	    Config.Map.set y exy !s.Config.find;
	    Config.Map.remove x !s.Config.find
	with
	    Not_found -> ()
	      
    let rec propagate_diseq d =   
      toplevel propagate_diseq1 d;
      
    and propagate_diseq1 d =
      assert(Term.is_var d#lhs);
      assert(Term.is_var d#rhs);
      failwith "To do"
	(*
	  functionality d;
	  Can.Close.on_vardiseq d iter
	  
	(** [x = f(u1,...ri...,un), y = f(u1,...si...,un), x <> y ==> ri <> si] *)
	  and functionality d =
	  let x = d#lhs and y = d#rhs in
	  try
	  let ex = Config.Map.find x !s.Config.find 
	  and ey = Config.Map.find y !s.Config.find in
	  J.Equals.iter
	  (fun e1 -> 
	  assert(Term.eq x e1#lhs);
	  let t1 = e1#rhs in
	  J.Equals.iter
	  (fun e2 -> 
	  assert(Term.eq y e2#lhs);
	  let t2 = e2#rhs in
	  if Funsym.eq (Term.sym_of t1) (Term.sym_of t2) then
	  match Term.Args.eqbut (Term.args_of t1) (Term.args_of t2) with
	  | None -> ()
	  | Some(r, s) -> 
	  let d' = J.mk_func 7 (failwith "to do") in
	  V.Infsys.process_diseq d')
	  ey)
	  ex
	  with
	  Not_found -> ()
	*)
	
    (** No branching for convex theories. *)
    let branch () = 
      failwith "to do"
	(*
	  try
	  let dl = Can.disjunction iter in
	  Some(dl)
	  with
	  Not_found -> None
	*)
	
    let rec normalize _ =
      assert(Config.is_canonical (V.Infsys.current()) !s);
      ()
      
  end 

end

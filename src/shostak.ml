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
 
module type T = sig
  val th : Theory.t 
  val can : Term.interp
  val solve : Term.t -> Term.t -> Term.Set.t * Term.Subst.t
  val disjunction : ((Judgement.equal -> unit) -> unit) -> Judgement.disjunction
end

module type CONFIG = sig
  type t  
  val is_empty : t -> bool  
  val empty : unit -> t  
  val pp : Format.formatter -> t -> unit  
  val is_fresh : t -> Term.t -> bool
  val in_dom : t -> Term.t -> bool
  val in_cod : t -> Term.t -> bool
  val dep : t -> Term.t -> Dep.Set.t
  val occ : t -> Term.t -> bool
  val iter : t -> (Judgement.equal -> unit) -> unit
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

module type INFSYS = sig
  type config
  val current : unit -> config
  val initialize : config -> unit
  val finalize : unit -> config
  val reset : unit -> unit
  val is_unchanged : unit -> bool
  val abstract : Term.t -> Judgement.atom -> unit 
  val process_equal : Judgement.equal -> unit
  val process_diseq : Judgement.diseq -> unit
  val propagate_equal : Term.t -> unit
  val propagate_diseq : Judgement.diseq -> unit
  val branch : unit -> Judgement.disjunction option
  val normalize : unit -> unit
end

module type COMPONENT = sig
  val th : Theory.t
  module Config : CONFIG
  module Infsys : (INFSYS with type config = Config.t)
end

module Make(Sh: T): COMPONENT = struct

  let th = Sh.th

  module Map = Term.Map
  module Set = Term.Set
    
  let is_pure = Term.is_pure th

  let is_diseq t1 t2 = 
    try 
      let _ = Sh.solve t1 t2 in 
	false 
    with 
      | Exc.Inconsistent -> true
      | _ -> false
	  
  let is_equal t1 t2 = 
    try 
      let _ = Sh.solve t1 t2 in 
	false 
    with 
      | Exc.Valid -> true
      | _ -> false

  let map f t = 
    assert(is_pure t);
    let f' t = try f t with Not_found -> t in
    let rec mapf t =
      try
	let g = Term.sym_of t and a = Term.args_of t in
	let a' = Term.Args.map mapf a in
	  if Term.Args.eq a a' then t else Sh.can g a'
      with
	  Not_found -> f' t
    in
      mapf t
	
  let is_solved e = 
    let x = e#lhs and t = e#rhs in
      Term.is_var x && not(Term.occurs x t)
	
  (** Proof theory for a Shostak theory. *)
  module J = struct
    
    let mk_refl = Judgement.mk_refl
    let mk_sym = Judgement.mk_sym
    let mk_join = Judgement.mk_join
    let mk_trans = Judgement.mk_trans
    let mk_alias = Judgement.mk_alias
    let mk_bot = Judgement.mk_bot
    let mk_replace_in_atom = Judgement.mk_replace_in_atom
    let mk_replace_in_equal e1 e2 = Judgement.mk_replace_in_equal [e1; e2]
    let mk_replace_in_diseq = Judgement.mk_replace_in_diseq 
    let mk_transform_equal = Judgement.mk_transform_equal
    let mk_transform_rhs_equal = Judgement.mk_transform_rhs_equal
    let mk_transform_diseq = Judgement.mk_transform_diseq
			       
    let is_refl e = (e#lhs == e#rhs)
      
    let th = Theory.to_string th
	       
    (** If [e{i} |- x{i} = c{i}], then [apply [e{1};...;e{n}] |- t1 t2],
      where the canonical [b] is equal to [a[x{1}/c{1};...;x{n}/c{n}]] in the theory. *)
    class apply e t1 t2 = (object
      inherit Judgement.Top.equal
      method lhs = t1
      method rhs = t2
      method name = Format.sprintf "replace[%s]" th 
      method hyps = Judgement.mk_singleton (e:>Judgement.atom)
    end: Judgement.equal)
    
    let mk_apply e t =
      let lookup x = if Term.eq x e#lhs then e#rhs else x in
      let t' = map lookup t in
	if t == t' then Judgement.mk_refl t else
	  new apply e t t'
	    
    (** If [e |- t1 = t2] and [(x = t) in solve(t1 = t2)],
      then [solve e x t |- x = t]. *)
    class solve (e: Judgement.equal) (x: Term.t) (t: Term.t) = object
      inherit Judgement.Top.equal
      method lhs = x
      method rhs = t
      method name = Format.sprintf "solve[%s]" th
      method hyps = Judgement.mk_singleton(e:>Judgement.atom)
    end

    let mk_solve e x t = new solve e x t
	  
    (** If [e |- t1 = t2] with [t1 = t2] [th]-invalid,
      then [contra_equal e |- false]. *)
    class inconsistent (e: Judgement.equal) = (object
      inherit Judgement.Top.unsat
      method hyps = Judgement.mk_singleton (e:>Judgement.atom)
      method name = Format.sprintf "solve[%s]" th
      method validate = is_diseq e#lhs e#rhs
    end : Judgement.unsat)

    let mk_inconsistent e = new inconsistent e

  end

  (** Configurations of a Shostak inference system. *)
  module Config = struct

    type t = {
      mutable solset : Solset.t;
      mutable fresh : Set.t
    }

    let is_empty s = Solset.is_empty s.solset
		       
    let empty() = {
      solset = Solset.empty();
      fresh = Set.empty()
    }
		    
    let copy s = {
      solset = Solset.copy s.solset;
      fresh = Set.copy s.fresh
    }
		   
    let is_fresh s x = Set.mem x s.fresh
			 
    let in_dom s x = Solset.in_dom x s.solset
    let in_cod s x = Solset.in_cod x s.solset
    let is_dependent s x = Solset.in_dom x s.solset   
    let not_is_dependent s x = not (is_dependent s x)  
    let is_independent s x = Solset.in_cod x s.solset
    let occ s x = is_dependent s x || is_independent s x  
    let not_occ s x =  not (occ s x)
		
    module Apply = struct    
      let get s x = (Solset.apply s.solset x)#rhs
      let justify s = Solset.apply s.solset
    end 

    module Inv = struct
      let get s t = (Solset.inv s.solset t)#lhs
      let justify s = Solset.apply s.solset
    end
	  
    let dep s = Solset.dep s.solset
		  
    let model s = Solset.model s.solset
		    
    let pp fmt s = Solset.pp fmt s.solset
		     
    let invariant s =
      Solset.is_solved s.solset
	
    let is_canonical v s =
      Solset.is_canonical v s.solset
	
    let extend s e = 
      Solset.extend s.solset e
	
    let restrict s x = 
      Solset.restrict s.solset x
	
    let update s e =
      Solset.update s.solset e
	
    let normalize s =
      Set.iter
	(fun x -> 
	   if not(is_independent s x) then
	     Set.remove x s.fresh)
	s.fresh
	
    let iter s f = Solset.iter f s.solset

    module Replace = struct

      let get s t =
	let lookup y = (Solset.apply s.solset y)#rhs in
	  map lookup t 

      (** Iterate on all hypothesis of [t = Replace.get s t]. *)
      let iter f s t =
	assert(is_pure t);
	let apply_to_dom x = 
	  try f (Solset.apply s.solset x) with Not_found -> ()
	in
	  Term.iter apply_to_dom t

      (** Hypotheses for [t = Replace.get s t]. *)
      let hyps s t = 
	let acc = Judgement.Equals.empty () in
	  iter (fun e -> Judgement.Equals.add e acc) s t;
	  acc
	
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
	  new replace (hyps s t) t t'

    end
      
    module Diseq = struct

      let test s t1 t2 =
	try
	  let _ = Sh.solve (Replace.get s t1) (Replace.get s t2) in
	    false
	with
	  | Exc.Inconsistent -> true  (* could be memoized. *)
	  | _ -> false

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
	let es1 = Replace.hyps s t1 and es2 = Replace.hyps s t2 in
	  Judgement.Equals.union es1 es2;
	  new diseq es2 t1 t2

    end 

    module Equal = struct

      let test s t1 t2 = 
	Term.eq (Replace.get s t1) (Replace.get s t2)

      let justify s t1 t2 = 
	assert(test s t1 t2);
	let e1 = Replace.justify s t1      (* [e1 |- t1 = t]. *)
	and e2 = Replace.justify s t2 in   (* [e2 |- t2 = t]. *)
	  Judgement.mk_join e1 e2

    end 
      
  end 

  module Infsys = struct

    type config = Config.t

    (** Configuration of inference system. *)
    let s = ref (Config.empty())
    let unchanged = ref true
		      
    let current () = !s
		       
    let initialize s0 =
      assert(Config.invariant s0);
      unchanged := true; 
      s := s0
	
    let reset () =
      initialize (Config.empty())
	
    let protect () =     (* copy before first update. *)
      if !unchanged then 
	(unchanged := false; s := Config.copy !s)
	
    let finalize () = !s
			
    let is_unchanged () = !unchanged
			    
    (** Replace dependent variables in [s] with corresponding
      right hand sides (assumed to be canonical). *)
    let replace t =
      Config.Replace.justify !s t
	  
    (** Basic manipulations of configuration. *)
    module Update = struct
      
      let install ks = 
	protect (); Term.Set.union ks !s.Config.fresh
	  
      let restrict x =
	assert(Term.is_var x);
	if Config.in_dom !s x then 
	  (protect(); Config.restrict !s x)
	
      let alias e =
	assert(is_solved e);
	assert(not(Config.occ !s e#lhs)); 
	assert(not(Term.is_var e#rhs));
	protect(); Config.extend !s e
	  
      let rec extend e =         (* [e |- x = a] with [x] fresh. *)
	assert(is_solved e);
	assert(not(Config.occ !s e#lhs));
	if Term.is_var e#rhs then deduce e else 
	  try
	    let e' = Config.Inv.justify !s e#rhs in
	      protect(); deduce (J.mk_join e e')
	  with
	      Not_found -> protect(); Config.extend !s e
		
      and deduce e = 
	assert(Term.is_var e#lhs && Term.is_var e#rhs);
	if Config.is_fresh !s e#rhs then fuse e else
	  if Config.is_fresh !s e#lhs then fuse (J.mk_sym e) else
	    V.Infsys.process_equal e
	      
      and update e =                            (* [e |- x = b]. *)
	assert(is_solved e);
	assert(not(Config.occ !s e#lhs));
	assert(not(Term.is_var e#rhs));
	if Term.is_var e#rhs then (restrict e#lhs; deduce e) else 
	  try
	    let e' = Config.Inv.justify !s e#rhs in
	      deduce (J.mk_join e e')
	  with
	      Not_found -> protect(); Config.update !s e
		
      and fuse e =                               (* [e |- x = b]. *)
	assert(Config.invariant !s);
	let instr y = 
	  try
	    let e' = Config.Apply.justify !s y in (* [e' |- y = a]. *)
	    let e''= J.mk_transform_rhs_equal (J.mk_apply e) e' in  
	      update e''                     (* ==> [y = a[x:=b]. *)
	  with
	      Not_found -> invalid_arg "imprecise dependency"
	in
	  try 
	    Dep.Set.iter instr (Config.dep !s e#lhs);
            assert(Config.invariant !s);
	  with
	      Not_found -> ()
		
      let compose e =
	assert(is_solved e);
	assert(not(Config.occ !s e#lhs));
	fuse e;
	extend e;
	assert(Config.invariant !s)
	  
    end 
      
    (** Return a variable [x] for a term [t] with [x = t] in the (possibly extended) 
      current solution set. *)
    let var_of_pure t =
      assert(is_pure t);
      if Term.is_var t then J.mk_refl t else 
	try Config.Inv.justify !s t with Not_found ->
	  let v = Term.mk_fresh_var "v" in
	  let e = J.mk_alias v t in
	    Update.alias e;
	    e
	      
    let abstract t a =
      assert(is_pure t);
      let e = var_of_pure t in   (* [e |- y = t] for some variable [y]. *)
	assert(Term.is_var e#lhs);
	G.Infsys.put (J.mk_replace_in_atom (J.mk_sym e) a)
	  
    let rec process_equal e =
      assert(is_pure e#lhs && is_pure e#rhs);
      let e = J.mk_transform_equal replace e in
	try
	  let ks, sl = Sh.solve e#lhs e#rhs in
	    Term.Subst.iter
	      (fun x t -> Update.compose (J.mk_solve e x t))
	      sl;
	    Update.install ks
	with
	  | Exc.Inconsistent -> 
	      raise(Judgement.Unsat(J.mk_inconsistent e))
	  | Exc.Incomplete -> 
	      Update.deduce (J.mk_transform_equal var_of_pure e)
	  | Exc.Valid -> ()
	      
    let process_diseq d =                      (* [d |- a <> b]. *)
      assert(is_pure d#lhs && is_pure d#rhs);
      let d' = J.mk_transform_diseq replace d in
	V.Infsys.process_diseq
	  (J.mk_transform_diseq var_of_pure d')
	  
    let rec propagate_equal x =
      if Config.is_empty !s then () else
	if Config.occ !s x then
	  let _, e = V.Infsys.can x in   
	    assert(Term.eq x e#lhs && (not(Term.eq x e#rhs)));
	    propagate_equal1 e
	      
    and propagate_equal1 e =                     (* [e |- x = y]. *)
      assert(not(Term.eq e#lhs e#rhs));
      let x = e#lhs in
	(try
	   let e1 = Config.Inv.justify !s x in  (* [e1 |- x = t]. *)
	     Config.restrict !s x;  
	     let e' = J.mk_join (J.mk_sym e) (J.mk_sym e1) in
	       process_equal e'             (* ==> [e' |- y = t]. *)
	 with                
	     Not_found -> 
	       Dep.Set.iter
	       (fun z ->
	          try                   (* [e2 |- z = t[x]]. *)
		    let e2 = Config.Apply.justify !s z in
		      Config.restrict !s z;
		      process_equal (J.mk_transform_rhs_equal (J.mk_apply e) e2);
		  with
		      Not_found -> ())
	       (Config.dep !s x));
	assert(not(Config.occ !s x))
	  
   
    let rec propagate_diseq d =              (* [d |- t1 <> t2]. *)
      let t1 = d#lhs and t2 = d#rhs in
      if Config.is_empty !s then () else
	if Config.Equal.test !s t1 t2 then    (* [e |- t1 = t2]. *)
	  let e = Config.Equal.justify !s t1 t2 in
	    raise(Judgement.Unsat(Judgement.mk_contra e d))
	      
    let rec branch () = 
      try
	let cl = Sh.disjunction (Config.iter !s)  in
	  Some(cl)
      with
	  Not_found -> None
	    
    let normalize () =
      assert(Config.invariant !s);
      assert(Config.is_canonical (V.Infsys.current()) !s);
      Config.normalize !s
	
  end

end










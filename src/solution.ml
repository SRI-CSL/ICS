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

(** Datatype of functional equality sets. *)


(** Iterators over {i dependency} index. *)
module type DEP = sig
  type eqs
  val iter : eqs -> (Fact.Equal.t -> unit) -> Term.t -> unit 
  val fold : eqs -> (Fact.Equal.t -> 'a -> 'a) -> Term.t -> 'a  -> 'a 
  val for_all : eqs -> (Fact.Equal.t -> bool) -> Term.t -> bool 
  val exists : eqs -> (Fact.Equal.t -> bool) -> Term.t -> bool  
  val choose : eqs -> (Fact.Equal.t -> bool) -> Term.t -> Fact.Equal.t
end


module type SET = sig
  type t 
  type ext
  val eq : t -> t -> bool
  val pp : t Pretty.printer
  val empty : t
  val is_empty : t -> bool
  val is_dependent : t -> Term.t -> bool
  val is_independent : t -> Term.t -> bool  
  val iter : (Fact.Equal.t -> unit) -> t -> unit
  val fold : (Fact.Equal.t -> 'a -> 'a) -> t -> 'a -> 'a
  val for_all : (Fact.Equal.t -> bool) -> t -> bool
  val to_list : t -> Fact.Equal.t list 
  val equality : t -> Term.t -> Fact.Equal.t
  val apply : t -> Jst.Eqtrans.t
  val find : t -> Jst.Eqtrans.t
  val inv : t -> Jst.Eqtrans.t  
  val replace : t -> Jst.Eqtrans.t
  val dep : t -> Term.t -> Term.Var.Set.t
  val ext : t -> ext
  module Dep : DEP with type eqs = t
  val restrict : t -> Term.t -> t
  type config = Partition.t * t
  val update : config -> Fact.Equal.t -> config 
  val fuse: config -> Fact.Equal.t list -> config  
  val fuse1: config -> Fact.Equal.t -> config
  val compose : config -> Fact.Equal.t list -> config 
  val diff : t -> t -> t 
  val copy : t -> t
end

module type EXT = sig
  type t
  val pp: t Pretty.printer
  val empty : t
  val update : t -> Fact.Equal.t -> t
  val restrict : t -> Fact.Equal.t -> t
end


module type TH = sig
  val th : Th.t
  val map : (Term.t -> Term.t) -> Term.t -> Term.t
end



(** Dependency Index *)
module Use = struct
  
  type t = Term.Var.Set.t Term.Var.Map.t

  let mem = Term.Var.Map.mem

  let apply u a = Term.Var.Map.find a u

  let find u a =
    try Term.Var.Map.find a u with Not_found -> Term.Var.Set.empty

  let set = Term.Var.Map.add

  let empty = Term.Var.Map.empty

  let pp_set fmt us =
    Pretty.set Term.pp fmt (Term.Var.Set.elements us)

  (** [add x y m] adds [x] to the use of [y]. *)
  let add1 x y m =
    try 
      let uy = Term.Var.Map.find y m in
      let uy' = Term.Var.Set.add x uy in
	if uy == uy' then m else Term.Var.Map.add y uy' m
    with
	Not_found -> 
	  Term.Var.Map.add y (Term.Var.Set.singleton x) m

  (** [add x a use] adds [x] to the use of [y] for each toplevel
    uninterpreted term in [a]. *)
  let add x = Term.fold (add1 x)

  (** [remove x y s] deletes [x] from the use of [y]. *)
  let remove1 x y m = 
    try 
      let uy = Term.Var.Map.find y m in
      let uy' = Term.Var.Set.remove x uy in
	if Term.Var.Set.is_empty uy' then
	  Term.Var.Map.remove y m
	else if uy == uy' then 
	  m
	else
	  Term.Var.Map.add y uy' m
    with
	Not_found -> m

  (** [remove x a m] deletes [x] from the use of [y] for each toplevel
    uninterpreted term in [a]. *)
  let remove x = Term.fold (remove1 x)

  (** [remove_but b x a m] deletes [x] from the use of [y] for each toplevel
    uninterpreted term in [a] only if [y] does not occur in [b]. *)
  let remove_but b x =
    Term.fold
      (fun y acc ->
	 if Term.subterm y b then acc else 
	   remove1 x y acc)

  (** Pretty-printing. *)
  let rec pp fmt u =
    Pretty.map Term.pp (Pretty.set Term.pp) fmt (to_list u)

  and to_list u =
    Term.Var.Map.fold (fun x ys acc -> (x, Term.Var.Set.elements ys) :: acc) u []
end


let pp_index = ref false


(** {6 Solution sets with extension fields} *)
module Make(T: TH)(Ext: EXT): (SET with type ext = Ext.t) = struct
  
  (** [x |-> (a, rho)] in [find] represent the equality [x = a] 
    with justification [rho]. We also write [rho |- x = a]. 
    [inv] is just the inverse [a |-> x] for every such entry in [find].
    [dep] indexes for each nondependent variables the set of variables 
    dependent on it, and [ext] is an extension field. *)
  type t = {
    mutable find: Fact.Equal.t Term.Var.Map.t;
    mutable dep : Use.t;                       (* dependency index for variables *)
    mutable extension : Ext.t;
  }

      
  let level = Th.to_string T.th ^ "''"
		
  type ext = Ext.t
      
  let eq s1 s2 = (s1.find == s2.find)

  let empty = {
    find = Term.Var.Map.empty;
    dep = Use.empty;
    extension = Ext.empty;
  }

  let iter f s = 
    let f' _ e = f e in
      Term.Var.Map.iter f' s.find
 
  let fold f s =
    let f' _ e = f e in
      Term.Var.Map.fold f' s.find

  exception Not_holds

  let for_all f s = 
    let f' _ e = f e in
      try
	Term.Var.Map.iter 
	  (fun _ e -> if not(f e) then raise Not_holds)
	  s.find;
	true
      with
	  Not_holds -> false

  let to_list s = 
    let cons e l = e :: l in
      fold cons s []

  let is_empty s = (s.find == Term.Var.Map.empty)

  let is_dependent s x = Term.Var.Map.mem x s.find

  let equality s = function
    | Term.App _ -> 
	raise Not_found  (* Invariant: only vars in domain of [s]. *)
    | x -> 
	Term.Var.Map.find x s.find

  let apply s x =
    let e = equality s x in
    let (y, a, rho) = Fact.Equal.destruct e in
      assert(Term.eq x y);
      (a, rho)

  let find s = Jst.Eqtrans.totalize (apply s)

  let dep s = Use.find s.dep

  let ext s = s.extension

  exception Found of Term.t * Jst.t

  (** Return [(x, rho)] if [rho |- x = a] is in [s]. 
    If [a] is a constant, then traverse the data structure
    to find the inverse.  Otherwise, the dependency index
    on some variable [y] in [a] is used to find [x = a]. *)
  let rec inv s a =
    if Term.is_const a then
      inv_cnstnt s a
    else 
      inv_noncnstnt s a
	
  and inv_cnstnt s a =
    try
      iter
	(fun e ->
	   let (x, b, rho) = Fact.Equal.destruct e in
	     if Term.eq a b then 
	       raise (Found(x, rho)))
	s;
      raise Not_found
    with
	Found(x, rho) -> (x, rho)
	
  and inv_noncnstnt s a =
    let x = Term.choose Term.is_var a in
      try
	Term.Var.Set.iter
	  (fun y ->
	     let (b, rho) = apply s y in
	       if Term.eq a b then
		 raise(Found(y, rho)))
	  (dep s x);
	raise Not_found
      with
	  Found(y, rho) -> (y, rho)

  
  let is_independent s x =
    not(Term.Var.Set.is_empty (dep s x))
 
  let pp fmt s =
    let el = to_list s in
      if not(el = []) then
	begin
	  Pretty.set Fact.Equal.pp fmt (to_list s);
	  if !pp_index then
	    begin
	      Format.fprintf fmt "\ndep: ";
	      Use.pp fmt s.dep;
	    end 
	end
	
   (** Iterators *)
   module Dep = struct 
     type eqs = t
     let iter s f y = 
       let xs = dep s y in
       let apply_to x =    (* [s] might have changed. *)
	 try f (equality s x) with Not_found -> () 
       in
	 Term.Var.Set.iter apply_to (dep s y)

     let apply_to_e s f x = 
       let e = 
	 try 
	   equality s x
	 with 
	     Not_found -> 
	       failwith ("Overapproximating dependency for: " ^ (Term.to_string x))
       in
	 f e
                                          
     let fold s f y = Term.Var.Set.fold (apply_to_e s f) (dep s y)
     let for_all s p y = Term.Var.Set.for_all (apply_to_e s p) (dep s y)
     let exists s p y = Term.Var.Set.exists (apply_to_e s p) (dep s y)

     exception Found of Fact.Equal.t
       
     let choose s p y =
       try
	 Term.Var.Set.iter
	   (fun x ->
	      try
		let e = equality s x in
		  if p e then 
		    raise(Found(e))
	      with
		  Not_found -> ())
	   (dep s y);
	 raise Not_found
       with
	   Found(e) -> e
   end

   let restrict s x =
     Trace.msg level "Restrict" x Term.pp;
     try
       let e = equality s x in
       let (y, b, rho) = Fact.Equal.destruct e in
	 assert(Term.eq x y);
	 (* assert(not(Term.is_const b)); *)
	 assert(Term.Var.Map.mem x s.find);
	 let find' = Term.Var.Map.remove x s.find in
	 let dep' = Use.remove x b s.dep in
	   (* assert(not(Term.Var.Map.mem x s.find)); *)
	   let ext' = Ext.restrict s.extension e in
	     {find = find'; 
	      dep = dep';
	      extension = ext'}
     with
	 Not_found -> s
	 
   let add s e =  
     Trace.msg level "Add" e Fact.Equal.pp;
     let (x, b, rho) = Fact.Equal.destruct e in
       assert(Term.is_var x);
       try                  (* restrict, then update. *)
	 let e' = equality s x in
	 let (x', b', rho') = Fact.Equal.destruct e' in
	   assert(Term.eq x x');
	   let dep = Use.remove_but b x b' s.dep in
	   let dep' = Use.add x b dep in
	   let find' = Term.Var.Map.add x e s.find in
	   let ext' = Ext.update (Ext.restrict s.extension e') e in
           if !Tools.destructive then 
	     (s.find <- find'; s.dep <- dep'; s.extension <- ext'; s)
	   else 
	     {find = find'; dep = dep'; extension = ext'}
       with 
	   Not_found ->     (* extend *)
	     let dep' = Use.add x b s.dep
	     and find' = Term.Var.Map.add x e s.find 
	     and ext' = Ext.update s.extension e in
	       if !Tools.destructive then
		 begin
		   assert(not(s == empty));
		   (s.find <- find'; s.dep <- dep'; s.extension <- ext'; s)
		 end 
	       else 
		 {dep = dep'; find = find'; extension =  ext'}
	  

   (** [replace s a] one-step replaces dependent variables [x] in [a] 
     with [b] if [x = b] in [s]. *)
   let replace s a =
     let hyps = ref Jst.dep0 in
     let lookup y = 
       try
	 let (b, rho) = apply s y in
	   hyps := Jst.dep2 rho !hyps; b
       with
	   Not_found -> y
     in
    let b = T.map lookup a in
      (b, !hyps)

   type config = Partition.t * t


   let copy s = 
     if !Tools.destructive then 
       {find = s.find; dep = s.dep; extension = s.extension}
     else 
       s

   let copy = 
     Trace.func level "Copy" pp pp copy

   (** Add equality and propagate possible variable equalities to 
     variable partitioning [p]. *)
   let rec update (p, s) e =
     Trace.msg level "Update" e Fact.Equal.pp;
     let (x, b, rho) = Fact.Equal.destruct e in   (* [rho |- x = b]. *)
       assert(Term.is_var x);  (* allow for equalities [x = y] with [y] internal. *)
       if Term.is_var b && not(Term.Var.is_fresh T.th b) then
	 (Partition.merge p e, restrict s x)    
       else                        
	 try
	   let (y, tau) = inv s b in              (* [tau |- y = b]. *)
	     if Term.eq x y then 
	       (p, s) 
	     else 
	       let e' = Fact.Equal.make (x, y, Jst.dep2 rho tau) in
	       let p = merge p e' in
	       let s = 
		 if Term.(<<<) y x then
		   restrict s x  (* [y] 'more canonical' than [x]. *)
		 else 
		   add (restrict s y) e
	       in
		 (p, s)
	 with
	     Not_found -> 
	       (p, add s e)

   (** Variable equalities [x = y] with [x], [y] internal can be dropped. *)
   and merge p e =
     if Fact.Equal.both_sides (Term.Var.is_fresh T.th) e 
     then p 
     else Partition.merge p e
       
   (** Fuse a solution set [t] into [s] by replacing occurrences
     of [x] in [b] with [a], where [y = b] in [s], and [x = a] in [t]. 
     - [fuse s {} = s]
     - [fuse s ({x = a} union t) = fuse (fuse1 s {x = a}) t] *)
   let rec fuse (p, s) tl =
     let fs1 e (p, s) = fuse1 (p, s) e in
       List.fold_right fs1 tl (p, s)

       
   (** Fusing a single equality [x = a] into [s] by replacding
     occurrences of [x] in [b] with [a] for all [y = b] in [s]. *)
   and fuse1 (p, s) t1 = 
     let x = Fact.Equal.lhs_of t1 in
     let instantiate e (p, s) = 
       let e' = Fact.Equal.map_rhs (inst t1) e in
	 update (p, s) e'
     in
       Dep.fold s instantiate x (p, s)

   and inst e a = 
     let (x, b, rho) = Fact.Equal.destruct e in
     let lookup y = if Term.eq x y then b else y in
     let a' = T.map lookup a in
       if a == a' then Jst.Eqtrans.id a else (a', rho)

   (** Fusing a list of solved equalities [t] on rhs 
     followed by updates of [el]. *)
   let rec compose (p, s) tl =
     let (p, s) = fuse (p, s) tl in
       union (p, s, tl)

   (** Union of two solution sets [s], [t] with disjoint set of
     dependent variables. *)
   and union (p, s, tl) =
     let upd e (p, s) = 
       let x = Fact.Equal.lhs_of e in     (* drop [x = a] with [x] an *)
	 if Term.Var.is_fresh T.th x then (* internal variable. *)
	   (p, s)
	 else
	   update (p, s) e
     in
       List.fold_right upd tl (p, s)

       
   let diff s1 s2 =
     fold
       (fun e acc ->
	  let (x, a, rho) = Fact.Equal.destruct e in
	    try
	      let (b, tau) = apply s2 x in
		if Term.eq a b then acc else add acc e
	    with
		Not_found -> add acc e)
       s1 (copy empty)
    
end

(** Theory-specification without side effects. *)
module type TH0 = sig
  val th : Th.t
  val map : (Term.t -> Term.t) -> Term.t -> Term.t
end

module Ext0 = struct
  type t = unit
  let pp = Pretty.unit
  let empty = ()
  let restrict () _ = ()
  let update () _ = ()
end

module type SET0 = (SET with type ext = unit)

module Make0(T: TH): SET0 = 
  Make(T)(Ext0)


(** Projection to nonextendable set. *)
module Proj(S: SET): SET0 = struct
  
  type t = S.t

  type ext = unit
   
  let eq = S.eq
  let empty = S.empty
  let iter = S.iter
  let fold = S.fold
  let for_all = S.for_all
  let to_list = S.to_list
  let is_empty = S.is_empty
  let is_dependent = S.is_dependent
  let equality = S.equality
  let apply = S.apply
  let find = S.find
  let dep = S.dep
  let ext _ = ()
  let inv = S.inv
  let is_independent = S.is_independent
  let pp = S.pp
  module Dep = S.Dep 
  let replace = S.replace
  let restrict = S.restrict
  type config = S.config
  let update = S.update
  let fuse = S.fuse
  let fuse1 = S.fuse1
  let compose = S.compose
  let diff = S.diff
  let copy = S.copy
end

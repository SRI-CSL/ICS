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



(** {6 Dependency Index} *)

let pp_index = ref false

module Use = struct
  
  type t = Term.Set.t Term.Map.t

  let mem = Term.Map.mem

  let apply u a = Term.Map.find a u

  let find u a =
    try Term.Map.find a u with Not_found -> Term.Set.empty

  let set = Term.Map.add

  (** empty use list. *)
  let empty = Term.Map.empty

  let pp_set fmt us =
    Pretty.set Term.pp fmt (Term.Set.elements us)

  (** [add x y m] adds [x] to the use of [y]. *)
  let add1 x y m =
    Trace.msg "use" "Add" (x, y) (Pretty.pair Term.pp Term.pp);
    try 
      let uy = Term.Map.find y m in
      let uy' = Term.Set.add x uy in
	if uy == uy' then m else Term.Map.add y uy' m
    with
	Not_found -> 
	  Term.Map.add y (Term.Set.singleton x) m

  (** [add x a use] adds [x] to the use of [y] for each toplevel
    uninterpreted term in [a]. *)
  let add x = Term.fold (add1 x)

  (** [remove x y s] deletes [x] from the use of [y]. *)
  let remove1 x y m = 
    Trace.msg "use" "Rem" (x, y) (Pretty.pair Term.pp  Term.pp);
    try 
      let uy = Term.Map.find y m in
      let uy' = Term.Set.remove x uy in
	if Term.Set.is_empty uy' then
	  Term.Map.remove y m
	else if uy == uy' then 
	  m
	else
	  Term.Map.add y uy' m
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
    Term.Map.fold (fun x ys acc -> (x, Term.Set.elements ys) :: acc) u []
end


(** {Equality Sets} *)

(** Specification of an equality theory. *)
module type TH = sig
  val th : Th.t
  val nickname : string
  val apply : Term.Equal.t -> Term.t -> Term.t
end

type effects = 
    (equality -> unit) * 
    (equality -> unit)

and equality = Term.t * Term.t * Justification.t


let effects0 =
  let f _ = () 
  and g _ = () in
    (f, g)


(** Signature for equality sets with dependency index. *)
module type SET = sig
  type t 
  val eq : t -> t -> bool
  val pp : t Pretty.printer
  val empty : t
  val is_empty : t -> bool
  val is_dependent : t -> Term.t -> bool
  val is_independent : t -> Term.t -> bool
  val fold : (Term.t -> Term.t * Justification.t -> 'a -> 'a) -> t -> 'a -> 'a
  val to_list : t -> (Term.t * Term.t) list
  val apply : t -> Justification.Eqtrans.t
  val equality : t -> Term.t -> Fact.Equal.t
  val find : t -> Justification.Eqtrans.t
  val inv : t -> Justification.Eqtrans.t 
  val dep : t -> Term.t -> Term.Set.t
  val index : t -> int -> Term.Set.t
  val cnstnt : t -> Term.Set.t
  module Dep : sig
    val iter : t -> (Fact.Equal.t -> unit) -> Term.t -> unit 
    val fold : t -> (Fact.Equal.t -> 'a -> 'a) -> Term.t -> 'a  -> 'a
    val for_all : t -> (Fact.Equal.t -> bool) -> Term.t -> bool
    val exists : t -> (Fact.Equal.t -> bool) -> Term.t -> bool
    val choose : t -> Term.t -> Fact.Equal.t
  end
  val copy : t -> t
  type config = Partition.t * t
  val name:  effects -> config -> Justification.Eqtrans.t
  val update : effects -> config -> Fact.Equal.t -> unit
  val restrict : effects -> config -> Term.t -> unit
  val fuse: effects -> config -> Fact.Equal.t list -> unit
  val compose : effects -> config -> Fact.Equal.t list -> unit

end


module Make(Th: TH): SET = struct

  type t = {
    mutable find: (Term.t * Justification.t) Term.Map.t;
    mutable inv : Term.t Term.Map.t;
    mutable dep : Use.t
  }

  let eq s1 s2 = (s1.find == s2.find)

  let empty = {
    find = Term.Map.empty;
    inv = Term.Map.empty;
    dep = Use.empty
  }

  let copy s = {
    find = s.find;
    inv = s.inv;
    dep = s.dep
  }

  let is_empty s = (s.find == Term.Map.empty)

  let is_dependent s x = Term.Map.mem x s.find
  
  let dep s = Use.find s.dep

  let is_independent s x =
    not(Term.Set.is_empty (dep s x))
 
  let fold f s = Term.Map.fold f s.find

  let to_list s = fold (fun x (b,_) acc -> (x, b) :: acc) s []

  let pp fmt s =
    let el = to_list s in
      if not(el = []) then
	begin
	  Format.fprintf fmt "\n%s: " Th.nickname;
	  Pretty.set (Pretty.eqn Term.pp) fmt (to_list s);
	  if !pp_index then
	    begin
	      Format.fprintf fmt "\nuse: ";
	      Use.pp fmt s.dep;
	    end 
	end

  let apply s a =
    match a with
      | Term.App _ -> raise Not_found  (* Invariant: only vars in domain of [s]. *)
      | _ -> Term.Map.find a s.find

  let equality s a =
    let (b, rho) = apply s a in
      Fact.Equal.make (a, b, rho)

  let equality s =
    Trace.func "foo8" "equality" Term.pp Fact.Equal.pp (equality s)

  let find s a =
    match a with
      | Term.App _ -> 
	  Justification.Eqtrans.id a
      | _ -> 
	  (try 
	     Term.Map.find a s.find 
	   with 
	       Not_found -> Justification.Eqtrans.id a)

  let inv s a =
    let x = Term.Map.find a s.inv in
    let (_, rho) = apply s x in
      (x, rho)

  let index _ _ =
    raise(Invalid_argument("No indices defined for " ^ Th.nickname))

  let cnstnt _ =
    raise(Invalid_argument("No constant index defined for " ^ Th.nickname))

   (** {6 Iterators} *)

   module Dep = struct
     let apply_to_e s f x = 
       let e = 
	 try 
	   equality s x
	 with 
	     Not_found -> 
	       failwith ("Overapproximating dependency for: " ^ (Term.to_string x))
       in
	 f e
                        
     let iter s f y = Term.Set.iter (apply_to_e s f) (dep s y)
     let fold s f y = Term.Set.fold (apply_to_e s f) (dep s y) 
     let for_all s p y = Term.Set.for_all (apply_to_e s p) (dep s y)
     let exists s p y = Term.Set.exists (apply_to_e s p) (dep s y)

     let choose s y =
       let x = Term.Set.choose (dep s y) in
       let (b, rho) = find s x in
	 assert(not(x == b));
	 Fact.Equal.make (x, b, rho)
   end

   (** {6 Internal Update Functions} *)
     
   type config = Partition.t * t
       
   let do_add =
     let trace_add _ = () in
       ref trace_add
	 
   let do_restrict =
     let trace_remove _ = () in
       ref trace_remove
	 
   let add (p, s) e =   
     Trace.msg Th.nickname "Update" e Fact.Equal.pp; 
     let (x, b, rho) = Fact.Equal.destruct e in
       assert(Term.is_var x);
       (try  
	  let (b', rho') = apply s x in 
	    (* s.dep <- Use.remove_but b x b' s.dep; *)  (* this needs to be fixed. *)
	    s.dep <- Use.remove x b' s.dep;
	    s.dep <- Use.add x b s.dep;
	    !do_restrict (x, b, rho')
	with 
	    Not_found ->
              s.dep <- Use.add x b s.dep);
       s.find <- Term.Map.add x (b, rho) s.find;
       s.inv <- Term.Map.add b x s.inv;
       !do_add (x, b, rho)
	 
   let remove (p, s) x =
     try
       let (b, rho) = Term.Map.find x s.find in
	 Trace.msg Th.nickname "Restrict" (x, b) Term.Equal.pp;
	 s.find <- Term.Map.remove x s.find;
	 s.inv <- Term.Map.remove b s.inv;
	 s.dep <- Use.remove x b s.dep; 
	 !do_restrict (x, b, rho)
     with
	 Not_found -> () 
	   
	   
   let rec upd (p, s) e =
     let (x, b, rho1) = Fact.Equal.destruct e in  (* [rho1 |- x = b]. *)     
       if Term.eq x b then
	 remove (p, s) x 
       else if Term.is_var b then
	 begin
	   Partition.merge p e;     (* propagate new variable equality. *)
	   merge (p, s) e           (* and merge in solution set *)
	 end 
       else
	 try
	   let (y, rho2) = inv s b in           (* [rho2 |- y = b]. *)
	     if not(Term.eq x y) then
	       let rho' =                       (* [rho' |- x = y]. *)          
		 Justification.trans (x, b, y) 
		   rho1 (Justification.sym (b, y) rho2)
	       in
		 Partition.merge p (Fact.Equal.make (x, y, rho'));
		 if Term.(<<<) y x then 
		   remove (p, s) x 
		 else 
		   begin
		     remove (p, s) y;
		     add (p, s) e
		   end 
	 with
	     Not_found -> 
	       add (p, s) e
	       
   and merge (p, s) e =
     let (x, y, rho1) = Fact.Equal.destruct e in
       try                                        (* [rho1 |- x = y]. *)
	 let (a, rho2) = apply s y in             (* [ rho2 |- y = a]. *)
	   if Term.(<<<) y x then 
	     remove (p, s) x 
	   else 
	     let rho = Justification.trans (x, y, a) rho1 rho2 in
	       begin                               (* [rho |- x = a]. *)
		 remove (p, s) y;
		 add (p, s) (Fact.Equal.make (x, a, rho))
	       end 
       with
	   Not_found -> 
	     remove (p, s) x
	     
	     
   (** {6 External Update Functions} *)

   let restrict (f, g) (p, s) =
     do_add := f; do_restrict := g;
     remove (p, s)

   let update (f, g) (p, s) =
     do_add := f; do_restrict := g;
     upd (p, s)

   (** Return a canonical variable [x] equal to [b]. If [b] is not a rhs in
     the equality set for theory [i], then a variable [x] is newly created. *)
   let name (f, g) (p, s) b =
     if Term.is_var b then 
       Justification.Eqtrans.id b
     else
       try 
	 inv s b 
       with 
	   Not_found ->
	     do_add := f;
	     do_restrict := g;
	     let dom = try Some(Arith.dom_of b) with Not_found -> None in
	     let x = Term.Var.mk_rename (Name.of_string "v") None dom in 
	     let rho = Justification.extend (x, b) in
	       upd (p, s) (Fact.Equal.make (x, b, rho));
	       (x, rho)
	       

   (** Propagating a list of solved equalities on rhs. *)
   let rec fuse (f, g) (p, s) el =
     do_add := f; do_restrict := g;
     fuse_star (p, s) el
       
   and fuse_star (p, s) el =   
     let norm = Fact.Equal.Inj.norm Th.apply in
       List.iter
	 (fun e ->
	    (Dep.iter s
	       (fun e -> 
		  let e' = Fact.Equal.map_rhs (norm el) e in
		    upd (p, s) e')
	       (Fact.Equal.lhs_of e)))
	 el
     
   (** Fuse a list of solved equalities [el] on rhs 
     followed by updates of [el]. *)
   let compose (f, g) (p, s) el =
     do_add := f; do_restrict := g;
     fuse_star (p, s) el;
     List.iter (upd (p, s)) el

end



(** {6 Equality Theories with indices} *)

module type INDEX = sig
  val max : int       
  val name: int -> string
  val holds : int -> Term.t -> bool
end

module MakeIndex(Th: TH)(Idx : INDEX): SET = struct

  module Eqs = Make(Th)

  type t = { 
    eqs : Eqs.t; 
    mutable idx : Term.Set.t array }

  let eq s1 s2 = Eqs.eq s1.eqs s2.eqs

  let empty = { eqs = Eqs.empty; idx = Array.make Idx.max Term.Set.empty }
  let copy s = { eqs = Eqs.copy s.eqs; idx = Array.copy s.idx }
 
  let eqs s = s.eqs

  let index s = Array.get s.idx

  let is_empty s = Eqs.is_empty s.eqs
  let is_dependent s = Eqs.is_dependent s.eqs
  let is_independent s = Eqs.is_independent s.eqs
  let fold f s = Eqs.fold f s.eqs
  let to_list s = Eqs.to_list s.eqs

  let apply s = Eqs.apply s.eqs
  let equality s = Eqs.equality s.eqs
  let find s = Eqs.find s.eqs
  let inv s = Eqs.inv s.eqs
  let dep s = Eqs.dep s.eqs

  let cnstnt _ =
    raise(Invalid_argument("No constant index defined for" ^ Th.nickname))

  let pp fmt s =
    if not(is_empty s) then
      begin
	Eqs.pp fmt s.eqs;
	if !pp_index then
	  for i = 0 to Idx.max-1 do
	    Format.fprintf fmt "\n%s: " (Idx.name i);
            Pretty.set Term.pp fmt (Term.Set.elements (index s i))
	  done
      end 

  module Dep = struct
    let iter s = Eqs.Dep.iter s.eqs
    let fold s = Eqs.Dep.fold s.eqs
    let for_all s = Eqs.Dep.for_all s.eqs
    let exists s = Eqs.Dep.exists s.eqs
    let choose s = Eqs.Dep.choose s.eqs
  end

  type config = Partition.t * t

  (** Apply [proc] to extended side effects. *)
  let with_extended_effects proc (do_add, do_restrict) (p, s) = 
    let do_add' ((x, a, _) as e) =
      for i = 0 to Idx.max-1 do
	if Idx.holds i a then
	  Array.set s.idx i (Term.Set.add x (Array.get s.idx i))
      done;
      do_add e
    and do_restrict' ((x, a, _) as e) =
      for i = 0 to Idx.max-1 do
	let xs = Array.get s.idx i in
	let xs' = Term.Set.remove x xs in
	  if not(xs == xs') then
	    Array.set s.idx i xs'
      done;
      do_restrict e
    in
      proc (do_add', do_restrict') (p, s.eqs)

  let name = with_extended_effects Eqs.name
  let restrict = with_extended_effects Eqs.restrict
  let update = with_extended_effects Eqs.update
  let fuse = with_extended_effects Eqs.fuse
  let compose = with_extended_effects Eqs.compose

end


(** {6 Solution sets with constant index} *)


module type CNSTNT = sig
  val is_const : Term.t -> bool
  val is_diseq : Term.t -> Term.t -> bool
end 

module MakeIndexCnstnt(Th: TH)(Idx: INDEX)(C: CNSTNT) = struct

    (** Encode constant index as a new index at position [max]. *)
  module Idx1 = struct
    let max = Idx.max + 1
    let name i = 
      assert(0 <= i && i <= Idx.max);
      if i < Idx.max then Idx.name i else "cnstnt"
    let holds i =
      assert(0 <= i && i <= Idx.max);
      if i < Idx.max then Idx.holds i else C.is_const
  end

  module Eqs = MakeIndex(Th)(Idx1)

  type t = Eqs.t
      
  let eq = Eqs.eq
  let empty = Eqs.empty
  let copy = Eqs.copy

  let rec index s i =
    assert(i < Idx.max);
    let xs = Eqs.index s i in
      assert(Term.Set.for_all (index_checker s i) xs);
      xs

  and index_checker s i x =
    try
      let (a, _) = Eqs.apply s x in
      let result = Idx1.holds i a in
	if not(result) then
	   failwith (Format.sprintf "Invariant violated for index %s and equality %s in state %s"
		       (Idx.name i) 
		       (Pretty.to_string Term.Equal.pp (x, a))
		       (Pretty.to_string Eqs.pp s));
	result
    with
	Not_found ->
	  failwith (Format.sprintf "Wrong Index %s: no rhs for %s in state \n%s"
		      (Idx.name i) (Term.to_string x) (Pretty.to_string Eqs.pp s));
	  false

  let rec cnstnt s = 
    let xs = Eqs.index s Idx.max in
      assert(Term.Set.for_all (cnstnt_checker s) xs);
      xs

  and cnstnt_checker s x =
    try
      let (a, _) = Eqs.apply s x in
      let result = C.is_const a in
	if not(result) then
	  failwith (Format.sprintf "Invariant violated for constant index %s 
                    and equality %s in state %s"
		      "cnstnt" 
		      (Pretty.to_string Term.Equal.pp (x, a))  
		      (Pretty.to_string Eqs.pp s));
	result
    with
	Not_found -> 
	  failwith (Format.sprintf "Wrong Constant index %s: no rhs for %s in state \n%s"
		     "cnstnt"  (Term.to_string x) (Pretty.to_string Eqs.pp s));
	  false

  let is_empty = Eqs.is_empty
  let is_dependent = Eqs.is_dependent
  let is_independent = Eqs.is_independent
  let fold = Eqs.fold
  let to_list = Eqs.to_list
  let apply = Eqs.apply
  let equality = Eqs.equality
  let find = Eqs.find
  let inv = Eqs.inv
  let dep = Eqs.dep
  let pp = Eqs.pp

  module Dep = struct
    let iter = Eqs.Dep.iter
    let fold = Eqs.Dep.fold
    let for_all = Eqs.Dep.for_all
    let exists = Eqs.Dep.exists
    let choose = Eqs.Dep.choose
  end

  type config = Partition.t * t

  (** Apply [proc] to extended side effects, which consists of
    generating disequalities [x <> y] for constant equalities 
    [x = a] and [y = b] with [a <> b]. *)
  let with_extended_effects proc (do_add, do_restrict) (p, s) = 
    let do_add' ((x, a, rho) as e) =
      do_add e;
      Term.Set.iter                    (* [rho |- x = a] *)
	(fun y ->
	   let (b, tau) = find s y in  (* [tau |- y = b] *)
	     assert(C.is_const b && not(y == b));
	     if C.is_diseq a b then       (* [sigma |- x <> y] *)
	       let sigma = Justification.dependencies [rho; tau] in
	       let d = Fact.Diseq.make (x, y, sigma) in
		 Partition.dismerge p d)
	(cnstnt s)
    in
      proc (do_add', do_restrict) (p, s)

  let name = with_extended_effects Eqs.name
  let restrict = with_extended_effects Eqs.restrict
  let update = with_extended_effects Eqs.update
  let fuse = with_extended_effects Eqs.fuse
  let compose = with_extended_effects Eqs.compose

end

module type INDEX1 = sig
  val name : string
  val holds : Term.t -> bool
end

module OfIndex1(Idx1: INDEX1): INDEX = struct
  let max = 1
  let name _ = Idx1.name
  let holds _ = Idx1.holds
end

module MakeIndexCnstnt1(Th: TH)(Idx1: INDEX1)(C: CNSTNT) = 
  MakeIndexCnstnt(Th)(OfIndex1(Idx1))(C)



module Idx0: INDEX = struct
  let max = 0
  let name _ = assert false
  let holds _ = assert false
end

module MakeCnstnt(Th: TH)(C: CNSTNT) =
  MakeIndexCnstnt(Th)(Idx0)(C)



(** {6 Side-effect free equality sets} *)


module type SET0 = sig
  type t 
  type ext
  val eq : t -> t -> bool
  val pp : t Pretty.printer
  val empty : t
  val is_empty : t -> bool
  val is_dependent : t -> Term.t -> bool
  val is_independent : t -> Term.t -> bool
  val fold : (Term.t -> Term.t * Justification.t -> 'a -> 'a) -> t -> 'a -> 'a
  val to_list : t -> (Term.t * Term.t) list
  val apply : t -> Justification.Eqtrans.t
  val equality : t -> Term.t -> Fact.Equal.t
  val find : t -> Justification.Eqtrans.t
  val inv : t -> Justification.Eqtrans.t 
  val dep : t -> Term.t -> Term.Set.t
  val index : t -> int -> Term.Set.t
  val cnstnt : t -> Term.Set.t
  val ext : t -> ext
  module Dep : sig
    val iter : t -> (Fact.Equal.t -> unit) -> Term.t -> unit 
    val fold : t -> (Fact.Equal.t -> 'a -> 'a) -> Term.t -> 'a  -> 'a
    val for_all : t -> (Fact.Equal.t -> bool) -> Term.t -> bool
    val exists : t -> (Fact.Equal.t -> bool) -> Term.t -> bool
    val choose : t -> Term.t -> Fact.Equal.t
  end
  val copy : t -> t
  type config = Partition.t * t
  val name:  config -> Justification.Eqtrans.t
  val update : config -> Fact.Equal.t -> unit
  val restrict : config -> Term.t -> unit
  val fuse: config -> Fact.Equal.t list -> unit
  val compose : config -> Fact.Equal.t list -> unit
end

module type EXT = sig
  type t
  type ext
  val empty : ext
  val pp : ext Pretty.printer
  val eq : ext -> ext -> bool
  val do_at_add :  Partition.t * ext * t -> equality -> ext
  val do_at_restrict : Partition.t * ext * t -> equality -> ext
end


module Extend(Set: SET)(Ext: EXT with type t = Set.t): (SET0 with type ext = Ext.ext) = struct
  type t = {eqs : Set.t; mutable ext : Ext.ext}
  type ext = Ext.ext
  let eq s t = Set.eq s.eqs t.eqs && Ext.eq s.ext t.ext
  let pp fmt s = Set.pp fmt s.eqs; if !pp_index then Ext.pp fmt s.ext
  let empty = {eqs = Set.empty; ext = Ext.empty}
  let is_empty s = Set.is_empty s.eqs
  let is_dependent s = Set.is_dependent s.eqs
  let is_independent s = Set.is_independent s.eqs
  let fold f s = Set.fold f s.eqs
  let to_list s = Set.to_list s.eqs
  let apply s = Set.apply s.eqs
  let equality s = Set.equality s.eqs
  let find s = Set.find s.eqs
  let inv s = Set.inv s.eqs
  let dep s = Set.dep s.eqs
  let index s = Set.index s.eqs
  let cnstnt s = Set.cnstnt s.eqs
  let ext s = s.ext
  module Dep = struct
    let iter s = Set.Dep.iter s.eqs
    let fold s = Set.Dep.fold s.eqs
    let for_all s = Set.Dep.for_all s.eqs
    let exists s = Set.Dep.exists s.eqs
    let choose s = Set.Dep.choose s.eqs
  end
  let copy s = {eqs = Set.copy s.eqs; ext = s.ext}   
  type config = Partition.t * t
  let effects cfg = 
    let do_at_update (p, s) e =
      s.ext <- Ext.do_at_add (p, s.ext, s.eqs) e
    and do_at_restrict (p, s) e =
      s.ext <- Ext.do_at_restrict (p, s.ext, s.eqs) e
    in
      (do_at_update cfg, do_at_restrict cfg)

  let inj f ((p, s) as cfg) = f (effects cfg) (p, s.eqs)

  let name = inj Set.name
  let update = inj Set.update
  let restrict = inj Set.restrict
  let fuse = inj Set.fuse
  let compose = inj Set.compose
end
 
  
module Close(Set: SET): SET0 with type ext = unit =
  Extend
    (Set)
    (struct
       type t = Set.t
       type ext = unit
       let empty = ()
       let eq _ _ = true
       let pp _ _ = ()
       let do_at_add _ _ = () 
       let do_at_restrict _ _ = ()
     end)


(** {6 Combining two equality sets} *)

type tag = Left | Right

let other =
  function 
    | Left -> Right
    | Right -> Left

module type EFFECTS2 = sig
  type left
  type right
  val do_at_update : tag -> Partition.t * left * right -> equality -> unit
  val do_at_restrict : tag -> Partition.t * left * right -> equality -> unit
end


module Union
  (Left: SET)
  (Right: SET)
  (Effects2: EFFECTS2 with type left = Left.t with type right = Right.t) =
struct

  type t = { left : Left.t; right : Right.t }
      
  let eq s t = Left.eq s.left t.left && Right.eq s.right t.right
		 
  let pp fmt s = Left.pp fmt s.left; Right.pp fmt s.right
    
  let empty = { left = Left.empty; right = Right.empty}
		
  let copy s = {left = Left.copy s.left; right = Right.copy s.right}
		 
  let is_empty s = Left.is_empty s.left && Right.is_empty s.right
		     
  (** Depending on value of [tag] apply either [f_left] to left part of
    state or [f_right] to the right part. *)
  let case_tag f_left f_right tag s =
    match tag with
      | Left -> f_left s.left
      | Right -> f_right s.right

  let is_dependent = case_tag Left.is_dependent Right.is_dependent
  let is_independent = case_tag Left.is_independent Right.is_independent
  let fold tag f = case_tag (Left.fold f) (Right.fold f) tag
  let to_list = case_tag Left.to_list Right.to_list
  let apply = case_tag Left.apply Right.apply
  let equality = case_tag Left.equality Right.equality
  let find = case_tag Left.find Right.find
  let inv = case_tag Left.inv Right.inv
  let dep = case_tag Left.dep Right.dep
  let index = case_tag Left.index Right.index
  let cnstnt = case_tag Left.cnstnt Right.cnstnt

  module Dep = struct
    let iter = case_tag Left.Dep.iter Right.Dep.iter
    let fold tag = case_tag Left.Dep.fold Right.Dep.fold tag
    let for_all = case_tag Left.Dep.for_all Right.Dep.for_all
    let exists = case_tag Left.Dep.for_all Right.Dep.for_all
    let choose = case_tag Left.Dep.choose Right.Dep.choose
  end
    
  type config = Partition.t * t

  (** Combined effects *)
  let effects tag (p, s) = 
    let do_at_update tag ((x, a, rho) as e) =
      Effects2.do_at_update tag (p, s.left, s.right) e;
      (try                           (* establish confluence *)
	 (match tag with
	    | Right ->
		let (y, tau) = Left.inv s.left a in        (* [tau |- y = a] *)
		let  sigma = Justification.trans (x, a, y) tau rho in
		  Partition.merge p (Fact.Equal.make (x, y, sigma));
		  Left.restrict effects0 (p, s.left) y (* restrict [y = a] in [Left]. *)
	    | Left ->
		let (y, tau) = Right.inv s.right a in   (* [tau |- y = a] *)
		let  sigma = Justification.trans (x, a, y) tau rho in
		  Partition.merge p (Fact.Equal.make (x, y, sigma));
		  Left.restrict effects0 (p, s.left) x)  (* restrict [x = a] in [Left] *)
       with
	   Not_found -> ())
    and do_at_restrict tag =
      Effects2.do_at_restrict tag (p, s.left, s.right)
    in
      (do_at_update tag, do_at_restrict tag)


  (** Depending on [tag] call either the update function [f_left]
    on the left configuration [(p, s.left)] or [f_right] on the
    right configuration [f_right]. Also, the effects are extended
    according to the parameter specification [Effects2]. *)
  let update_case_tag f_left f_right tag (p, s) =
    let effects' = effects tag (p, s) in
      match tag with
	| Left -> f_left effects' (p, s.left)
	| Right -> f_right effects' (p, s.right)
	    
  let name = update_case_tag Left.name Right.name
  let update = update_case_tag Left.update Right.update
  let restrict = update_case_tag Left.restrict Right.restrict
  let fuse = update_case_tag Left.fuse Right.fuse
  let compose = update_case_tag Left.compose Right.compose

end
    


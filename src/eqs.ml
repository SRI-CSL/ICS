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

let pp_index = ref false


(** {6 Dependency Index} *)

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


(** {6 Equality Sets} *)

(** Specification of an equality theory. *)
module type TH = sig
  val th : Th.t
  val nickname : string
  val apply : Term.Equal.t -> Term.t -> Term.t
  val is_infeasible : Justification.Pred2.t
end

type equality = Term.t * Term.t * Justification.t


(** Signature for equality sets with dependency index. *)
module type SET = sig
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

(** {6 Extensions} *)

module type EXT = sig
  type t
  val empty : t
  val pp : t Pretty.printer
  val eq : t -> t -> bool
  val do_at_add :  Partition.t * t -> equality -> t
  val do_at_restrict : Partition.t * t -> equality -> t
end

module Ext0: (EXT with type t = unit) =
struct
  type t = unit
  let empty = ()
  let eq _ _ = true
  let pp _ _ = ()
  let do_at_add _ _ = () 
  let do_at_restrict _ _ = ()
end

module CombineExt(Left: EXT)(Right: EXT): EXT =
  struct
    type t = Left.t * Right.t
    let pp fmt (l, r) = 
      Left.pp fmt l; Right.pp fmt r
    let empty = (Left.empty, Right.empty)
    let eq (l1, r1) (l2, r2) = 
      Left.eq l1 l2 && Right.eq r1 r2
    let do_at_add (p, (l, r)) e =
      (Left.do_at_add (p, l) e, 
       Right.do_at_add (p, r) e)
    let do_at_restrict (p, (l, r)) e =
      (Left.do_at_restrict (p, l) e,
       Right.do_at_restrict (p, r) e)
  end


(** {6 Solution sets with extension fields} *)

module Make(Th: TH)(Ext: EXT): (SET with type ext = Ext.t) = struct
  
  (** [x |-> (a, rho)] in [find] represent the equality [x = a] with justificationo [rho] *)
  type t = {
    mutable find: (Term.t * Justification.t) Term.Map.t;
    mutable inv : Term.t Term.Map.t;          (* inverse find *)
    mutable dep : Use.t;                      (* dependency index *)
    mutable ext : Ext.t                       (* extension field *)
  }

  type ext = Ext.t

  let eq s1 s2 = (s1.find == s2.find)

  let empty = {
    find = Term.Map.empty;
    inv = Term.Map.empty;
    dep = Use.empty;
    ext = Ext.empty;
  }

  let copy s = {
    find = s.find;
    inv = s.inv;
    dep = s.dep;
    ext = s.ext;
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
	      Ext.pp fmt s.ext
	    end 
	end

  let apply s a =
    match a with
      | Term.App _ -> raise Not_found  (* Invariant: only vars in domain of [s]. *)
      | _ -> Term.Map.find a s.find

  let equality s a =
    let (b, rho) = apply s a in
      Fact.Equal.make (a, b, rho)

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

  let ext s = s.ext


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
	 
   let add (p, s) e =   
     Trace.msg Th.nickname "Update" e Fact.Equal.pp; 
     let (x, b, rho) = Fact.Equal.destruct e in
       assert(Term.is_var x);
       match Th.is_infeasible x b with
	 | None ->
	     (try                  (* restrict, then update. *)
		let (b', rho') = apply s x in 
		  s.dep <- Use.remove_but b x b' s.dep;  (* this needs to be fixed. *)
		  (* s.dep <- Use.remove x b' s.dep; *)
		  s.dep <- Use.add x b s.dep; 
		  s.ext <- Ext.do_at_restrict (p, s.ext) (x, b, rho');
		  s.find <- Term.Map.add x (b, rho) s.find;
		  s.inv <- Term.Map.add b x s.inv;
		  s.ext <- Ext.do_at_add (p, s.ext) (x, b, rho)
	      with 
		  Not_found ->     (* extend *)
		    begin
		      s.dep <- Use.add x b s.dep;
		      s.find <- Term.Map.add x (b, rho) s.find;
		      s.inv <- Term.Map.add b x s.inv;
		      s.ext <- Ext.do_at_add (p, s.ext) (x, b, rho)
		    end)
	 | Some(tau) -> 
	     let sigma = Justification.dependencies [rho; tau] in
	       raise(Justification.Inconsistent(tau))

	 
   let restrict (p, s) x =
     try
       let (b, rho) = Term.Map.find x s.find in
	 Trace.msg Th.nickname "Restrict" (x, b) Term.Equal.pp;
	 s.find <- Term.Map.remove x s.find;
	 s.inv <- Term.Map.remove b s.inv;
	 s.dep <- Use.remove x b s.dep; 
	 s.ext <- Ext.do_at_restrict (p, s.ext) (x, b, rho)
     with
	 Not_found -> () 
	   
   
   let rec update (p, s) e =
     let (x, b, rho1) = Fact.Equal.destruct e in  (* [rho1 |- x = b]. *)     
       if Term.eq x b then
	 restrict (p, s) x 
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
		   restrict (p, s) x 
		 else 
		   begin
		     restrict (p, s) y;
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
	     restrict (p, s) x 
	   else 
	     let rho = Justification.trans (x, y, a) rho1 rho2 in
	       begin                               (* [rho |- x = a]. *)
		 restrict (p, s) y;
		 add (p, s) (Fact.Equal.make (x, a, rho))
	       end 
       with
	   Not_found -> 
	     restrict (p, s) x
	     

   (** Return a canonical variable [x] equal to [b]. If [b] is not a rhs in
     the equality set for theory [i], then a variable [x] is newly created. *)
   let name (p, s) b =
     if Term.is_var b then 
       Justification.Eqtrans.id b
     else
       try 
	 inv s b 
       with 
	   Not_found ->
	     let dom = try Some(Arith.dom_of b) with Not_found -> None in
	     let x = Term.Var.mk_rename (Name.of_string "v") None dom in 
	     let rho = Justification.extend (x, b) in
	       update (p, s) (Fact.Equal.make (x, b, rho));
	       (x, rho)
	       

   (** Propagating a list of solved equalities on rhs. *)
   let rec fuse (p, s) el =
     fuse_star (p, s) el
       
   and fuse_star (p, s) el =   
     let norm = Fact.Equal.Inj.norm Th.apply in
       List.iter
	 (fun e ->
	    (Dep.iter s
	       (fun e -> 
		  let e' = Fact.Equal.map_rhs (norm el) e in
		    update (p, s) e')
	       (Fact.Equal.lhs_of e)))
	 el
     
   (** Fuse a list of solved equalities [el] on rhs 
     followed by updates of [el]. *)
   let compose (p, s) el =
     fuse_star (p, s) el;
     List.iter (update (p, s)) el

end

module type SET0 = SET with type ext = unit

module Make0(Th: TH): (SET with type ext = unit) =
  Make(Th)(Ext0)


(** {6 Equality Theories with indices} *)

module type INDEX = sig
  val name: string
  val holds : Term.t -> bool
end

module Idx2Ext(Idx: INDEX): (EXT with type t = Term.Set.t) =
  struct
    type t = Term.Set.t
    let pp fmt s =   
      Format.fprintf fmt "\n%s: " Idx.name;
      Pretty.set Term.pp fmt (Term.Set.elements s)
    let empty = Term.Set.empty 
    let eq = (==)
    let do_at_add (p, s) (x, a, _) = 
      if Idx.holds a then Term.Set.add x s else s
    let do_at_restrict (p, s) (x, a, _) =
      Term.Set.remove x s
end


module MakeIndex(Th: TH)(Idx : INDEX): (SET with type ext = Term.Set.t) =
  Make(Th)(Idx2Ext(Idx))


(** {6 Constant extensions} *)

module type CNSTNT = sig
  val is_const : Term.t -> bool
  val is_diseq : Term.t -> Term.t -> bool
end 

module Cnstnt2Ext(Cnstnt: CNSTNT): (EXT with type t = (Term.t * Justification.t) Term.Map.t) =
  struct
    type t = (Term.t * Justification.t) Term.Map.t
    let pp fmt s =
      if not(s == Term.Map.empty) then
	begin
	  Format.fprintf fmt "\ncnstnt: ";
	  failwith "to do"
	end 
    let empty = Term.Map.empty
    let eq = (==)
	       (** Generate disequalities [x <> y] for constant equalities 
		 [x = a] and [y = b] with [a <> b]. *)
    let do_at_add (p, s) (x, a, rho) = 
      Term.Map.iter                         (* [rho |- x = a] *)
	(fun y (b, tau) ->                  (* [tau |- y = b] *)
	   if Cnstnt.is_diseq a b then      (* [sigma |- x <> y] *)
	     let sigma = Justification.dependencies [rho; tau] in
	     let d = Fact.Diseq.make (x, y, sigma) in
	       Partition.dismerge p d)
      s;
      if Cnstnt.is_const a then Term.Map.add x (a, rho) s else s
    let do_at_restrict (_, s) (x, a, _) =
      if Cnstnt.is_const a then Term.Map.remove x s else s
end

module MakeCnstnt(Th: TH)(Cnstnt: CNSTNT)
  : (SET with type ext = (Term.t * Justification.t) Term.Map.t) =
  Make(Th)(Cnstnt2Ext(Cnstnt))



(** {6 Solution sets with constant index} *)

module MakeIndexCnstnt(Th: TH)(Idx: INDEX)(Cnstnt: CNSTNT) =
  Make(Th)(CombineExt(Idx2Ext(Idx))(Cnstnt2Ext(Cnstnt)))



(** {6 Combining two equality sets} *)

module type SET2 = sig
  type t 
  type ext
  type lext
  type rext
  val eq : t -> t -> bool
  val pp : t Pretty.printer
  val empty : t
  val is_empty : t -> bool
  type tag = Left | Right
  val is_dependent : tag -> t -> Term.t -> bool
  val is_independent :  tag -> t -> Term.t -> bool
  val fold :  tag -> (Term.t -> Term.t * Justification.t -> 'a -> 'a) -> t -> 'a -> 'a
  val to_list :  tag -> t -> (Term.t * Term.t) list
  val apply :  tag -> t -> Justification.Eqtrans.t
  val equality :  tag -> t -> Term.t -> Fact.Equal.t
  val find :  tag -> t -> Justification.Eqtrans.t
  val inv :  tag -> t -> Justification.Eqtrans.t 
  val dep :  tag -> t -> Term.t -> Term.Set.t
  val ext : t -> ext * lext * rext
  module Dep : sig
    val iter :  tag -> t -> (Fact.Equal.t -> unit) -> Term.t -> unit 
    val fold :  tag -> t -> (Fact.Equal.t -> 'a -> 'a) -> Term.t -> 'a  -> 'a
    val for_all :  tag -> t -> (Fact.Equal.t -> bool) -> Term.t -> bool
    val exists :  tag -> t -> (Fact.Equal.t -> bool) -> Term.t -> bool
    val choose :  tag -> t -> Term.t -> Fact.Equal.t
  end
  val copy : t -> t
  type config = Partition.t * t
  val name:   tag -> config -> Justification.Eqtrans.t
  val update :  tag -> config -> Fact.Equal.t -> unit
  val restrict :  tag -> config -> Term.t -> unit
  val fuse:  tag -> config -> Fact.Equal.t list -> unit
  val compose :  tag -> config -> Fact.Equal.t list -> unit
end


module Union(Left: SET)(Right: SET)(Ext: EXT) = struct

  type t = {                    (* to do: establish confluence across theories. *)
    mutable left : Left.t; 
    mutable right : Right.t; 
    mutable ext : Ext.t 
  }

  type ext = Ext.t
  type lext = Left.ext
  type rext = Right.ext
      
  let eq s t = 
    Left.eq s.left t.left && 
    Right.eq s.right t.right
		 
  let pp fmt s = 
    Left.pp fmt s.left;
    Right.pp fmt s.right;
    if !pp_index then 
      Ext.pp fmt s.ext
      
  let empty = { 
    left = Left.empty; 
    right = Right.empty; 
    ext = Ext.empty
  }
		
  let copy s = {
    left = Left.copy s.left; 
    right = Right.copy s.right; 
    ext = s.ext
  }
		                                                    
  let is_empty s = 
    Left.is_empty s.left && 
    Right.is_empty s.right

  type tag = Left | Right
	     
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
  let ext s = (s.ext, Left.ext s.left, Right.ext s.right)

  let apply2 s x =
    try apply Left s x with Not_found -> apply Right s x

  module Dep = struct
    let iter = case_tag Left.Dep.iter Right.Dep.iter
    let fold tag = case_tag Left.Dep.fold Right.Dep.fold tag
    let for_all = case_tag Left.Dep.for_all Right.Dep.for_all
    let exists = case_tag Left.Dep.for_all Right.Dep.for_all
    let choose = case_tag Left.Dep.choose Right.Dep.choose
  end
    
  type config = Partition.t * t

  (** Establish confluence across solution sets; if [x = a] in [Left]
    and [y = a] in Right, then [x = y]. *)
  let do_at_add tag (p, s) e =
    let (x, a, rho) = Fact.Equal.destruct e in
      try                         
	(match tag with
	   | Right ->
	       let (y, tau) = Left.inv s.left a in        (* [tau |- y = a] *)
	       let  sigma = Justification.trans (x, a, y) tau rho in
		 Partition.merge p (Fact.Equal.make (x, y, sigma));
		 Left.restrict (p, s.left) y              (* restrict [y = a] in [Left]. *)
	   | Left ->
	       let (y, tau) = Right.inv s.right a in      (* [tau |- y = a] *)
	       let  sigma = Justification.trans (x, a, y) tau rho in
		 Partition.merge p (Fact.Equal.make (x, y, sigma));
		 Left.restrict (p, s.left) x)             (* restrict [x = a] in [Left] *)
      with
	  Not_found -> ()
	    
  (** Depending on [tag] call either the update function [f_left]
    on the left configuration [(p, s.left)] or [f_right] on the
    right configuration [f_right]. *)
  let update_case_tag f_left f_right tag (p, s) =
    match tag with 
      | Left -> f_left (p, s.left)   
      | Right -> f_right (p, s.right)
	  
  let name = update_case_tag Left.name Right.name
	       
  let update tag cfg e =                                
    update_case_tag Left.update Right.update tag cfg e;
    do_at_add tag cfg e                        (* establish confluence *)
      
  let restrict = update_case_tag Left.restrict Right.restrict
		   
  let fuse = update_case_tag Left.fuse Right.fuse

  let compose tag cfg el =
    update_case_tag Left.compose Right.compose tag cfg el;
    List.iter (do_at_add tag cfg) el            (* establish confluence *)

end
    


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

let publish th p e =
  let (x, y, _) = Fact.Equal.destruct e in 
    if not(Term.Var.is_fresh th x)      (* fresh variables of theory are *)
      || not(Term.Var.is_fresh th y)    (* considered to be internal. *)
    then 
      Partition.merge p e


(** {6 Dependency Index} *)

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


(** {6 Equality Sets} *)

(** Specification of an equality theory. *)
module type TH = sig
  val th : Th.t
  val nickname : string
  val map : Term.map
  val is_infeasible : Term.Equal.t -> bool
end

type equality = Term.t * Term.t * Jst.t


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
  val iter : (Term.t -> Term.t * Jst.t -> unit) -> t -> unit
  val fold : (Term.t -> Term.t * Jst.t -> 'a -> 'a) -> t -> 'a -> 'a
  val to_list : t -> (Term.t * Term.t) list
  val apply : t -> Jst.Eqtrans.t
  val equality : t -> Term.t -> Fact.Equal.t
  val find : t -> Jst.Eqtrans.t
  val inv : t -> Jst.Eqtrans.t 
  val dep : t -> Term.t -> Term.Var.Set.t
  val ext : t -> ext
  module Dep : sig
    val iter : t -> (Fact.Equal.t -> unit) -> Term.t -> unit 
    val fold : t -> (Fact.Equal.t -> 'a -> 'a) -> Term.t -> 'a  -> 'a
    val for_all : t -> (Fact.Equal.t -> bool) -> Term.t -> bool
    val exists : t -> (Fact.Equal.t -> bool) -> Term.t -> bool
    val choose : t -> (Fact.Equal.t -> bool) -> Term.t -> Fact.Equal.t
  end
  val copy : t -> t
  type config = Partition.t * t
  val name:  config -> Jst.Eqtrans.t
  val update : config -> Fact.Equal.t -> unit
  val restrict : config -> Term.t -> unit
  val fuse: config -> Fact.Equal.t list -> unit
  val compose : config -> Fact.Equal.t list -> unit
end

(** {6 Extensions} *)

type find = (Term.t * Jst.t) Term.Var.Map.t

module type EXT = sig
  type t
  val empty : t
  val pp : t Pretty.printer
  val do_at_add :  Partition.t * t * find -> equality -> t
  val do_at_restrict : Partition.t * t * find -> equality -> t
end

module Ext0: (EXT with type t = unit) =
struct
  type t = unit
  let empty = ()
  let pp _ _ = ()
  let do_at_add _ _ = () 
  let do_at_restrict _ _ = ()
end

module CombineExt(Left: EXT)(Right: EXT): EXT with type t = Left.t * Right.t =
  struct
    type t = Left.t * Right.t
    let pp fmt (l, r) = 
      Left.pp fmt l; Right.pp fmt r
    let empty = 
      (Left.empty, Right.empty)
    let do_at_add (p, (l, r), s) e =
      (Left.do_at_add (p, l, s) e, 
       Right.do_at_add (p, r, s) e)
    let do_at_restrict (p, (l, r), s) e =
      (Left.do_at_restrict (p, l, s) e, 
       Right.do_at_restrict (p, r, s) e)
  end


(** {6 Solution sets with extension fields} *)

module Make(Th: TH)(Ext: EXT): (SET with type ext = Ext.t) = struct
  
  (** [x |-> (a, rho)] in [find] represent the equality [x = a] 
    with justification [rho]. We also write [rho |- x = a]. 
    [inv] is just the inverse [a |-> x] for every such entry in [find].
    [dep] indexes for each nondependent variables the set of variables 
    dependent on it, and [ext] is an extension field. *)
  type t = {
    mutable find: (Term.t * Jst.t) Term.Var.Map.t;
    mutable inv : Term.t Term.Map.t;          (* inverse find *)
    mutable dep : Use.t;                      (* dependency index *)
    mutable ext : Ext.t                       (* extension field *)
  }

  type ext = Ext.t

  let eq s1 s2 = (s1.find == s2.find)

  let empty = {
    find = Term.Var.Map.empty;
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

  let is_empty s = (s.find == Term.Var.Map.empty)

  let is_dependent s x = Term.Var.Map.mem x s.find
  
  let dep s = Use.find s.dep

  let is_independent s x =
    not(Term.Var.Set.is_empty (dep s x))
 
  let iter f s = Term.Var.Map.iter f s.find
  let fold f s = Term.Var.Map.fold f s.find

  let to_list s = fold (fun x (b, rho) acc -> (x, b, rho) :: acc) s []

  let pp fmt s =
    let el = to_list s in
      if not(el = []) then
	begin
	  let eqn fmt (x, b, rho) = 
	    Fact.pp_justification fmt rho;
	    Pretty.infix Term.pp "=" Term.pp fmt (x, b)
	  in
	  Format.fprintf fmt "\n%s: " Th.nickname;
	  Pretty.set eqn fmt (to_list s);
	  if !pp_index then
	    begin
	      Format.fprintf fmt "\nuse: ";
	      Use.pp fmt s.dep;
	      Ext.pp fmt s.ext
	    end 
	end

  let to_list s = fold (fun x (b, _) acc -> (x, b) :: acc) s []

  let synchronized s =
    let res1 = 
      Term.Var.Map.fold
	(fun x (a, _) acc -> 
	   acc &&  
	   let res = Term.Map.mem a s.inv in
	     if not res then
	       Format.eprintf "\n%s not in inv@." (Pretty.to_string Term.Equal.pp (x, a));
	     res)
	s.find true
    and res2 = 
      Term.Map.fold
	(fun a x acc -> 
	   acc && 
	   let res = Term.Var.Map.mem x s.find in
	     if not res then
	       begin
		 Format.eprintf "\n%s not in find@." (Pretty.to_string Term.Equal.pp (x, a));
		 Format.eprintf "\n IN FIND \n";
		 pp Format.err_formatter s;
		 Format.eprintf "\n AND INV \n";
		 let l = Term.Map.fold (fun a x acc -> (a, x) :: acc) s.inv [] in
		   Pretty.set (Pretty.infix Term.pp "=" Term.pp) Format.err_formatter l
	       end;
	     res)
	s.inv true
    in
      res1 && res2


  let apply s a =
    assert(synchronized s);
    match a with
      | Term.App _ -> raise Not_found  (* Invariant: only vars in domain of [s]. *)
      | _ -> Term.Var.Map.find a s.find

  let equality s a =
    let (b, rho) = apply s a in
      Fact.Equal.make (a, b, rho)

  let find s a = 
    assert(synchronized s);
    match a with
      | Term.App _ -> 
	  Jst.Eqtrans.id a
      | _ -> 
	  (try 
	     Term.Var.Map.find a s.find 
	   with 
	       Not_found -> Jst.Eqtrans.id a)

  let inv s a =
    let x = Term.Map.find a s.inv in
      assert(Term.is_var x);
      assert(Term.Var.Map.mem x s.find);
	let (b, rho) = apply s x in
	  assert(if Term.eq a b then true else
                   begin
		     Format.eprintf "\nFatal Error: Inv is inconsistent with Find\n@.";
		     Format.eprintf "\nInv: %s" (Pretty.to_string (Pretty.pair Term.pp Term.pp) (x, a));
                     Format.eprintf "\nFind: %s@." (Pretty.to_string (Pretty.pair Term.pp Term.pp) (x, b));
		     false
		   end);
	  (x, rho)
    
  let ext s = s.ext

   (** {6 Iterators} *)

   module Dep = struct 

     let iter s f y = 
       let xs = dep s y in
       let apply_to x =             (* [s] might have changed. *)
	 try f (equality s x) with Not_found -> () 
       in
	 Term.Var.Set.iter apply_to (dep s y)

     let apply_to_e s f x = 
       let e = 
	 try 
	   equality s x
	 with 
	     Not_found -> 
	       pp_index := true;
	       pp Format.err_formatter s;
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

   (** {6 Internal Update Functions} *)

   let trace_level = Th.nickname ^ "''"
     
   type config = Partition.t * t
	 
   let add (p, s) e =   
     Trace.msg trace_level "Update" e Fact.Equal.pp; 
     assert(synchronized s);
     (let (x, b, rho) = Fact.Equal.destruct e in
       assert(Term.is_var x);
       if Th.is_infeasible (x, b) then
	 raise(Jst.Inconsistent rho)
       else 
	 (try                  (* restrict, then update. *)
	    let (b', rho') = apply s x in 
	      s.dep <- Use.remove_but b x b' s.dep; 
	      s.dep <- Use.add x b s.dep; 
	      s.ext <- Ext.do_at_restrict (p, s.ext, s.find) (x, b, rho');
	      s.find <- Term.Var.Map.add x (b, rho) s.find; 
	      s.inv <- Term.Map.remove b' s.inv; (* don't forget to remove this. *)
	      s.inv <- Term.Map.add b x s.inv;
	      s.ext <- Ext.do_at_add (p, s.ext, s.find) (x, b, rho)
	  with 
	      Not_found ->     (* extend *)
		begin
		  s.dep <- Use.add x b s.dep;
		  s.find <- Term.Var.Map.add x (b, rho) s.find;
		  s.inv <- Term.Map.add b x s.inv;
		  s.ext <- Ext.do_at_add (p, s.ext, s.find) (x, b, rho)
		end));
       assert(synchronized s)
	    
	 
   let restrict (p, s) x =
     assert(synchronized s);
     try
       let (b, rho) = Term.Var.Map.find x s.find in
	 Trace.msg trace_level "Restrict" x Term.pp;
	 assert(Term.Var.Map.mem x s.find); 
	 assert(Term.Map.mem b s.inv);
	 s.find <- Term.Var.Map.remove x s.find; 
	 s.inv <- Term.Map.remove b s.inv;
	 assert(not(Term.Var.Map.mem x s.find));
	 assert(not(Term.Map.mem b s.inv));
	 s.dep <- Use.remove x b s.dep; 
	 s.ext <- Ext.do_at_restrict (p, s.ext, s.find) (x, b, rho);  
	 assert(synchronized s)
     with
	 Not_found -> ()
	   
   
   let rec update (p, s) e =
     let (x, b, rho1) = Fact.Equal.destruct e in  (* [rho1 |- x = b]. *)     
       if Term.eq x b then
	 restrict (p, s) x 
       else if Term.is_var b then
	 begin
	   publish Th.th p e;       (* propagate new variable equality. *)
	   merge (p, s) e           (* and merge in solution set *)
	 end 
       else
	 try
	   let (y, rho2) = inv s b in             (* [rho2 |- y = b]. *)
	     if not(Term.eq x y) then
	       let tau = Jst.dep2 rho1 rho2 in    (* [tau |- x = y]. *)          
		 publish Th.th p (Fact.Equal.make (x, y, tau));
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
	 let (a, rho2) = apply s y in             (* [rho2 |- y = a]. *)
	   if Term.(<<<) y x then 
	     restrict (p, s) x 
	   else 
	     let rho = Jst.dep2 rho1 rho2 in
	       begin                               (* [rho |- x = a]. *)
		 restrict (p, s) y;
		 add (p, s) (Fact.Equal.make (x, a, rho))
	       end 
       with
	   Not_found -> 
	     restrict (p, s) x
	     
   let v = Name.of_string "v"
  
   (** Return a canonical variable [x] equal to [b]. If [b] is not a rhs in
     the equality set for theory [i], then a variable [x] is newly created. *)
   let name (p, s) b =
     if Term.is_var b then Jst.Eqtrans.id b else
       try 
	 inv s b 
       with 
	   Not_found ->
	     let c = try Var.Cnstrnt.mk_real(Arith.dom_of b) with Not_found -> Var.Cnstrnt.Unconstrained in
	     let x = Term.Var.mk_rename v None c in 
	     let rho = Jst.dep0 in
	       update (p, s) (Fact.Equal.make (x, b, rho));
	       (x, rho)
	       

   (** Propagating a list of solved equalities on rhs. *)
   let rec fuse (p, s) el =
     fuse_star (p, s) el
       
   and fuse_star (p, s) el =  
     let norm = Fact.Equal.norm Th.map el in
     List.iter
       (fun e ->
	  (Dep.iter s
	     (fun e -> 
		let e' = Fact.Equal.map_rhs norm e in
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

module Idx2Ext(Idx: INDEX): (EXT with type t = Term.Var.Set.t) =
  struct
    type t = Term.Var.Set.t
    let pp fmt s =   
      Format.fprintf fmt "\n%s: " Idx.name;
      Pretty.set Term.pp fmt (Term.Var.Set.elements s)
    let empty = Term.Var.Set.empty 
    let do_at_add (p, s, _) (x, a, _) = 
      if Idx.holds a then Term.Var.Set.add x s else s
    let do_at_restrict (p, s, _) (x, a, _) =
      Term.Var.Set.remove x s
end

type index = Term.Var.Set.t
type cnstnt = (Term.t * Jst.t) Term.Var.Map.t

module MakeIndex(Th: TH)(Idx : INDEX): (SET with type ext = index) =
  Make(Th)(Idx2Ext(Idx))


(** {6 Constant extensions} *)

module type CNSTNT = sig
  val th : Th.t
  val is_const : Term.t -> bool
  val is_diseq : Term.t -> Term.t -> bool
end 

module Cnstnt2Ext(Cnstnt: CNSTNT): (EXT with type t = cnstnt) =
  struct
    type t = cnstnt
    let pp fmt s =
      if not(s == Term.Var.Map.empty) then
	begin
	  Format.fprintf fmt "\ncnstnt: ";
	  let l = Term.Var.Map.fold 
		    (fun x (a, _) acc -> (x, a) :: acc)
		    s []
	  in
	    Pretty.map Term.pp Term.pp fmt l   
	end 
    let empty = Term.Var.Map.empty
	       (** Generate disequalities [x <> y] for constant equalities 
		 [x = a] and [y = b] with [a <> b]. *)
    let th = Th.inj Cnstnt.th
    let do_at_add (p, s, _) (x, a, rho) = 
      Term.Var.Map.iter                     (* [rho |- x = a] *)
	(fun y (b, tau) ->                  (* [tau |- y = b] *)
	   if Cnstnt.is_diseq a b then      (* [sigma |- x <> y] *)
	     let sigma = Jst.dep2 rho tau in
	     let d = Fact.Diseq.make (x, y, sigma) in
	       Fact.Diseqs.push th d)
      s;
      if Cnstnt.is_const a then Term.Var.Map.add x (a, rho) s else s
    let do_at_restrict (_, s, _) (x, a, _) =
      if Cnstnt.is_const a then Term.Var.Map.remove x s else s
end

module MakeCnstnt(Th: TH)(Cnstnt: CNSTNT): (SET with type ext = cnstnt) =
  Make(Th)(Cnstnt2Ext(Cnstnt))



(** {6 Solution sets with constant index} *)

module MakeIndexCnstnt(Th: TH)(Idx: INDEX)(Cnstnt: CNSTNT)
  : (SET with type ext = index * cnstnt) =
  Make(Th)(CombineExt(Idx2Ext(Idx))(Cnstnt2Ext(Cnstnt)))


module MakeIndexExt(Th: TH)(Idx: INDEX)(Ext: EXT)
  : (SET with type ext = index * Ext.t) =
  Make(Th)(CombineExt(Idx2Ext(Idx))(Ext))


(** {6 Combining two equality sets} *)

module type SET2 = sig
  type t
  type lext
  type rext
  val eq : t -> t -> bool
  val pp : t Pretty.printer
  val empty : t
  val is_empty : t -> bool
  type tag = Left | Right
  val is_dependent : tag -> t -> Term.t -> bool
  val is_independent :  tag -> t -> Term.t -> bool 
  val iter :  tag -> (Term.t -> Term.t * Jst.t -> unit) -> t -> unit
  val fold :  tag -> (Term.t -> Term.t * Jst.t -> 'a -> 'a) -> t -> 'a -> 'a
  val to_list :  tag -> t -> (Term.t * Term.t) list
  val apply :  tag -> t -> Jst.Eqtrans.t
  val equality :  tag -> t -> Term.t -> Fact.Equal.t
  val find :  tag -> t -> Jst.Eqtrans.t
  val inv :  tag -> t -> Jst.Eqtrans.t 
  val dep :  tag -> t -> Term.t -> Term.Var.Set.t
  val ext : t -> lext * rext
  module Dep : sig
    val iter :  tag -> t -> (Fact.Equal.t -> unit) -> Term.t -> unit 
    val fold :  tag -> t -> (Fact.Equal.t -> 'a -> 'a) -> Term.t -> 'a  -> 'a
    val for_all :  tag -> t -> (Fact.Equal.t -> bool) -> Term.t -> bool
    val exists :  tag -> t -> (Fact.Equal.t -> bool) -> Term.t -> bool
    val choose :  tag -> t -> (Fact.Equal.t -> bool) -> Term.t -> Fact.Equal.t
  end
  val copy : t -> t
  type config = Partition.t * t
  val name:   tag -> config -> Jst.Eqtrans.t
  val update :  tag -> config -> Fact.Equal.t -> unit
  val restrict :  tag -> config -> Term.t -> unit
  val fuse:  tag -> config -> Fact.Equal.t list -> unit
  val compose :  tag -> config -> Fact.Equal.t list -> unit
end


module Union(Th: sig val th: Th.t end)(Left: SET)(Right: SET) = 
struct

  type t = {                    (* to do: establish confluence across theories. *)
    mutable left : Left.t; 
    mutable right : Right.t;
  }

  type lext = Left.ext
  type rext = Right.ext
      
  let eq s t = 
    Left.eq s.left t.left && 
    Right.eq s.right t.right
		 
  let pp fmt s = 
    Left.pp fmt s.left;
    Right.pp fmt s.right
      
  let empty = { 
    left = Left.empty; 
    right = Right.empty;
  }
		
  let copy s = {
    left = Left.copy s.left; 
    right = Right.copy s.right; 
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
  let iter tag f = case_tag (Left.iter f) (Right.iter f) tag
  let fold tag f = case_tag (Left.fold f) (Right.fold f) tag
  let to_list = case_tag Left.to_list Right.to_list
  let apply = case_tag Left.apply Right.apply
  let equality = case_tag Left.equality Right.equality
  let find = case_tag Left.find Right.find
  let inv = case_tag Left.inv Right.inv
  let dep = case_tag Left.dep Right.dep
  let ext s = (Left.ext s.left, Right.ext s.right)

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
	  
  let name tag (p, s) =
    match tag with
      | Left -> Left.name (p, s.left)
      | Right -> Right.name (p, s.right)
	     
  (** Update and establish confluence across solution sets; if [x = a] in [Left]
    and [y = a] in Right, then [x = y]. *)
  let update tag ((p, s) as cfg) e =
    match tag with
      | Left -> 
	  Left.update (p, s.left) e;  
	  let (x, a, rho) = Fact.Equal.destruct e in
	    (try
	       let (y, tau) = Right.inv s.right a in      (* [tau |- y = a] *)
	       let  sigma = Jst.dep2 tau rho in
		 publish Th.th p (Fact.Equal.make (x, y, sigma));
		 Left.restrict (p, s.left) x              (* restrict [x = a] in [Left] *)
	     with
		 Not_found -> ())
      | Right -> 
	  Right.update (p, s.right) e;
	  (try
	     let (x, a, rho) = Fact.Equal.destruct e in
	     let (y, tau) = Left.inv s.left a in        (* [tau |- y = a] *)
	     let  sigma = Jst.dep2 tau rho in
	       publish Th.th p (Fact.Equal.make (x, y, sigma));
	       Left.restrict (p, s.left) y              (* restrict [y = a] in [Left]. *)
	   with
	       Not_found -> ())

  let restrict tag (p, s) =
    match tag with
      | Left -> Left.restrict (p, s.left)
      | Right -> Right.restrict (p, s.right)
		   

  (** Propagating a list of solved equalities on rhs. *)
  let rec fuse tag cfg el =
    fuse_star tag cfg el
      
  and fuse_star tag (p, s) el =  
    let norm = Fact.Equal.norm Arith.map el in  (* hack *)
      List.iter
	(fun e ->
	   (Dep.iter tag s
	      (fun e -> 
		 let e' = Fact.Equal.map_rhs norm e in
		   update tag (p, s) e')
	      (Fact.Equal.lhs_of e)))
	el

  (** Fuse a list of solved equalities [el] on rhs 
    followed by updates of [el]. *)
  let compose tag cfg el =
    fuse_star tag cfg el;
    List.iter (update tag cfg) el

end
    



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

exception Found

(** Theory definition. *)
let theory =  Theory.create "u"

let is_theory = Theory.eq theory

let _ =
  Theory.Description.add theory
    "Theory of equality of uninterpreted function symbols."

(** Uninterpreted function symbols are names. *)
module Sig = struct
  let th = theory
  type t = Name.t
  let name n = n
end

module Op = Funsym.Make(Sig)

let mk_app n t = 
  Term.mk_app (Op.inj n) (Term.Args.make1 t)

let mk_const n = 
  Term.mk_const (Op.inj n)
      
let is_uninterp t =
  try 
    Op.is_interp (Term.sym_of t) &&
    Term.Args.length (Term.args_of t) <= 1
  with 
      Not_found -> false

let is_app0 t = 
  try
    Op.is_interp (Term.sym_of t) &&
    Term.is_const t
  with
      Not_found -> false

let is_app1 t = 
  try
    Op.is_interp (Term.sym_of t) &&
    Term.is_unary t
  with
      Not_found -> false
 
(** [op a] and [args a] do not throw 
  an exception if [is_uninterp a] holds. *)
let op a = Term.sym_of a

let args t = 
  assert(is_uninterp t);
  let a = Term.args_of t in
    match Term.Args.length a with
      | 0 -> None
      | 1 -> Some(Term.Args.get a 0)
      | _ -> invalid_arg "multiary uninterpreted function symbol"
	  
let arg1 t = 
  assert(is_app1 t);
  Term.Args.get (Term.args_of t) 0

(** Pure terms have only uninterpreted function symbols. *)
let rec is_pure a =
  Term.is_var a ||
  (is_uninterp a && 
   match args a with
     | None -> true
     | Some(b) -> is_pure b)

(** Flat terms are of the form [f(x1,...,xn)]. *)
let is_flat a = 
  is_uninterp a &&
  (match args a with
     | None -> true
     | Some(x) -> Term.is_var x)

(** Proof theory. *)
module J = struct

  (** If [e1 |- s1 = t] and [e2 |- s2 = t], then [join e1 e2 |- s1 = s2]. *)
  class join e1 e2 = (object
     inherit Judgement.Top.equal
     method lhs = e1#lhs
     method rhs = e2#lhs
     method name = Format.sprintf "join"
     method hyps = Judgement.mk_add_equal e1 
		     (Judgement.mk_add_equal e2 Judgement.mk_empty)
     method validate = Term.eq e1#rhs e2#rhs
   end : Judgement.equal)

  (** If [e |- s = t], then [cong f e |- f(s) = f(t)]. *)
  class cong f e = (object
     inherit Judgement.Top.equal
     method lhs = Term.mk_unary f e#lhs
     method rhs = Term.mk_unary f e#rhs
     method name = Format.sprintf "cong[%s]" (Funsym.to_string f)
     method hyps = Judgement.mk_singleton (e:>Judgement.atom)
   end : Judgement.equal)

  (** If [e1 |- u = f(x)], [e2 |- v = f(y)], [d |- u <> v],
    then [func e1 e2 d |- x <> y] *)
  class func (e1: Judgement.equal) (e2: Judgement.equal) (d: Judgement.diseq) = (object
    inherit Judgement.Top.diseq
    method lhs = arg1 d#lhs
    method rhs = arg1 d#rhs
    method hyps = 
      Judgement.mk_add_equal e1
	(Judgement.mk_add_equal e2
	   (Judgement.mk_add_diseq d Judgement.mk_empty))
    method name = Format.sprintf "functionality"
    method validate =
      is_app1 e1#rhs && is_app1 e2#rhs &&
      Funsym.eq (op e1#rhs) (op e2#rhs) &&
      Term.eq e1#lhs d#lhs && Term.eq e2#lhs d#rhs
  end : Judgement.diseq)

  class replace (e: Judgement.equal) (a: Judgement.atom) = (object
     inherit Judgement.Top.atom
     method concl = Atom.replace e#lhs e#rhs a#concl
     method name = "replace"
     method hyps = Judgement.mk_add_equal e (Judgement.mk_singleton a)
  end: Judgement.atom)

  let is_refl e = (e#lhs == e#rhs)

  let mk_refl = Judgement.mk_refl
  let mk_sym = Judgement.mk_sym
  let mk_trans = Judgement.mk_trans
  let mk_trans3 = Judgement.mk_trans3
  let mk_alias = Judgement.mk_alias

  let mk_join e1 e2 = new join e1 e2

  let mk_func e1 e2 d = new func e1 e2 d

  let mk_cong f e = new cong f e

  let mk_replace e = failwith "u: to do"
  let mk_replace_in_diseq = Judgement.mk_replace_in_diseq

  module Equals = Judgement.Equals
  module Diseqs = Judgement.Diseqs

end


(** Representation for renaming variables for flat terms. *)
module Config = struct

  module Map = Term.Map
  module Set = Term.Set
    
  (** A {i renaming context} consists of a finite set of bindings 
    - [u |-> (f, e)] with [e |- a = x] for some term [a] and variable [x]
    for representing renamings [u] for the {i unary} term [f(x)] and bindings
    - [u |-> f] for representing a renaming [u] for {i nullary} terms [f()].
    [dep] is an index such that [u in dep(x)] if there is a justification [e] 
    for [x] in [unary(u)]. *)
  type t = {
    mutable find1: (Funsym.t * Judgement.equal) Map.t;
    mutable find0 : Funsym.t Map.t;
    mutable dep : Dep.t;
  }

  let is_empty s =
    Map.is_empty s.find1 &&
    Map.is_empty s.find0

  let to_equalities s =
    let es = J.Equals.empty () in
      Map.iter
	(fun u (f, e0) ->
	   let e = J.mk_alias u (Term.mk_unary f e0#rhs) in
	     J.Equals.add e es)
	s.find1;
      Map.iter
	(fun u f ->
	   let e = J.mk_alias u (Term.mk_const f) in
	     J.Equals.add e es)
	s.find0;
      es
    
  let pp fmt s =
    J.Equals.pp fmt (to_equalities s);
    if (Version.debug() >= 1) then
      (Format.fprintf fmt "\ndep: "; Dep.pp fmt s.dep)
   
  let in_dom s u = 
    assert(Term.is_var u);
    Map.mem u s.find1 || Map.mem u s.find0
      
  let in_cod s x = 
    assert(Term.is_var x);
    Dep.mem x s.dep 

  let occ x s = in_dom s x || in_cod s x

  let dep s x =
    assert(Term.is_var x);
    Dep.find s.dep x

  let find0 s u = 
    assert(Term.is_var u);
    Map.find u s.find0

  let find1 s u =
    assert(Term.is_var u);
    let f, e = Map.find u s.find1 in
      assert(Term.is_var e#rhs);
      f, e#rhs, e
      
  (** The {i Lookup} [apply s x] of [a] in configuration [s]
    is [a] if [x = a] on [s] and the [x] is chosen in a fixed but
    arbitrary way. Otherwise, [apply s x] throws [Not_found]. *)
  let rec apply s x =
    if Term.is_var x then 
      try apply1 s x with Not_found -> apply0 s x
    else 
      raise Not_found

  and apply1 s u = 
    assert(Term.is_var u);
    let f, x, e = find1 s u in
    let t1 = Term.mk_unary f x in
      t1, J.mk_alias u t1

  and apply0 s x = 
    assert(Term.is_var x);
    let f = Map.find x s.find0 in
    let t0 = Term.mk_const f in
      t0, J.mk_alias x t0
   
  (** Return [(x, rho)] if [rho |- x = a] is in [s]. 
    If [a] is a constant, then traverse the data structure
    to find the inverse.  Otherwise, the dependency index
    on some variable [y] in [a] is used to find [x = a]. *)
  let rec inv s a =
    let f = op a in
      match args a with
	| None -> inv0 s f
	| Some(x) -> if Term.is_var x then inv1 s f x else raise Not_found

  and inv0 s f =
    let u0 = ref (Obj.magic 0) in
      try
	Map.iter 
	  (fun u g ->
	     if Funsym.eq f g then (u0 := u; raise Found))
	  s.find0;
	raise Not_found
      with
	  Found ->                    (* [e0 |- u0 = f()]. *)
	    let e0 = J.mk_alias !u0 (Term.mk_const f) in 
	      !u0, e0
    
  and inv1 s f x =
    let us = Dep.find s.dep x in
    let u0 = ref (Obj.magic 0) and e0 = ref (Obj.magic 0) in
      try
	Dep.Set.iter
	  (fun u ->
	     try
	       let g, y, e = find1 s u in
		 if Funsym.eq f g && Term.eq x y then
		   (u0 := u; e0 := e; raise Found)
	     with
		 Not_found -> invalid_arg "U.inv: imprecise dependency.")
	  us;
	raise Not_found
      with
	  Found -> !u0, !e0
	
  let empty () = {
    find0 = Map.empty();
    find1 = Map.empty();
    dep = Dep.empty();
  }
		   
  let copy s = { 
    find0 = Map.copy s.find0; 
    find1 = Map.copy s.find1;
    dep = Dep.copy s.dep;
  }
      
  let rec model s =
    let alpha0 = Term.Assign.empty in
      (interp s, alpha0)

  and interp s =
    let i0 = Term.Interp.empty in
      interp0 s (interp1 s i0)

  and interp0 s =
    Map.fold
      (fun u f -> 
	 Term.Interp.update f Term.Args.make0 u)
      s.find0

  and interp1 s =
    Map.fold
      (fun u (f, e1) -> 
	 Term.Interp.update f (Term.Args.make1 e1#rhs) u)
      s.find1

end



(** Set of flat equalities [y = f(x{1},...,x{n})]. *)
module Infsys = struct
  
  let s = ref (Config.copy (Config.empty()))
  let unchanged = ref true

  let current () = !s

  let is_unchanged () = !unchanged

  let initialize s0 =
    unchanged := true;
    s := s0 

  let reset () = 
    initialize (Config.empty())

  let finalize () = 
    if !unchanged then !s else Config.copy !s

  (** Flatten a pure term [a] and introduce variables as
    necessary for naming subterms. The result is a variable 
    [u] equal to [a] in the extended context. *)
  let rec can t =
    assert(is_pure t);
    if Term.is_var t then V.Infsys.can t else canapp t

  and canapp t =
    assert(is_uninterp t);
    match op t, args t with
      | f, None -> canapp0 f
      | f, Some(b) -> canapp1 f b

  and canapp0 f =
    try
      let u = inv0 f in
	u, J.mk_alias u (Term.mk_const f)
    with
	Not_found -> 
	  let u = Term.mk_fresh_var "u" in
	    unchanged := false;
	    Config.Map.set u f !s.Config.find0;
	    u, J.mk_refl u

  and canapp1 f a1 =
    let x1, e1 = can a1 in          (* [e1 |- a1 = x1]. *)
      assert(Term.is_var x1);
      try
	let u, e2 = inv1 f x1 in    (* [e2 |- b = x1]. *)
	let e = J.mk_join e1 e2 in  (* ==> [join e1 e2 |- a = b]. *)
	  u, failwith "u:to do"
      with
	  Not_found ->
	    let u = Term.mk_fresh_var "u" in
	      unchanged := false;
	      Config.Map.set u (f, e1) !s.Config.find1;
	      Dep.add u x1 !s.Config.dep;
	      u, J.mk_refl u
		
  and inv0 f =
    let u0 = ref (Obj.magic 0) in
      try
	Config.Map.iter 
	  (fun u g ->
	     if Funsym.eq f g then (u0 := u; raise Found))
	  !s.Config.find0;
	raise Not_found
      with
	  Found -> !u0

  and inv1 f x =
    let us = Dep.find !s.Config.dep x in 
    let u0 = Obj.magic 0 and e0 = Obj.magic 0 in
      try
	Dep.Set.iter
	  (fun u ->
	     try
	       let g, e = Config.Map.find u !s.Config.find1 in
		 if Funsym.eq f g && Term.eq x e#rhs then
		   (u0 := u; e0 := e; raise Found)
	     with
		 Not_found -> invalid_arg "U.inv: imprecise dependency.")
	  us;
	raise Not_found
      with
	  Found -> !u0, !e0
      
  let abstract a atm =
    assert(is_pure a && not(Term.is_var a));
    let x, e = can a in                 (* [e |- a = x]. *)
      assert(Term.is_var x);
      let concl' = Atom.replace a x atm#concl in
      let atm' = J.mk_replace e atm in
	G.Infsys.put atm'
      
  let process_equal e =                 (* [e |- a = b]. *)
    let a = e#lhs and b = e#rhs in
      assert(is_pure a && is_pure b);
      if Term.eq a b then () else  
      let x, e1 = can a in              (* [e1 |- a = x]. *)
      let y, e2 = can b in              (* [e2 |- b = y]. *) 
      let e3 = J.mk_trans3 (J.mk_sym e1) e e2 in
	assert(Term.eq e3#lhs x && Term.eq e3#rhs y);
	V.Infsys.process_equal e3       (* ==> [e3 |- x = y]. *)
	 
  let process_diseq d =
    let a = d#lhs and b = d#rhs in      (* [d |- a <> b]. *)
      assert(is_pure a && is_pure b);
      let x, e1 = can a in              (* [e1 |- a = x]. *)
      let y, e2 = can b in              (* [e2 |- b = y]. *)
	assert(Term.is_var x && Term.is_var y);
	let d1 = J.mk_replace_in_diseq e1 e2 d in
	  assert(Term.eq d1#lhs x && Term.eq d1#rhs y);
	  V.Infsys.process_diseq d1     (* ==> [d1 |- x <> y]. *)
	    
  let rec propagate_equal x =
    assert(Term.is_var x);
    try
      let us = Dep.find !s.Config.dep x in
      let y, e1 = V.Infsys.can x in          (* [e |- x = y]. *)
	assert(Term.eq x e1#lhs);
	Dep.Set.iter
	  (fun u -> 
	     try                             (* [e2 |- a = x]. *)
	       let f, e2 = Config.Map.find u !s.Config.find1 in
		 assert(Term.eq x e2#rhs);
		 let e3 = J.mk_trans e2 e1 in
		   (try                      (* ==> [e3 |- a = y]. *)
		     let v, e4 = inv1 f y in (* [e4 |- b = y]. *)
		     let e = J.mk_cong f (J.mk_join e3 e4) in
		       V.Infsys.process_equal e
		   with
		       Not_found -> 
			 unchanged := false;
			 Config.Map.set u (f, e3) !s.Config.find1;
			 Dep.replace x y u !s.Config.dep)
	     with 
		 Not_found -> invalid_arg "U.Infsys.imprecise dependency")
	  us
    with
	Not_found -> ()

  let rec propagate_diseq d =
    functionality d

  (** [u = f(x), v = f(y), u <> v ==> x <> y] *)
  and functionality d =
    let u = d#lhs and v = d#rhs in
    assert(Term.is_var u && Term.is_var v);
      () (* to do *)

  let rec normalize _ =
    ()

end 


module Component: E.COMPONENT = struct
  let th = theory
  module Eqs = Config
  module I = Infsys
  module Infsys = struct
    type eqs = Eqs.t
    let current = I.current
    let reset = I.reset
    let initialize = I.initialize
    let is_unchanged = I.is_unchanged
    let finalize = I.finalize
    let abstract = I.abstract
    let process_equal = Some(I.process_equal)
    let process_diseq = Some(I.process_diseq)
    let process_nonneg = None
    let process_pos = None
    let propagate_equal = Some(I.propagate_equal)
    let propagate_diseq = Some(I.propagate_diseq)
    let propagate_cnstrnt = None
    let propagate_nonneg = None
    let process_pos = None
    let branch _ = raise Not_found
    let normalize = I.normalize
  end
end
  
module Unit = 
  E.Register(Component)




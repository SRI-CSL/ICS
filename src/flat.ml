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
 
module Map = Term.Vmap
module Set = Term.Vset


let is_flat a =
  not (Term.is_var a) &&
  List.for_all Term.is_var (Term.args_of a)
  
type t = {
  mutable find: Judgement.equal * Funsym.t *  Map.t;
  mutable dep : Dep.t;
}
  
let is_empty s = Map.is_empty s.find
 
let to_list s =
  Map.fold
    (fun x ->
       let add (a, rho) acc = (x, a, rho) :: acc in
	 Cod.fold add)
    s.find []
    
let pp fmt s =
  Map.pp Cod.pp fmt s.find;
  if (Version.debug() >= 1) then
    (Format.fprintf fmt "\ndep: "; Dep.pp fmt s.dep)

let get s x =
  if Term.is_var x then
    Map.find x s.find 
  else
    raise Not_found

(** The {i Lookup} [apply s x] of [a] in configuration [s]
  is [a] if [x = a] on [s] and the [x] is chosen in a fixed but
  arbitrary way. Otherwise, [apply s x] throws [Not_found]. *)
let apply s x =
  Cod.choose (get s x)
      
let iter f s = 
  let f' x = Cod.iter (f x) in
    Map.iter f' s.find
      
let fold f s e =
  let acc = ref e in
  let f' x (a, rho) =
    acc := f x (a, rho) !acc
  in
    iter f' s;
    !acc
      
let mem x s =
  Map.mem x s.find
    
let dep s x =
  assert(Term.is_var x);
  try Dep.find s.dep x with Not_found -> Dep.Set.empty()
    
let occ x s =
  mem x s || Dep.mem x s.dep
    
let iter_on s x f =
  let f_cod y (b, rho) = 
    if Term.occurs x b then f y (b, rho) 
  in 
  let iter_on_cod g y = 
    try 
      let cod = Map.find y s.find in
	Cod.iter (g y) cod
    with 
	Not_found ->  ()
  in
    Dep.Set.iter 
      (fun y -> 
	 if not(x == y) then 
	   iter_on_cod f_cod y) 
      (dep s x);
    iter_on_cod f x
      
let in_dom s x = 
  assert(Term.is_var x);
  Map.mem x s.find
    
let in_cod s x = 
  assert(Term.is_var x);
  Dep.mem x s.dep 
    
(** The {i Lookup} [apply s x] of [a] in configuration [s]
  is [a] if [x = a] on [s] and the [x] is chosen in a fixed but
  arbitrary way. Otherwise, [apply s x] throws [Not_found]. *)
let apply s x =
  if Term.is_var x then
    Cod.choose (Map.find x s.find)
  else 
    raise Not_found
      
(** Return [(x, rho)] if [rho |- x = a] is in [s]. 
  If [a] is a constant, then traverse the data structure
  to find the inverse.  Otherwise, the dependency index
  on some variable [y] in [a] is used to find [x = a]. *)
let inv s a =
  if is_flat a then  
    let found = ref (Obj.magic 1) in
    let x = Term.choose Term.is_var a in
      try
	Dep.Set.iter
	  (fun y ->
	     try
	       let es = Map.find y s.find in
		 (try
		    let rho = Cod.choose_eq a es in  (* [rho |- y = a]. *)
		      found := (y, rho); raise Found
		  with
		      Not_found -> ())
	     with
		 Not_found -> invalid_arg "Can.inv: imprecise dependency.")
	  (dep s x);
	raise Not_found
      with
	  Found -> !found
  else 
    raise Not_found
      
let empty () = {
  find = Map.empty();
  dep = Dep.empty();
}
		 
let copy s = { 
  find = s.find;
  dep = s.dep
}

       
let is_canonical v s =
  let is_canonical_var x = 
    Term.is_var x && V.is_canonical v x 
  in
  let is_canonical_cod (a, _) = 
    Term.for_all is_canonical_var a 
  in
    Map.for_all
      (fun x es ->
	 is_canonical_var x && 
	 Cod.for_all is_canonical_cod es)
      s.find
      
let interp s =
  fold 
    (fun x (a, _) -> 
       let f, al = Term.destruct a in
       Term.Interp.update f al x)
    s Term.Interp.empty
       

let model s =
  let id = Term.Assign.empty in
    (interp s, id)


let replace map s rhos =
  let lookup y = 
    try
      let b, tau = apply s y in
	rhos := Jst.dep2 tau !rhos; b
    with
	Not_found -> y
  in
    map lookup



(** {i Reduce} of [a] in configuration [S]
  - [S^-1(x) = x].   
  - [S^-1(a) = x] if [x = a] in [s].
  - [S^-1(f(a1, ...,an)) = S^-1(f(S^-1(a1), ..., S^-1(an)))] 
  if there is [ai =/= S^-1(ai)].
  - [S^-1(a)] = a otherwise. *)
let reduce (map, sigma) s rhos =
  let rec red a =
    if Term.is_var a then a else
      try
	let (x, rho) = inv s a in
	  rhos := Jst.dep2 rho !rhos; x
      with
	  Not_found -> 
	    let al = Term.args_of a in
	    let al' = Term.mapl red al in
	      if al == al' then a else 
		red (sigma (Term.sym_of a) al')
  in
    red


module type ARGS = sig
  val can : Term.interp
  val map : Term.map
  val effect : Jst.Equal.t -> unit
end


module type UPDATE = sig

  val extend : Jst.Equal.t -> t -> unit

  val add : Jst.Equal.t -> t -> unit

  val remove : Jst.Equal.t -> t -> unit

  val instantiate : Jst.Equal.t -> t -> unit

  val varify : t -> Jst.t ref -> Term.t -> Term.t

end
 

module Update(Args: ARGS) = struct

  let extend (x, b, rho) s = 
    assert(not(in_dom s x));
    Map.set x (Cod.singleton (b, rho)) s.find;
    Term.iter (fun y -> Dep.add x y s.dep) b
      
  let add (x, a, rho) s =
    assert(Term.is_var x);
    assert(is_flat a);  
    try
      let z, tau = inv s a in          (* [tau |- z = a]. *)
	assert(Term.is_var z);
	V.Infsys.process_equal (x, z, tau)
    with
	Not_found -> 
	  let cod' =
	    try
	      let cod = Map.find x s.find in
		Cod.add (a, rho) cod
	    with
		Not_found -> Cod.singleton (a, rho)
	  in
	    Map.set x cod' s.find;
	    Term.iter (fun y -> Dep.add x y s.dep) a
	      
  let remove (x, a, rho) s =
    try
      let cod = Map.find x s.find in
	if Cod.mem (a, rho) cod then
	  let cod' = Cod.remove (a, rho) cod in
	    if Cod.is_empty cod' then
	      begin
		Map.remove x s.find;
		Term.iter (fun y -> Dep.remove x y s.dep) a
	      end 
	    else
	      begin
		Map.set x cod' s.find;
		failwith "to do"
	      end 
	else 
	  ()
    with
	Not_found -> ()


  let rec instantiate ((x, y, _) as e) s = 
    collapse e s;
    fuse e s

  and fuse ((x, y, rho) as e) s =                  (* [rho |- x = y]. *)
  assert(Term.is_var x);
    assert(Term.is_var y);
    let inst_right z = 
      assert(in_dom s z);
      assert(Term.is_var x && Term.is_var y); 
      let lookup z = if x == z then y else z in
	try
	  let es = Map.find z s.find in
	  let es' = ref es in
	  Cod.iter
	    (fun (e, tau) ->                       (* [tau |- z = e]. *)
	       assert(is_flat e);
	       if Term.subterm x e then
		 let e' = Args.map lookup e in     (* ==> [rho |- e = e']. *)
		   assert(is_flat e');
		   let sigma = Jst.dep2 rho tau in
		     es' := Cod.add (e', sigma) (Cod.remove (e, tau) !es');
		     try
		       let (z', ups) = inv s e' in  (* [ups |- z' = e']. *)
			 if not(Term.eq z z') then
			   deduce (z, z', Jst.dep2 sigma ups)
		     with
			 Not_found -> ())
	    es;
	    Map.set z !es' s.find;
	    Dep.remove z x s.dep;
	    if Cod.exists (fun (e', _) -> (Term.subterm y e')) !es' then  
	      Dep.add z y s.dep         
	with
	    Not_found -> invalid_arg "U.fuse: not a domain variable"
    in
      if not(Term.eq x y) then
	Dep.Set.iter inst_right (dep s x)
	  
  and deduce e =
    V.Infsys.process_equal e
      
  (** For [x = a], [y = b] replace [x = b] with [y = b]. *)
  and collapse ((x, y, rho) as e) s =  (* [rho |- x = y]. *) 
    try
      let as' = Map.find x s.find in
	(try
	  let bs' = Map.find y s.find in
	  let abs' = ref bs' in
	    Cod.iter
	      (fun ((a, tau) as e) ->    (* [tau |- x = a] *)
		 let e' = (a, Jst.dep2 rho tau) in
		   abs' := Cod.add e' !abs';
		   Term.iter (fun z -> Dep.replace x y z s.dep) a)
	      as';
	    Map.remove x s.find;
	    Map.set y !abs' s.find
	with
	    Not_found -> 
	      Map.set y as' s.find;
	      Cod.iter
		(fun ((a, _) as e) ->
		   Term.iter (fun z -> Dep.replace x y z s.dep) a)
		(failwith "to do"))
    with
	Not_found -> ()
	      
  (** Flatten a pure term [a] and introduce variables as
    necessary for naming subterms. The result is a variable [z]
    equal to [a] in the extended context. *)
  let varify s rhos =
    let rec var_of_term a =
      if Term.is_var a then a else
	try
	  let x, rho = inv s a in
	    assert(Term.is_var x);
	    rhos := Jst.dep2 rho !rhos;
	    x
	with
	    Not_found -> 
	      let al = Term.args_of a in
	      let bl = Term.mapl var_of_term al in
	      assert(List.for_all Term.is_var bl);
		let v = Term.mk_fresh_var "v" in
		let b = if al == bl then a else Args.can (Term.sym_of a) bl in
		  extend (v, b, Jst.dep0) s;
		  v
    in
      var_of_term
	
end

module type TRACE = sig
  val level : int
  val theory : Theory.t
end

module Trace(T: TRACE)(U: UPDATE): UPDATE = struct

  let msg = 
    let th = Theory.to_string T.theory in
      Format.sprintf "%s.%s" th

  let extend = 
    Trace.func2 T.level (msg "extend") Jst.Equal.pp pp Pretty.unit U.extend

  let add = 
    Trace.func2 T.level (msg "add") Jst.Equal.pp pp Pretty.unit U.add

  let remove = 
    Trace.func2 T.level (msg "remove") Jst.Equal.pp pp Pretty.unit U.remove

  let instantiate = 
    Trace.func2 T.level (msg "inst") Jst.Equal.pp pp Pretty.unit U.instantiate

  let varify = U.varify

end

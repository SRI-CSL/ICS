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


module Map = Term.Map
module Set = Term.Set
    
type t = {
  mutable find: Judgement.equal Map.t;
  mutable dep : Dep.t;        (* dependency index for variables *)
}
    
let empty () = {
  find = Map.empty ();
  dep = Dep.empty ();
}
		 
let equalities s = 
  let es = Judgement.Equals.empty () in
    Map.iter (fun _ e -> Judgement.Equals.add e es) s.find;
    es
      
let copy s = {
  find = Map.copy s.find;
  dep = Dep.copy s.dep;
}
	       
let is_empty s = 
  Map.is_empty s.find
      
let pp fmt s =
  Format.fprintf fmt "@[";
  Judgement.Equals.pp fmt (equalities s);
  if Version.debug() > 0 then
    (Format.fprintf fmt "\ndep:"; Dep.pp fmt s.dep);
  Format.fprintf fmt "@]@;"
      
let apply s x = 	
  if Term.is_var x then
    let e = Map.find x s.find in
      assert(Term.eq x e#lhs);
      e
    else
      raise Not_found
	
let dep s x = Dep.find s.dep x
    
exception Found of Term.t
  
(** Return [(x, rho)] if [rho |- x = a] is in [s]. 
  If [a] is a constant, then traverse the data structure
  to find the inverse.  Otherwise, the dependency index
  on some variable [y] in [a] is used to find [x = a]. *)
let rec inv s a =
  let rho = ref (Obj.magic 1) in
  let inv_cnstnt a =
    try
      Map.iter
	(fun x e ->
	   let b = e#rhs in
	     if Term.eq a b then (rho := e; raise (Found(x))))
	s.find;
      raise Not_found
    with
	Found(x) -> x
  and inv_noncnstnt a =
    let x = Term.choose Term.is_var a in
      assert(Term.is_var x);
      try
	Dep.Set.iter
	  (fun y ->
	     let e = apply s y in
	       if Term.eq a e#rhs then (rho := e; raise(Found y)))
	  (dep s x);
	raise Not_found
      with
	  Found(y) -> y
  in
    if Term.is_var a then raise Not_found else
      let x = if Term.is_const a then inv_cnstnt a else inv_noncnstnt a in
	(x, !rho)

let inv s a = snd(inv s a)
	
let in_dom x s = 
  assert(Term.is_var x);
  Map.mem x s.find
    
let in_cod x s =
  not (Dep.Set.is_empty (dep s x))
      
let occ x s =
  in_dom x s || in_cod x s
      
let fold f s =
  Map.fold f s.find
      
let iter f s =
  let f' _ e = f e in
    Map.iter f' s.find
      
let to_assign s =
  Map.fold
    (fun x e ->
       let a = e#rhs in
	 Term.Assign.add x a)
    s.find
    Term.Assign.empty
    
let model s =
  (Term.Interp.empty, to_assign s)
 
let is_canonical v s =
  let is_canonical_var x = 
    Term.is_var x && V.Config.is_canonical v x
  in
    Map.for_all
      (fun x e ->
	 let a = e#rhs in
	   is_canonical_var x &&
	   Term.for_all is_canonical_var a)
      s.find
      
let is_solved s =
  Map.for_all (fun x _ -> not(in_cod x s)) s.find
    
let extend s e =                  
  let x = e#lhs and a = e#rhs in    (* [e |- x = a]. *)
    assert(Term.is_var x);
    assert(not(occ x s));
    assert(not(Term.occurs x a));
    Map.set x e s.find;
    Term.iter (fun y -> Dep.add x y s.dep) a
	
let restrict s x =
  assert(Term.is_var x);
  try
    let e = apply s x in
      assert(Term.eq x e#lhs);
      Map.remove x s.find;
      Term.iter (fun y -> Dep.remove x y s.dep) e#rhs
  with
      Not_found -> ()
	
let update s e  =                 (* [e |- x = b]. *)
  let x = e#lhs and b = e#rhs in
    assert(not(Term.subterm x b));
    try                          (* [e' |- x = b']. *)
      let e' = apply s x in   
      let b' = e'#rhs in   
	Term.iter                (* remove all [y] in [b'] but not in [b]. *)
	  (fun y -> 
	     if not(Term.is_var_of y b) then Dep.remove x y s.dep) 
	  b';
	Term.iter (fun y -> Dep.add x y s.dep) b;
	Map.set x e s.find;
    with 
	Not_found -> extend s e
	


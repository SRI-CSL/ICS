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


module type SIG = sig
  val th : Th.t
  val f : Sym.t
end 


module type TERM = sig
  val d_interp : Term.t -> Term.t * Term.t
  val is_interp : Term.t -> bool 
  val make : Term.t -> Term.t -> Term.t
  val iterate : Term.t -> int -> Term.t
  val multiplicity: Term.t -> Term.t -> int
  val decompose : Term.t -> (Term.t * int) * Term.t option
  val fold : (Term.t -> int -> 'a -> 'a) -> Term.t -> 'a -> 'a
  val iter : (Term.t -> int -> unit) -> Term.t -> unit
  val sigma : Sym.t -> Term.t list -> Term.t
  val map : (Term.t -> Term.t) -> Term.t -> Term.t
end

(** Term manipulations for pure AC terms. *)
module Make(Sig: SIG): TERM = struct

  let d_interp a =
    match Term.App.destruct a with
      | f, [a1; a2] when Sym.eq f Sig.f ->
	  (a1, a2)
      | _ ->
	  raise Not_found

  let is_interp a = 
    try
      (match Term.App.destruct a with
	| f, [a1; a2] when Sym.eq f Sig.f -> true
	| _ -> false)
    with
	Not_found -> false

  (** Ordered right-associative applications of AC symbol [f] *)
  let rec make a b =
    try
      let (a1, a2) = d_interp a in
	make a1 (make a2 b)
    with
	Not_found -> 
	  assert(not(is_interp a));
	  try
	    let (b1, b2) = d_interp b in
	    let cmp = Term.cmp a b1 in
	      if Term.cmp a b1 <= 0 then   (* case [a <= b1] *)
		mk_app a b
	      else                         (* case [a > b1] *)
		make b1 (make a b2)
	  with
	      Not_found -> 
		assert(not(is_interp b));
		if Term.cmp a b <= 0 then
		  mk_app a b
		else
		  mk_app b a

  and mk_app a b = 
    Term.App.mk_app Sig.f [a; b]
      

  (** Number of occurrences of [x] in [a]. *)  
  let multiplicity x =
    let rec scan acc a =
      try
	let (y, b) = d_interp a in
	let cmp = Term.cmp x y in
	  if cmp < 0 then        (* [x << y] *)
	    scan acc b
	  else if cmp = 0 then   (* [x = y] *)
	    scan (1 + acc) b
	  else                   (* [x >> y] *)
	    acc
      with
	  Not_found -> 
	    if Term.eq x a then (1 + acc) else acc
    in
      scan 0
	
  (** Decompose [x*x*...*x*y*....] into [(x, n)] with [n] the
    multiplicity of [x] in [a] and [y*...], or raise [Not_found]
    if input term is not a multiplication. *)
  let decompose a =
    try
      let (x, _) = d_interp a in  
      let rec scan acc post =
	try
	  let (y, b) = d_interp post in
	    if Term.eq x y then
	      scan (acc + 1) b
	    else 
	      (acc, Some(post))
	with
	    Not_found -> 
	      if Term.eq x post then (acc + 1, None) else  (acc, None)  
      in
      let (n, b) = scan 0 a in
	((x, n), b)
    with
	Not_found -> ((a, 1), None)


  let rec fold f a acc =
    let ((x, n), b) = decompose a in
    let acc' = f x n acc in
      match b with
	| None -> acc'
	| Some(b') -> fold f b' acc'


  let rec iter f a =
    let ((x, n), b) = decompose a in
      f x n;
      match b with
	| None -> ()
	| Some(b') -> iter f b'
	    

  let rec iterate a n =
    assert(n >= 1);
    if n = 1 then a else make a (iterate a (n - 1))

  let rec of_list = function
    | [(a, n)] -> iterate a n
    | (a, n) :: al -> make (iterate a n) (of_list al)
    | [] -> invalid_arg "Sig.of_list: empty list"
	

  (** Sigma normal forms. *)
  let sigma g = function
    | [a; b] when Sym.eq Sig.f g -> make a b
    | al -> Term.App.mk_app g al

 
  (** Apply [f] to uninterpreted positions. *)
  let rec map f a = 
    try
      let (a1, a2) = d_interp a in
      let b1 = map f a1 and b2 = map f a2 in
	if a1 == b1 && a2 == b2 then a else make b1 b2
    with
	Not_found -> f a
	  
	  
  (** Replacing a variable with a term. *)
  let apply (x, b) = 
    map (fun y -> if Term.eq x y then b else y)
  
end 


module S = Solution.Set


(** Specification of AC theory as a canonizable and ground
  confluent theory. In particular, forward chaining on 
  - [x' = z*u], [y = x*v], [x =v x'] ==> [y = z*u*v]
  is used. *)
module T(Sig: SIG): Can.T = struct

  (**  AC canonizer for AC symbol in [Sig]. *)
  module Ac = Make(Sig)
      
  let th = Sig.th
	     
  let map = Ac.map
	      
  let sigma = Ac.sigma


  let d_flat e =
    let (x, a, rho) = e in
      (x, Ac.d_interp a, rho)

  let fold s f =
    let f' e acc =
      try f (d_flat e) acc with Not_found -> acc
    in
      S.fold f' s []
	

  (** Forward chaining on an equality [x' = z*u]. *)
  let rec of_equal e cfg =
    let el1 = of_equal1 e cfg
    and el2 = of_equal2 e cfg in
      el1 @ el2
	
  and of_equal1 e (p, s) =
    try
      let (x', (z, u), rho) = d_flat e in      (* [rho |- x' = z*u]. *)
	fold s
	  (fun (y, (x, v), tau) acc ->         (* [tau |- y = x*v]. *)
	     match Partition.is_equal p x x' with
	       | Some(sigma) -> 
		   let theta = Jst.dep3 rho tau sigma  in
		   let e' = Fact.Equal.make u (Ac.make z (Ac.make u v)) theta in
		     e' :: acc
	       | None -> 
		   acc)
    with
	Not_found -> []
	  
  (** Forward chaining on an equality [y = x*v]. *)
  and of_equal2 e (p, s) =
    try
      let (y, (x, v) , rho) = d_flat e in       (* [rho |- y = x*v] *)
	fold s
	  (fun (x', (z, u), tau) acc ->         (* [tau |- x' = z*u] *)
	     match Partition.is_equal p x x' with
	       | None -> acc
	       | Some(sigma) ->
		   let theta = Jst.dep3 rho tau sigma in
		   let e' = Fact.Equal.make u (Ac.make z (Ac.make u v)) theta in
		     e' :: acc)
    with
	Not_found -> []
	  
	  
  (** Forward chaining on variable equality [x =v x'] in
    - [x' = z*u], [y' = x*v], [x =v x'] ==> [y = z*u*v] *)
  let of_var_equal e (p, s) = 
    assert(Fact.Equal.is_var e);
    let (x0, rho) = Partition.find p (Fact.Equal.lhs_of e) in
      S.Dep.fold s
	(fun e1 acc ->
	   try
	     let (y', (x, v), tau) = d_flat e1 in
	       if not(Term.eq x x0) then acc else
		 Partition.fold p
		   (fun e2 acc ->
		      try
			let (x', (z, u), sigma) = d_flat e2 in
			  (match Partition.is_equal p x' x0 with
			     | None -> acc
			     | Some(theta) -> 
				 let (y, ups) = Partition.find p y' in
				 let eps = Jst.dep5 rho tau sigma theta ups in
				 let e = Fact.Equal.make y' (Ac.make z (Ac.make u v)) eps in
				   e :: acc)
		      with
			  Not_found -> acc)
		   x0 acc
	   with
	       Not_found -> acc)
	x0 []
	
      

  (** No forward chaining on disequalities. *)
  let of_var_diseq d (p, s) = 
    assert(Fact.Diseq.is_var d);
    []

  (** No branching. *)
  let disjunction (p, s) = 
    raise Not_found

end


(** Inference system for AC theories. *)
module Infsys(Sig: SIG): (Infsys.EQ with type e = Solution.Set.t) =
  Can.Make(T(Sig))



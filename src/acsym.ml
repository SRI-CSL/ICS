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

(** Operations on an AC symbol. *)

module type SIG = sig
  val th : string
  val f : string
end 

module type TERM = sig
  val arg1: Term.t -> Term.t
  val arg2: Term.t -> Term.t
  val is_interp : Term.t -> bool 
  val make : Term.t -> Term.t -> Term.t
  val iterate : Term.t -> int -> Term.t
  val multiplicity: Term.t -> Term.t -> int
  val decompose : Term.t -> (Term.t * int) * Term.t option
  val fold : (Term.t -> int -> 'a -> 'a) -> Term.t -> 'a -> 'a
  val iter : (Term.t -> int -> unit) -> Term.t -> unit
  val sigma : Funsym.t -> Term.t list -> Term.t
  val map : (Term.t -> Term.t) -> Term.t -> Term.t
end

module Make(Sig: SIG): TERM = struct

  (** Theory definition. *)
  let theory = Theory.create Sig.th

  let is_theory = Theory.eq theory


  (** Interpreted operations. *)
  module Op = struct

    let f: Funsym.t = Funsym.create theory Sig.f
			
    let pp fmt _ =
      Format.fprintf fmt "%s" Sig.f
	
    let eq _ _ = true
		   
    let _ = Funsym.register theory (pp, eq)
	      
    let get f =
      let i = Funsym.theory_of f in
	if is_theory i then Funsym.get theory f else raise Not_found
	  
    let is_interp f = 
      is_theory (Funsym.theory_of f)
	
    let unsafe_get f = 
      assert(is_interp f);
      Funsym.get theory f
  end 
    
  let op a = Op.get (Term.sym_of a)

  let is_interp a =
    Op.is_interp (Term.sym_of a)
      
  let arg1 a =
    if is_interp a then 
      match Term.args_of a with
	| [a1; _] -> a
	| _ -> raise Not_found
    else 
      raise Not_found
	
  let arg2 a =
    if is_interp a then 
      match Term.args_of a with
	| [_; a2] -> a2
	| _ -> raise Not_found
    else 
      raise Not_found
	
  let rec is_pure a =
    try
      is_interp a && is_pure (arg1 a) && is_pure (arg2 a)
    with
	Not_found ->  Term.is_var a

  (** Ordered right-associative applications of AC symbol [f] *)
  let rec make a b =
    try
      let a1 = arg1 a and a2 = arg2 a in
	make a1 (make a2 b)
    with
	Not_found -> 
	  assert(not(is_interp a));
	  try
	    let b1 = arg1 b and b2 = arg2 b in
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
    Term.mk_app Op.f [a; b]
      

  (** Number of occurrences of [x] in [a]. *)  
  let multiplicity x =
    let rec scan acc a =
      try
	let y = arg1 a and b = arg2 a in
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
      let x = arg1 a in
      let rec scan acc post =
	try
	  let y = arg1 post and b = arg2 post in
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
  let sigma g = 
    assert(Op.is_interp g);
    function
      | [a; b] -> make a b
      | al -> Term.mk_app g al

 
  (** Apply [f] to uninterpreted positions. *)
  let map f = 
    let rec mapf a =
      try
	if is_interp a then
	  let a1 = arg1 a and a2 = arg2 a in
	  let b1 = mapf a1 and b2 = mapf a2 in
	    if a1 == b1 && a2 == b2 then a else make b1 b2
	else 
	  f a
      with
	Not_found -> f a
    in
      mapf
	  
	  
  (** Replacing a variable with a term. *)
  let apply (x, b) = 
    map (fun y -> if Term.eq x y then b else y)
  
end 

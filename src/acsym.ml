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

(** Operations on an AC symbol. *)

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

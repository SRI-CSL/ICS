
(*i
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
 * 
 * Author: Harald Ruess, N. Shankar
 i*)

(*i*)
open Term
(*i*)

module type INTERP = sig
  val name : string  
  val is_dom : Term.t -> bool
  val norm : (Term.t -> Term.t) -> Term.t -> Term.t
  val solve : Term.t * Term.t -> (Term.t * Term.t) list
  val fold : (Term.t -> 'a -> 'a) -> Term.t -> 'a -> 'a 
end


module Make(Th: INTERP) = struct

  type t = {
    find: Term.t Map.t;
    inv : Term.t Map.t;
    use : Set.t Map.t
  }

  let empty = {
    find = Map.empty;
    inv = Map.empty;
    use = Map.empty
  }

  let apply s a = Map.find a s.find

  let find s a = try Map.find a s.find with Not_found -> a

  let inv s a = Map.find a s.inv

  let mem s a =  Map.mem a s.find

  let use s a = try Map.find a s.use with Not_found -> Set.empty


(*s Rewiring of [use] indices when changing the [find] structure. *)

  let add_use a =
    Th.fold
      (fun x acc -> 
	 try
	   let ux = Map.find x acc in
	   let ux' = Set.add a ux in
	   if ux == ux' then acc else Map.add x ux' acc
	 with
	     Not_found -> 
	       Map.add x (Set.singleton a) acc)
      a
     
  let del_use a =
    Th.fold
      (fun x acc ->
	 try
	   let ux = Map.find x acc in
	   let ux' = Set.remove a ux in
	   if Set.is_empty ux' then
	     Map.remove x acc
	   else if ux == ux' then 
	     acc 
	   else 
	     Map.add x ux' acc
	 with
	     Not_found -> acc)
      a


(*s [update a b s] sets the find of [a] to [b]. *)

  let union a b s = 
    Trace.msg 6 ("Union("^Th.name^")") (a,b) Pretty.eqn;
    let use' = 
      try 
	del_use (Map.find a s.find) s.use 
      with 
	  Not_found -> s.use 
    in
    {find =  Map.add a b s.find;
     inv = Map.add b a s.inv;
     use = add_use b use'}


(*s Restrict. *)

 let restrict a s =  
   Trace.msg 6 ("Restrict("^Th.name^")") a Pretty.term;
   try
     let b = Map.find a s.find in
     {find = Map.remove a s.find;
      inv = Map.remove b s.inv;
      use = del_use b s.use}
   with
       Not_found -> s
  

(*s Normalization function applied to logical context. *)

  let norm s = 
    Th.norm 
      (fun x -> 
	 try 
	   Map.find x s.find
	 with 
	     Not_found -> x)


(*s Normalization w.r.t. an association list. *)

  let norml el =
    Th.norm 
      (fun x -> 
	 try 
	   Term.assq x el 
	 with 
	     Not_found -> x)


(* Tests whether a rhs is external. *)

  let is_external b =
    Th.is_dom b 

(*s Adding a new equality and infer new facts. *)

  let add infer (a,b) (s, al) = 
    let s' = union a b s in
    let al' = match infer (a,b) with
      | Some(x) -> x :: al
      | None -> al
    in
    (s', al')


(*s Merging in a list of solved forms [el]. *)

  let compose infer vareq el = 
    let rec compose1 (a, b) ((s, xl, al) as acc) =
      if eq a b then
	acc
      else 
	let ((s', xl', al') as acc') = 
	  if is_external b then
	    (restrict a s, vareq (a, b) xl, al)
	  else 
	    try 
	      let a' = inv s b in     
	      if eq a a' then
		acc
	      else
                 let xl' = vareq (a,a') xl in
                 if V.(<<<) a' a then
		   (s, xl', al)
		 else 
		   let (s',al') = add infer (a,b) (s,al) in
		   (s', xl', al')
	    with
		Not_found ->
		  let (s',al') = add infer (a,b) (s,al) in
		  (s', xl, al')
	in
	Set.fold                        
	  (fun x ((s,_,_) as acc) -> 
	     let a' = inv s x in
	     let b' = norml el x in
	     compose1 (a',b') acc)
	  (use s a)
	  acc'
    in
    List.fold_right compose1 el

  let merge infer (x,y) s =
    let vareq (x',y') xl =
      let (x',y') = V.orient (x',y') in
      if eq x x' && eq y y' then xl else V.add x' y' xl 
    in
    let x' = norm s x in
    let y' = norm s y in
    compose infer vareq (Th.solve (x',y')) (s, V.empty, [])


  let extend b s = 
    let c = mk_const(Sym.mk_fresh_label()) in
    let s' = union c (norm s b) s in
    (c, s')

  let inst f s =
    Map.fold
      (fun x y acc ->
	 let x' = f x and y' = Th.norm f y in
	 union x' y' acc) 
      s.find
      empty

  let solution s =
    Map.fold
      (fun x b acc ->
	 (x, b) :: acc)
      s.find
      []

end


(*s Arithmetic context. *)

module A = Make(
  struct
    let name = "a"
    let is_dom a = not(Arith.is_interp a)
    let norm = Arith.norm
    let solve = 
      let not_is_slack x = not (is_slack x) in
      Arith.solve_for not_is_slack
    let fold = Arith.fold
  end)

(*s Tuple context. *)

module T = Make(
  struct
    let name = "t"
    let is_dom a = not(Tuple.is_tuple a || Tuple.is_fresh a)
    let norm = Tuple.norm   
    let solve = Tuple.solve
    let fold = Tuple.fold
  end)

(*s Bitvector context. *)

module BV = Make(
  struct
    let name = "bv"
    let is_dom a = not(Bv.is_interp a || Bv.is_fresh a)
    let norm = Bv.norm  
    let solve = Bv.solve
    let fold = Bv.fold
  end)

type t = {
  la : A.t;
  t : T.t;
  bv : BV.t
}


(*s Accessors. *)

let la_of s = s.la.A.find
let t_of s = s.t.T.find
let bv_of s = s.bv.BV.find

let pp fmt s =
  Pretty.solution fmt (A.solution s.la);
  Pretty.solution fmt (T.solution s.t);
  Pretty.solution fmt (BV.solution s.bv)


let empty = {
  la = A.empty;
  t = T.empty;
  bv = BV.empty
}

type i = A | T | BV

let name_of = function
  | A -> "a"
  | T -> "t"
  | BV -> "bv"

let index op =
  match op with
    | Sym.Arith _ -> A
    | Sym.Tuple _ -> T
    | Sym.Bv _ -> BV

let sigma i f l = 
  match Sym.destruct f with
    | Sym.Interp(Sym.Arith(op)) when i = A -> Arith.sigma op l
    | Sym.Interp(Sym.Tuple(op)) when i = T -> Tuple.sigma op l
    | Sym.Interp(Sym.Bv(op)) when i = BV -> Bv.sigma op l
    | _ -> App.sigma f l

let solve i =
  match i with
    | A -> Arith.solve 
    | T -> Tuple.solve
    | BV -> Bv.solve

let find i s a =
  match i with
    | A -> Map.find a s.la.A.find 
    | T -> Map.find a s.t.T.find 
    | BV -> Map.find a s.bv.BV.find 

let inv i s =
  match i with
    | A -> A.inv s.la
    | T -> T.inv s.t
    | BV -> BV.inv s.bv

let use i s =
  match i with
    | A -> A.use s.la
    | T -> T.use s.t
    | BV -> BV.use s.bv

let solution i s =
  match i with
    | A -> A.solution s.la
    | T -> T.solution s.t
    | BV -> BV.solution s.bv

(*s Tracing. *)

let call funname i = 
  Trace.call 5 (funname ^ "(" ^ (name_of i) ^ ")")

let exit funname i = 
  Trace.exit 5 (funname ^ "(" ^ (name_of i) ^ ")")


(*s Extending the state. *)

let extend i b s =
  call "Extend" i b Pretty.term;
  let (x,s') = 
    match i with
      | A -> let (x,la') = A.extend b s.la in (x, {s with la = la'})
      | T -> let (x,t') = T.extend b s.t in (x, {s with t = t'})
      | BV -> let (x,bv') = BV.extend b s.bv in (x, {s with bv = bv'})
  in
  exit "Extend" i x Pretty.term;
  (x, s')
  


(*s Infer constaint from equality. *)

type cnstrnt = Term.t -> Number.t option

let infera f (x,y) =
  match f x with
    | Some(cx) ->
	(match f y with
	   | Some(cy) ->
	       let cxy = Number.inter cx cy in
	       (match Number.cmp cxy cx with
		  | (Binrel.Same | Binrel.Super) -> 
		      None
		  | Binrel.Sub ->
		      Some(Atom.mk_in cxy x)
		  | Binrel.Disjoint ->
		      raise Exc.Inconsistent
		  | Binrel.Overlap ->
		      (match Number.d_singleton cxy with
			 | None ->
			     None
			 | Some(q) ->
			     Some(Atom.mk_equal x (Arith.mk_num q))))
	   | None -> 
	       (match Arith.d_add y with        (* hack for special case. *)
		  | Some([y1;y2]) ->            (* needs to be extended. *)
		      (match Arith.d_num y1 with
			 | Some(q) ->
			     let d = Number.add cx (Number.mk_singleton (Mpa.Q.minus q)) in
			     Some(Atom.mk_in d y2)
			 | None ->
			     None)
		  | _ -> 
		      None))
    | _ ->
	(match Arith.d_num y with
	   | Some(q) ->
	       Some(Atom.mk_in (Number.mk_singleton q) x)
	   | None ->
	       (match f y with
		  | Some(cy) ->
		      Some(Atom.mk_in cy x)
		  | None -> None))


(*s Merging. *)

let merge i f e s = 
  call "Merge" i e Pretty.eqn;
  let noinfer _ = None in
  let (s',xl',al') = 
    match i with
      | A -> 
	  let (la',xl,al) = A.merge (infera f) e s.la in
	  ({s with la = la'}, xl, al)
      | T ->
	  let (t',xl,al) = T.merge noinfer e s.t in
	  ({s with t = t'}, xl, al)
      | BV ->
	  let (bv',xl,al) = BV.merge noinfer e s.bv in
	  ({s with bv = bv'}, xl, al)
  in
  exit "Merge" i (xl',al') Pretty.infer;
  (s',xl',al')

let merge_all f e s =
  let noinfer _ = None in
  let (la',xl1,al1) = A.merge (infera f) e s.la in
  let (t',xl2,al2) = T.merge noinfer e s.t in
  let (bv',xl3,al3) = BV.merge noinfer e s.bv in
  ({la = la'; t = t'; bv = bv'},
   xl1 @ xl2 @ xl3,
   al1 @ al2 @ al3)


(*s Propagating a new constraint [c] for a variable [x]. *)

let propagate1 cnstrnt (x,c) s =
  try
    let y = A.apply s.la x in
    (match  cnstrnt y with
       | None -> []
       | Some(d) -> 
	   (match Number.cmp c d with
	      | Binrel.Disjoint ->
		  raise Exc.Inconsistent
	      | _ -> []))
  with 
      Not_found ->
	Set.fold
	  (fun y acc ->
	     match cnstrnt y with
	       | None -> acc
	       | Some(d) ->
		   let x = A.inv s.la y in
		   (match cnstrnt x with
		      | None ->
			  Atom.mk_in d x :: acc
		      | Some(c) ->
			  (match Number.cmp c d with
			     | Binrel.Disjoint -> 
				 raise Exc.Inconsistent
			     | (Binrel.Same | Binrel.Sub) -> 
				 acc
			     | Binrel.Super ->
				 let x = A.inv s.la y in
				 Atom.mk_in d x :: acc
			     | Binrel.Overlap ->
				 let x = A.inv s.la y in
				 let cd = Number.inter c d in
				 Atom.mk_in cd x :: acc)))
	  (A.use s.la x)
	  []

let propagate cnstrnt (x,c) s =
  call "Prop" A (x,c) Pretty.inn;
  let al = propagate1 cnstrnt (x,c) s in
  exit "Prop" A al (Pretty.list Pretty.atom);
  al


(*s Build canonical context by replacing all variables [x] with "canonical" 
 variables [f x]. *)

let inst f s =
  let la' = A.inst f s.la in
  let t' = T.inst f s.t in
  let bv' = BV.inst f s.bv in
  {s with la = la'; t = t'; bv = bv'}

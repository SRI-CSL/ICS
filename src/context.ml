
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
open Three
open Mpa
open Sym
open Th
(*i*)

(*s Decision procedure state. *)


type t = {
  mutable ctxt : Atom.Set.t;      (* Current context. *)
  mutable p : Partition.t;        (* Variable partitioning. *)
  eqs : Solution.t Th.Array.arr;  (* Theory-specific solution sets. *)
  mutable upper : int;            (* Upper bound on fresh variable index. *)
}

let empty = {
  ctxt = Atom.Set.empty;
  p = Partition.empty;
  eqs = Array.create Solution.empty;
  upper = 0
} 


(*s Accessors for components of partitioning. *)

let ctxt_of s = s.ctxt
let p_of s = s.p
let v_of s = Partition.v_of s.p
let d_of s = Partition.d_of s.p
let c_of s = Partition.c_of s.p
let eqs_of s = Array.get s.eqs
let upper_of s = s.upper


(*s Equality test. Do not take upper bounds into account. *)

let eq s t =              
 Partition.eq s.p t.p &&
 Array.for_all2 
   (fun eqs1 eqs2 -> 
      Solution.eq eqs1 eqs2) 
   s.eqs t.eqs


(*s Destructive updates. *)

let extend a s = 
  (s.ctxt <- Atom.Set.add a s.ctxt; s)

let update s i eqs = 
  (Array.set s.eqs i eqs; s)

let install s i (p, eqs) =
  s.p <- p;
  Array.set s.eqs i eqs;
  s

let union i e s =
  update s i (Solution.union i e (eqs_of s i))

let restrict i x s =
  update s i (Solution.restrict i x (eqs_of s i))

let name i (s, b) =
  let (x', ei') = Solution.name i (b, eqs_of s i) in
    (update s i ei', x')


(*s Shallow copying. *)

let copy s = {
  ctxt = s.ctxt;
  p = Partition.copy s.p;
  eqs = Array.copy s.eqs;
  upper = s.upper}


(*s Canonical variables module [s]. *)

let v s = V.find (v_of s)

let c s = C.apply (c_of s)

let d s = D.deq (d_of s)

let fold s f x = V.fold (v_of s) f (v s x)



(*s Constraint of [a] in [s]. *)

let cnstrnt s = 
  let rec of_term a =
    match a with
    | Var _  -> 
	c s (v s a)
    | App(Arith(op), xl) ->
	Arith.tau of_term op xl
    | App(Pp(op), xl) -> 
	Pp.tau of_term op xl
    | App(Bvarith(op), xl) -> 
	Bvarith.tau of_term op xl
    | App(Fun(op), xl) ->
	Apply.tau of_term op xl
    | _ -> 
	raise Not_found
  in
    Trace.func "context" "Cnstrnt" Term.pp Cnstrnt.pp
      of_term


(*s Choosing a variable. *)

let choose s = V.choose (v_of s)


(*s Pretty-printing. *)
  
let pp fmt s =
  let pps i sl =   
    if not(Solution.is_empty sl) then
      Solution.pp i fmt sl
  in
  Partition.pp fmt s.p;
  Array.iter (fun i eqs -> pps i eqs) s.eqs


(*s Parameterized operations on solution sets. *)

let mem i s = Solution.mem (eqs_of s i)

let use i s = Solution.use (eqs_of s i)

let apply i s = Solution.apply (eqs_of s i)

let find i s = Solution.find (eqs_of s i)

let rec inv i s =
  if Th.eq i Th.pprod then
    inv_pprod s
  else 
    Solution.inv (eqs_of s i)

(*s Search for largest match on rhs. For example, if [a] is
 of the form [x * y] and there is an equality [u = x^2 * y],
 then [inv_pprod s a] returns [u * x] if there is no larger
 rhs which matches [a]. *)

and inv_pprod s a =           
  let usea = 
    match a with
      | App(Pp(Mult), x :: _) -> (use Th.pprod s x)
      | App(Pp(Expt(_)), [x]) -> (use Th.pprod s x)
      | _ -> Set.empty
  in
  let lookup =
    Set.fold
      (fun x acc ->
	 try
	   let b' = apply Th.pprod s x in
	     (match Pp.div (a, b') with
		| None -> acc
		| Some(c') ->    (* [a * c' = b'] *)
		    (match acc with
		       | Some(_, _, b) when Pp.cmp b b' > 0 -> acc
		       | _ -> Some(x, c', b')))
	 with
	     Not_found -> acc)
      usea
      None
  in
    match lookup with
      | Some(x, c, _) -> 
	  Pp.mk_mult (v s x) c
      | None ->
	  raise Not_found


let equality i s = Solution.equality (eqs_of s i)


(*s Variable partitioning. *)

let rec is_equal s x y =
  match  Term.is_equal x y with
    | Three.X -> Partition.is_equal s.p x y
    | res -> res


(*s [sigma]-normal forms. *)

let sigma s f =
  match f with
    | Arith(op) -> Arith.sigma op
    | Product(op) -> Tuple.sigma op
    | Bv(op) -> Bitvector.sigma op
    | Coproduct(op) -> Coproduct.sigma op
    | Fun(op) -> Apply.sigma op
    | Pp(op) -> Pp.sigma op
    | Arrays(op) -> Arr.sigma op
    | Bvarith(op) -> Bvarith.sigma op
    | Uninterp _ -> mk_app f


(* Component-wise solver. Only defined for fully interpreted theories. *)

let solve i _ = 
  Trace.func "context" "solve" 
    Fact.pp_equal
    (Pretty.list Fact.pp_equal)
    (Th.solve i)

let fuse i e s =
  install s i (Solution.fuse i (s.p, eqs_of s i) [e])


let rec compose i e s =
  try
    let sl' = solve i s e in
      install s i (Solution.compose i (s.p, eqs_of s i) sl')
  with
      Exc.Unsolved -> 
	Format.eprintf "Warning: Incomplete Solver@.";
	ignore i e s

and ignore i e s =
  let (a, b, prf) = Fact.d_equal e in
  let (x', ei') = Solution.name i (a, eqs_of s i) in
  let (y', ei'') = Solution.name i (b, ei') in
  let e' = Fact.mk_equal x' y' None in
    union i e' s

let update p s = (s.p <- p; s)


(* Lookup terms on rhs of solution sets. *)
    
let lookup s a = 
  match a with
    | Var _ ->
	v s a
    | App(f, _) ->
	let i = Th.of_sym f in
	  try 
	    let x = inv i s a in
	      v s x
	  with 
	      Not_found -> a


(*s List all constraints with finite extension. *)

let rec split s =
  Atom.Set.union 
    (split_cnstrnt s) 
    (split_arrays s)

and split_cnstrnt s = 
  C.split (c_of s)

and split_arrays s = 
  Solution.fold
    (fun _ (b,_) acc1 ->
       match b with
	 | App(Arrays(Select), [upd1; j1]) ->
	     V.fold (v_of s)
	     (fun upd2 acc2 ->
		try
		  (match apply arr s upd2 with
		     | App(Arrays(Update), [_; i2; _]) ->
			 (match is_equal s i2 j1 with
			    | X -> Atom.Set.add (Atom.mk_equal (Fact.mk_equal i2 j1 None)) acc2
			    | _ -> acc2)
		     | _ -> 
			 acc1)
		with
		    Not_found -> acc1)
	     upd1 acc1
	 | _ -> acc1)
    (eqs_of s arr)
    Atom.Set.empty



(*s Administration of changed sets. For each of component [v], [d], [c] of the
 partition there is such a set stored in respective global variables [V.changed],
 [D.changed], and [C.changed]. Here, we define the change sets for the theory-specific
 solution sets. In addition, functions for saving, resetting, and restoring are provided. *)

module Changed = struct

  type t = Term.Set.t * Term.Set.t * Term.Set.t * Term.Set.t Array.arr

  let reset () =
    Partition.Changed.reset ();
    Solution.Changed.reset ()

  let save () =
    let (v, d, c) = Partition.Changed.save () in
    let e = Solution.Changed.save () in
      (v, d, c, e)

  let restore (v, d, c, e) =
    Partition.Changed.restore (v, d, c);
    Solution.Changed.restore e
    

  let stable () =
    Partition.Changed.stable () &&
    Solution.Changed.stable () 

  let in_v (v, _, _, _) = v
  let in_d (_, d, _, _) = d
  let in_c (_, _, c, _) = c
  let in_eqs i (_, _, _, e) = Array.get e i

  let pp fmt (v, d, c, e) =
    let ppset str xs = 
      if not(Set.is_empty xs) then
	begin
	  Format.fprintf fmt "\n%s: " str;
	  Pretty.set Term.pp fmt (Set.elements xs) 
	end 
    in
      ppset "v" v; ppset "d" d; ppset "c" c;
      Array.iter (fun i -> ppset (Th.to_string i)) e
 
end
  

(*s Update rules work on the following global variables together with the index
 for creating new variables. Within a [protect] environment, updates are performed
 destructively. Global variables are protected! *)

let protect f s =
  let k' = !Var.k in
  let r' = !V.removable in
  let ch' = Changed.save () in
    try
      Var.k := s.upper;
      Changed.reset ();
      V.removable := Term.Set.empty;
      let s' = f (copy s) in
	s'.upper <- !Var.k;
	Var.k := k';
	V.removable := r';
	Changed.restore ch';
	s'
    with
      | exc ->
	  Var.k := k';
	  V.removable := r';
	  Changed.restore ch';
	  raise exc

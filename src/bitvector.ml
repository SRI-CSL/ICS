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

open Mpa

(** Unsigned interpretation. *)
let bitv2nat b =
  Bitv.fold_right 
    (fun x acc -> 
       if x then 2 * acc + 1 else 2 * acc) 
    b 0

(** Unsigned bitvector of length [n] for integer [i]. Hashed. *)
let rec nat2bitv n i =
  let ht = Hashtbl.create 17 in
  let _ = Tools.add_at_reset (fun () -> Hashtbl.clear ht) in
    try
      Hashtbl.find ht (n, i)
    with
	Not_found -> 
	  let b = nat2bitv_rec n i in
	    Hashtbl.add ht (n, i) b; b
  
and nat2bitv_rec n i =
  assert(i >= 0);
  let rec loop acc i =
    if i <= 0 then acc else
      let acc' = (if i mod 2 = 0 then "0" else "1") ^ acc in
	loop acc' (i / 2)
  in
  let str = loop "" i in
  let m = String.length str in
    assert(n - m >= 0);
    let patch = String.make (n - m) '0' in
    let str' = patch ^ str in
      assert(String.length str' = n);
      Bitv.from_string str'


(** Theory definition. *)
let theory = Theory.create "bv" 

let is_theory = Theory.eq theory

let _ =
  Theory.Description.add theory
    "Theory of bitvectors"


module Sig = struct
  let th = theory
  type t =
    | Const of Bitv.t
    | Conc of int * int
    | Sub of int * int * int
  let to_string = function
    | Const(b) -> Format.sprintf "0b%s" (Bitv.to_string b)
    | Conc(n, m) -> Format.sprintf "conc[%d,%d]" n m
    | Sub(n, i, j) -> Format.sprintf "sub[%d,%d,%d]" n i j
  let name op = 
    Name.of_string (to_string op)
  let width = function
    | Const(c) -> Bitv.length c
    | Sub(n, i, j) -> j - i + 1
    | Conc(n, m) -> n + m
end

module Op = Funsym.Make(Sig)

let op a = Op.out (Term.sym_of a)
let args a = Term.Args.to_list (Term.args_of a)
 
let is_interp a =
  try Op.is_interp (Term.sym_of a) with Not_found -> false

let rec is_pure a =
  try
    (match op a, args a with
       | Sig.Const _, [] -> true
       | Sig.Conc _, [b1; b2] -> is_pure b1 && is_pure b2
       | Sig.Sub _, [b] -> is_pure b
       | _ -> false)
  with
      Not_found -> Term.is_var a


let d_const a =
  match op a, args a with
    | Sig.Const(c), [] -> c
    | _ -> raise Not_found

let d_conc a =
  match op a, args a with
    | Sig.Conc(n, m), [a1; a2] -> (n, m, a1, a2)
    | _ -> raise Not_found

let d_sub a = 
  match op a, args a with
    | Sig.Sub(n, i, j), [b] -> (n, i, j, b)
    | _ -> raise Not_found

let destructure a =
  try Some(op a, args a) with Not_found -> None

let width a =
  try
    let op = op a in
      Some(Sig.width op);
  with
      Not_found ->
	failwith "Bitvector.width: to do"


(** Constant bitvectors *)
let mk_const c = Term.mk_const (Op.inj(Sig.Const(c)))

let mk_zero n = mk_const (Bitv.create n false)
let mk_one n = mk_const (Bitv.create n true)
let mk_eps () = mk_const (Bitv.from_string "")

let is_const c a =
  try Bitv.equal (d_const a) c with Not_found -> false

let is_eps a =
  try Bitv.length (d_const a) = 0 with Not_found -> false

let is_zero a =
  try Bitv.all_zeros (d_const a) with Not_found -> false
 
let is_one a =
  try Bitv.all_ones (d_const a) with Not_found -> false

let rec iter f a =
  try
    (match op a, args a with
       | Sig.Const _, [] -> ()
       | Sig.Sub _, [x] -> iter f x
       | Sig.Conc _, [x; y] -> iter f x; iter f y
       | _ -> invalid_arg "Bitvector.iter: invalid term")
  with
      Not_found -> f a

let rec fold f a e =
  try
    (match op a, args a with
       | Sig.Const _, [] -> e
       | Sig.Sub _, [x] -> fold f x e
       | Sig.Conc _, [x; y] -> fold f x (fold f y e)
       | _ ->invalid_arg "Bitvector.fold: invalid term")
  with
      Not_found -> f a e

let rec exists p a =
  try
    (match op a, args a with
       | Sig.Sub _, [b] -> exists p b
       | Sig.Conc(n,m), [b1; b2] -> (exists p b1) || (exists p b2)
       | _ ->  false)
  with 
      Not_found -> p a


let rec mk_sub n i j a =
  assert (0 <= i && j < n && n >= 0);
  if i = 0 && j = n - 1 then 
    a 
  else if j < i then
    mk_eps()
  else 
    try
      (match op a, args a with
	 | Sig.Const(b), [] -> 
	     mk_const (Bitv.sub b i (j - i + 1))
	 | Sig.Sub(m,k,l), [x] ->
	     mk_sub m (k + i) (k + j) x
	 | Sig.Conc(n,m), [x; y] ->
	     if j < n then
	       mk_sub n i j x
	     else if n <= i then
	       mk_sub m (i - n) (j - n) y
	     else 
	       (assert(i < n && n <= j);
		let t = mk_sub n i (n - 1) x in
		let u = mk_sub m 0 (j - n) y in
		  mk_conc (n - i) (j - n + 1) t u)
	 | _ ->
	     invalid_arg "Bv.mk_sub: ill-formed expression")
    with
	Not_found -> Term.mk_unary (Op.inj(Sig.Sub(n, i, j))) a
	
and mk_conc n m a b =
  assert (0 <= n && 0 <= m);
  match n = 0, m = 0 with
    | true, true -> mk_eps()
    | true, false -> b
    | false, true -> a
    | false, false ->
	(try merge n m a b with Not_found -> 
	   Term.mk_binary (Op.inj(Sig.Conc(n, m))) a b)

and merge n m a b =
  match destructure a, destructure b with
    | _, Some(Sig.Conc(m1,m2), [b1;b2]) -> 
	mk_conc (n + m1) m2 (mk_conc n m1 a b1) b2
    | Some(Sig.Const(c),[]), Some(Sig.Const(d),[]) -> 
	mk_const (Bitv.append c d)
    | Some(Sig.Sub(n,i,j),[x]), Some(Sig.Sub(m,j',k), [y])
	when j' = j + 1 && Term.eq x y ->
	assert(n = m);
	  mk_sub n i k x
    | Some(Sig.Conc(m1, m2), [b1; b2]), Some(Sig.Const(d), []) ->
	let c = d_const b2 in
	let n = Bitv.length d in
	  mk_conc m1 (m2 + n) b1 (mk_const (Bitv.append c d))
    | _ -> 
	raise Not_found
 
and mk_conc3 n m k a b c =
  mk_conc n (m + k) a (mk_conc m k b c)

and cut n i a =
  (mk_sub n 0 (i - 1) a, mk_sub n i (n - 1) a)

(** Mapping over bitvector terms. *)
let map f =
  let rec mapf a =
    try
      (match op a, args a with
	 | Sig.Const(_), [] ->
	     a
	 | Sig.Sub(n,i,j), [x] -> 
	     let x' = mapf x in
	       if x == x' then a else mk_sub n i j x'
	 | Sig.Conc(n,m), [x;y] -> 
	     let x' = mapf x and y' = mapf y in
	       if x == x' && y == y' then a else mk_conc n m x' y'
	 | _ -> 
	     assert false)
    with
	Not_found -> f a
  in
  mapf

let apply (x, b) = 
  map (fun y -> if Term.eq x y then b else y)

let sigma f l =
  assert(Op.is_interp f);
  match Op.out f, Term.Args.to_list l with
    | Sig.Const(c), [] -> mk_const c
    | Sig.Sub(n, i, j), [x] -> mk_sub n i j x
    | Sig.Conc(n, m), [x; y] ->  mk_conc n m x y
    | _ -> invalid_arg "Bv.sigma: invalid bitvector term."

 
(** [n]-ary concatenation of some concatenation normal form *)
let rec mk_iterate n b = function
  | 0 -> mk_eps ()
  | 1 -> b
  | k -> mk_conc n (n * (k-1)) b (mk_iterate n b (k-1))


(** Flattening out concatenations. The result is a list of equivalent
 equalities not containing any concatenations. *)
let decompose e =
  let d_conc a =
    try
      (match op a, args a with
	| Sig.Conc(n, m), [a1; a2] -> Some(n, m, a1, a2)
	| _ -> None)
    with
	Not_found -> None
  in
  let rec loop acc = function
    | [] -> acc
    | (a,b) :: el when Term.eq a b ->
	loop acc el
    | (a, b) :: el -> 
	let (acc',el') = 
	  match d_conc a, d_conc b with   
	    | Some(n1,m1,x1,y1), Some(n2,m2,x2,y2) ->
		if n1 = n2 then
		  (acc, ((x1,x2) :: (y1,y2) :: el))
		else if n1 < n2 then
		  let (x21,x22) = cut n2 n1 x2 in
		  let e1 = (x1,x21) in
		  let e2 = (y1, mk_conc (n2-n1) m2 x22 y2) in
		    (acc, (e1 :: (e2 :: el)))
		else (* n1 > n2 *)
		  let (x11,x12) = cut n1 n2 x1 in
		  let e1 = (x11,x2) in
		  let e2 = (mk_conc (n1-n2) m1 x12 y1, y2) in
		    (acc, (e1 :: e2 :: el))
	    | Some(n,m,x,y), None ->
		let e1 = (mk_sub (n+m) 0 (n-1) b, x) in
		let e2 = (mk_sub (n+m) n (n+m-1) b, y) in
		  (acc, (e1 :: e2 :: el))
	    | None, Some(n,m,x,y) ->
		let e1 = (mk_sub (n+m) 0 (n-1) a, x) in
		let e2 = (mk_sub (n+m) n (n+m-1) a, y) in
		  (acc, (e1 :: e2 :: el))
	    | None, None ->
		(((a, b) :: acc), el)
	in
	  loop acc' el'
  in
    loop [] [e] 
      

let fresh = ref []
	      
(** Creating fresh bitvector variables for solver. 
  The index variable are always reset to the current value
  when solver is called. *)
let mk_fresh n =
  assert (n >= 0);
  if n = 0 then mk_eps () else 
    let x = Term.mk_fresh_var "bv" in
      fresh := x :: !fresh;
      x
	
let is_fresh x =
  let eqx = Term.eq x in
    List.exists eqx !fresh
      
let rec solve ((a, b) as e) =
  match width a, width b with
    | Some(n), Some(m) when n <> m -> 
	raise Exc.Inconsistent
    | _ -> 
	fresh := [];         (* initialize fresh variables. *)
	solvel ([e], [])
  
and solvel (el, sl) =
  match el with
    | [] -> sl
    | (a, b) :: el when Term.eq a b ->
	solvel (el,sl)
    | (a, b) :: el ->
	(match destructure a, destructure b  with   
	   | None, Some _ when not(occurs a b) ->      (* Check if solved. *)
	       solvel (add a b (el, sl))
	   | Some _, None  when not(occurs b a) ->
	       solvel (add b a (el, sl))
	   | Some(Sig.Conc _, _), _                    (* Decomposition of [conc] *)
	   | _, Some(Sig.Conc _, _) ->
	       solvel (decompose (a, b) @ el, sl)
	   | Some(Sig.Const(c), []), Some(Sig.Const(d), []) ->
	       if Pervasives.compare c d = 0 then 
		 solvel (el,sl)
	       else 
		 raise Exc.Inconsistent
	   | Some(Sig.Sub(n,i,j),[x]), Some(Sig.Sub(m,k,l),[y]) when Term.eq x y ->
	       assert(n = m);
	       let (x, b) = solve_sub_sub x n i j k l in
		 solvel (add x b (el,sl))
	   | Some(Sig.Sub(n,i,j),[x]),  Some(Sig.Const _, []) ->
	       assert(n-j-1 >= 0);
	       assert(i >= 0);
	       let b' = 
		 mk_conc3 i (j-i+1) (n-j-1) 
		   (mk_fresh i) b (mk_fresh (n-j-1)) 
	       in
		 solvel (add x b' (el,sl))
	   | Some(Sig.Const _, []), Some(Sig.Sub(n,i,j), [x]) -> 
               assert(n-j-1 >= 0); 
	       assert(i >= 0);
	       let a' = mk_conc3 i (j-i+1) (n-j-1) (mk_fresh i) 
			  a (mk_fresh (n-j-1)) in
	       solvel (add x a' (el,sl))
	   | _ ->
	       assert(not(is_interp a));
	       assert(not(is_interp b));
	       let a, b = Term.orient(a, b) in
		 solvel (add a b (el,sl)))

(* Solving [xn[i:j] = xn[k:l]] *)
and solve_sub_sub x n i j k l =  
  assert (n >= 0 && i < k && j-i = l-k);
  let lhs = 
    mk_sub n i l x 
  in
  let rhs =
    if (l-i+1) mod (k-i) = 0 then
      let a = mk_fresh (k-i) in
      mk_iterate (k-i) a ((l-i+1)/(k-i))
    else
      let h = (l-i+1) mod (k-i) in
      let h' = k-i-h in
      let a = mk_fresh h in
      let b = mk_fresh h' in
      let nc = (l-i-h+1)/(k-i) in
      let c = mk_iterate (h + h') (mk_conc h' h b a) nc in
      mk_conc h (nc * (h' + h)) a c
  in
    (lhs, rhs)


and occurs x a =
  assert(not(is_interp x));
  try
    is_interp a &&
    List.exists (occurs x) (args a)
  with
      Not_found -> Term.eq x a


(** Adding a solved pair [a |-> b] to the list of solved forms [sl],
 and propagating this new binding to the unsolved equalities [el] and 
 the rhs of [sl]. It also makes sure that fresh variables [a] are never
 added to [sl] but only propagated. *)
and add x b (el, sl) =
  assert(not(is_interp x));
  if Term.eq x b then 
    (el, sl)
  else if inconsistent x b then
    raise Exc.Inconsistent
  else 
    let (x, b) = orient (x, b) in
    let el' = subst el (x, b) 
    and sl' = compose sl (x, b) in
      (el', sl')
		
(** Ensure [y = x] with [y] fresh and [x] not fresh can not happen. *)
and orient (a, b) =
  if Term.is_var a && Term.is_var b then
    if is_fresh a && not(is_fresh b) then
      (b, a)
    else 
      Term.orient (a, b)
  else
    (a, b)
     
and subst el (x, a) = 
  let apply_to_eq (b1, b2) =
    (apply (x, a) b1, apply (x, a) b2)
  in
    List.map apply_to_eq el

and compose sl (x, a) =
  failwith "to do"
(*
  if is_fresh x && is_fresh a then   (* equality between fresh variables *)
    Term.Subst.fuse sl x a           (* can be dropped. *)
  else 
    Term.Subst.compose sl x a;
  sl
*)

and inconsistent a b =
  match width a, width b with
    | Some(n), Some(m) -> n <> m
    | _ -> false


(** Two terms are disequal iff there is at least one
  position on which bitconstants differ. *)
let is_diseq a b =
  try
    let sl = solve (a, b) in
      sl <> []
  with
      Exc.Inconsistent -> true


open Mpa


(** Definition of a Shostak theory for bitvectors. Currently,
  no branching is performed. *)
module T: Shostak.T = struct
  let th = theory
  let can = sigma
  let map = map
  let solve a b = failwith "to do"
  let disjunction _ = raise Not_found
end

module P = V

(*
(** Inference system as a variant of Shostak inference systems. *)
module Infsys = struct

  module I = Shostak.Infsys(T)

  let reset = I.reset
  let is_unchanged = I.is_unchanged
  let current = I.current
  let initialize = I.initialize
  let finalize = I.finalize

  let abstract = I.abstract
  let process_equal = I.process_equal
  let process_diseq = I.process_diseq
  let propagate_equal = I.propagate_equal
  let propagate_diseq = I.propagate_diseq
  let branch () = raise Not_found
  let normalize = I.normalize


  (** If [x] is interpreted over bitvectors of length [n], then
    - if there are [2^n] different bitvector constants of length [n]
    disequal to [x], then infer inconsistency,
    - if there are [2^n-1] different bitvector constant of length [n]
    disequal to [x], then generate the appropriate equality. 
    Following code has potential for optimization. *)
 

  (** Check if [a] is disequal from the bitvector constant [b]
    with unsigned interpretation [n]. *)
  let is_const_diseq (p, s) a n =
    match width a with
      | Some(l) ->
	  let b = mk_const (nat2bitv l n) in
	    failwith "to do"
      | None ->
	  None

(*
    (* assert(Fact.Diseq.is_var d); *)
    let to_option f a = try Some(f a) with Not_found -> None in
    let d = Fact.Diseq.map (can (p, s)) d in
    let (a, b, rho) = d in
      match to_option Bitvector.d_const a, to_option Bitvector.d_const b with
	| Some(c), Some(d) ->
	    if Bitv.equal c d then
	      raise(Jst.Inconsistent(rho))
	    else 
	      (p, s)
	| None, None ->
	    (p, s)
	| Some(c), None ->
	    let n = Bitvector.bitv2nat c in
	      process_const_diseq (p, s) (b, n, rho)
	| None, Some(d) ->
	    let m = Bitvector.bitv2nat d in
	      process_const_diseq (p, s) (a, m, rho)

  and two_to_the_n_sub_one n =
    let ht = Hashtbl.create 4 in
    let _ = Tools.add_at_reset (fun () -> Hashtbl.clear ht) in
      try
	Hashtbl.find ht n
      with
          Not_found ->
            let m = Z.sub (Z.expt Z.two n) Z.one in
              Hashtbl.add ht n m; m


  and process_const_diseq (p, s) (a, n, rho) =
    assert(n >= 0);
    match Bitvector.width a with
      | None ->	
	  (p, s)
      | Some(l) ->
	  let lo = Mpa.Z.zero
	  and hi = two_to_the_n_sub_one l in
	    (p, s)
*)

end

*)

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


(** A term is either a variable or an application of a function symbol
  to a possibly empty list of arguments. In addition, each term has
  an integer slot that can be used as a hash function for terms.  In 
  addition, these indices are unique for variables. *)
type t =
  | Var of Var.t * int
  | App of Sym.t * t list * int
  

type trm = t  
    (** Synonym for avoiding name clashes *)


(** {6 Hashing} *)

(** Every term carries a hash value with it. For two variables [x] and [y],
  this value is {i unique}, that is, [hash x] equals [hash y] iff {!Term.eq}[x y]. *)
let hash = function
  | Var(_, hsh) -> hsh
  | App(_, _, hsh) -> hsh


(** {6 Recognizers} *)

let is_var = function Var _ -> true | _ -> false
let is_app = function App _ -> true | _ -> false
let is_const = function App(_,[], _) -> true | _ -> false


(** {6 Pretty-Printing} *)

let rec pp fmt a =
  match a with
    | Var(x, hsh) -> 
	Var.pp fmt x;
	pp_hash fmt hsh
    | App(f, l, hsh) ->
	Sym.pp pp fmt (f, l);
	pp_hash fmt hsh

and pp_hash fmt hsh =
  if !debug then Format.fprintf fmt "{%d}" hsh

and debug = ref false

let to_string = Pretty.to_string pp


(** {6 Variables} *)

module Var = struct

  (** Every variable has a {i unique} index associated with it. *)
  module Index = struct
    let current = ref 0
    (* let _ = Tools.add_at_reset (fun () -> current := 0) *)
 
    let create x =
      let a = Var(x, !current) in
	incr(current);
	a
  end
 

  (** Constructing hashconsed external variables *)
  let mk_var =
    let module ExternalHash = Hashtbl.Make(
    struct
      type t = Name.t * Var.Cnstrnt.t
      let equal (n1, d1) (n2, d2) = Name.eq n1 n2 && d1 = d2
      let hash (n, _) = Name.hash n
    end)
    in
    let table = ExternalHash.create 17 in
    let _ =  Tools.add_at_reset (fun () -> ExternalHash.clear table) in 
      fun n d ->
	try
	  ExternalHash.find table (n, d)
	with
	    Not_found ->
	      let x = Var.mk_external n d in
	      let a = Index.create x in
		ExternalHash.add table (n, d) a; a
	  

  (** Global variable for creating fresh variables. *)
  let k = ref (-1)
  let _ = Tools.add_at_reset (fun () -> k := (-1))

  let fresh_index = function
    | Some(i) -> i
    | None -> incr(k); !k


  (** Constructing hashconsed rename variables *)
  let mk_rename =
    let module RenameHash = Hashtbl.Make(
      struct
	type t = Name.t * int * Var.Cnstrnt.t
	let equal (x1, k1, d1) (x2, k2, d2) = 
	  Name.eq x1 x2 && k1 = k2 && d1 = d2
	let hash (_, k, _) = k
      end)
    in
    let table = RenameHash.create 17 in
    let _ =  Tools.add_at_reset (fun () -> RenameHash.clear table) in 
      fun n i d -> 
	let k = fresh_index i in
	  try
	    RenameHash.find table (n, k, d)
	  with
	      Not_found ->
		let x = Var.mk_rename n k d in
		let a = Index.create x in
		  RenameHash.add table (n, k, d) a; a


  (** Constructing hashconsed slack variables. *)
  let mk_slack =
    let module SlackHash = Hashtbl.Make(
      struct
	type t = int * Var.slack
	let equal (k1, sl1) (k2, sl2) = 
	  k1 = k2 && sl1 = sl2
	let hash (k, _) = k
      end)
    in
    let table = SlackHash.create 17 in
    let _ =  Tools.add_at_reset (fun () -> SlackHash.clear table) in 
      fun i sl -> 
	let k = fresh_index i in 
	  try
	    SlackHash.find table (k, sl)
	  with
	      Not_found ->
		let x = Var.mk_slack k sl in
		let a = Index.create x in
		  SlackHash.add table (k, sl) a; a


  (** Construct hashconsed fresh variables local to some theory. *)
  let mk_fresh =
    let module FreshHash = Hashtbl.Make(
      struct
	type t = Th.t * int * Var.Cnstrnt.t
	let equal (th1, k1, d1) (th2, k2, d2) = 
	  k1 = k2 && th1 = th2 && d1 = d2
	let hash (_, k, _) = k
      end)
    in
    let table = FreshHash.create 17 in
    let _ =  Tools.add_at_reset (fun () -> FreshHash.clear table) in 
      fun th i d -> 
	let k = fresh_index i in 
	  try
	    FreshHash.find table (th, k, d)
	  with
	      Not_found ->
		let x = Var.mk_fresh th k d in
		let a = Index.create x in
		  FreshHash.add table (th, k, d) a; a



  (** Construct hashconsed "constant" variables *)
  let mk_const =
    let module FreshHash = Hashtbl.Make(
      struct
	type t = Th.t * int * Var.Cnstrnt.t
	let equal (th1, k1, d1) (th2, k2, d2) = 
	  k1 = k2 && th1 = th2 && d1 = d2
	let hash (_, k, _) = k
      end)
    in
    let table = FreshHash.create 17 in
    let _ =  Tools.add_at_reset (fun () -> FreshHash.clear table) in 
      fun th i d -> 
	let k = fresh_index i in 
	  try
	    FreshHash.find table (th, k, d)
	  with
	      Not_found ->
		let x = Var.mk_const th k d in
		let a = Index.create x in
		  FreshHash.add table (th, k, d) a; a


  let index_of = function
    | Var(_, idx) -> idx
    | a -> invalid_arg("Getting unique index of nonvariable term " ^ to_string a)

  let eq = (==)

  (** Variable ordering *)
  let cmp x y = 
    if x == y then 0 else Var.cmp x y 

  (** Syntactic variable ordering, does not obey {!Var.cmp}. *)
  let compare x y =
    match x, y with
      | Var(_, n), Var(_, m) ->
	  assert(if n <> m then not(eq x y) else eq x y);
	  if n < m then -1 else if n == m then 0 else 1
      | App _, _ ->
	  invalid_arg("Term.Var.compare: getting unique index of nonvariable term " ^ to_string x)
      | _, App _ ->
	  invalid_arg("Term.Var.compare: getting unique index of nonvariable term " ^ to_string y)

  module Set = Set.Make(
    struct
      type t = trm
      let compare = compare
    end)

  module Map = Map.Make(
    struct
      type t = trm
      let compare = compare
    end)


  (** {7 Recognizers} *)

  let is_zero_slack = function Var(x, _) -> Var.is_zero_slack x | _ -> false
  let is_nonneg_slack = function Var(x, _) -> Var.is_nonneg_slack x | _ -> false
  let is_slack x = is_zero_slack x || is_nonneg_slack x
  let is_external = function Var(x, _) -> Var.is_var x | _ -> false
  let is_rename = function Var(x, _) -> Var.is_rename x | _ -> false
  let is_fresh i = function Var(x, _) -> Var.is_fresh i x | _ -> false
  let is_const = function Var(x, _) -> Var.is_const x | _ -> false
  let is_internal = function Var(x, _) -> Var.is_internal x | _ -> false
 
  let d_external = function
    | Var(x, _) -> Var.d_external x 
    | _ -> raise Not_found


  (** {7 Accessors} *)

  let dom_of = function
    | Var(x, _) -> Var.dom_of x
    | _ -> raise Not_found

 let width_of = function
    | Var(x, _) -> Var.width_of x
    | _ -> raise Not_found
       
  let name_of = function 
    | Var(x, _) -> Var.name_of x 
    | _ -> raise Not_found

  let cnstrnt_of = function 
    | Var(x, _) -> Var.cnstrnt_of x 
    | _ -> raise Not_found

  let is_dom d a =
    try Dom.eq (dom_of a) d with Not_found -> false

  let is_int = function
    | Var(x, _) -> Var.is_int x
    | _ -> false

  let is_real = function
    | Var(x, _) -> Var.is_real x
    | _ -> false


end 

module App = struct

  let mk_const c =
    App(c, [], Sym.hash c)

  let mk_app f l =
    match l with
    | [] -> 
	mk_const f
    | [a] -> 
	App(f, l , (14007 + (Sym.hash f) + hash a) land 0x3FFFFFFF)
    | [a; b] -> 
	App(f, l, (19007 + (Sym.hash f) + hash a + hash b) land 0x3FFFFFFF)
    | [a; b; c] -> 
	App(f, l, (17007 + (Sym.hash f) + hash a + hash b + hash c) land 0x3FFFFFFF)
    | l -> 
	App(f, l, (27007 + (Sym.hash f) + (List.fold_left (fun h a -> h+hash a) 1 l)) land 0x3FFFFFFF)

  let destruct a =
    match a with App(f, l, _) -> (f, l) | _ -> raise Not_found

  let sym_of a = 
    match a with App(f,_, _) -> f | _ -> raise Not_found

  let args_of a = 
    match a with App(_,l, _) -> l | _ -> raise Not_found

  let theory_of = function
    | App(f, _, _) -> Sym.theory_of f 
    | _ -> raise Not_found

end


(** Syntactic term equality *)
let rec eq a b = 
  (hash a == hash b) &&      (* [eq] only if hash values coincide *)
  eq1 a b

and eq1 a b =
  (a == b)  ||               (* variables are hashconsed *)
  (match a, b with
     | App(f, l, _), App(g, m, _) -> 
	 (match l, m with
	    | [], [] -> Sym.eq f g
	    | [], _ -> false  (* quick failures *)
	    | _, [] -> false
	    | [x], [y] -> Sym.eq f g && eq x y
	    | [_], _ -> false
	    | _, [_] -> false
	    | [x1; y1], [x2; y2] -> Sym.eq f g && eq x1 x2 && eq y1 y2
	    | [_; _], _ -> false
	    | _, [_; _] -> false
	    | _ -> Sym.eq f g && eql l m)
     | _ -> false)

and eql al bl =
  try List.for_all2 eq al bl with Invalid_argument _ -> false


(** Term Ordering *)    
let rec cmp a b = 
  match a, b with  
    | Var(x, _), Var(y, _) -> Var.cmp x y
    | Var _, App _ -> 1
    | App _, Var _ -> -1
    | App(f, l, _), App(g, m, _) ->
	(match l, m with
	   | [], [] -> 
	       Sym.cmp f g
	   | [], _ -> -1
	   | _, [] -> 1
	   | [x], [y] -> 
	       let c1 = Sym.cmp f g in 
		 if c1 <> 0 then c1 else cmp x y
	   | [_], _ -> -1
	   | _, [_] -> 1
	   | [x1; x2], [y1; y2] ->
	       let c1 = Sym.cmp f g in 
		 if c1 <> 0 then c1 else
		   let c2 = cmp x1 y1 in
		     if c2 <> 0 then c2 else cmp x2 y2
	   | [_; _], _ -> -1
	   | _, [_; _] -> 1
	   | _ ->
	       let c1 = Sym.cmp f g in
		 if c1 != 0 then c1 else cmpl l m)
 
and cmpl l m =
  let rec loop c l m =
    match l, m with
      | [], [] -> c
      | [], _  -> -1
      | _,  [] -> 1
      | x :: xl, y :: yl -> 
	  if c != 0 then 
	    loop c xl yl 
	  else 
	    loop (cmp x y) xl yl
  in
  loop 0 l m

let (<<<) a b = (cmp a b <= 0)

let orient ((a, b) as e) =
  if cmp a b >= 0 then e else (b, a)


(** {i Syntactic term ordering}. 
  [compare] is faster than [cmp] and is used for building sets
  and maplets. It does not obey the variable ordering {!Var.cmp} *)
let rec compare a b =
  let ha = hash a and hb = hash b in
    if ha < hb then
      -1
    else if ha = hb then
      if eq1 a b then 0 else Pervasives.compare a b
    else 
      1

(** Some recognizers... *)
let is_equal a b =
  if eq a b then Three.Yes else 
    match a, b with                
      | App(c, [], _), App(d, [], _) 
	  when Sym.theory_of c = Sym.theory_of d 
	    && not(Th.is_uninterpreted (Sym.theory_of c))
	    -> Three.No
      | _ -> 
	  Three.X

(** Mapping over list of terms. Avoids unnecessary consing. *)
let rec mapl f l =
  match l with
    | [] -> []
    | a :: l1 ->
	let a' = f a and l1' = mapl f l1 in
	  if l1 == l1' && eq a' a then l else a' :: l1'
	    

(** Association lists for terms. *)   
let rec assq a = function
  | [] -> raise Not_found
  | (x,y) :: xl -> if eq a x then y else assq a xl


(** Iteration over terms. Apply [f] at variable positions. *)
let rec fold f a acc =
  match a with
    | Var _ -> f a acc
    | App(_, l, _) -> List.fold_right (fold f) l acc

let rec iter f a  =
  match a with
    | Var _ -> f a
    | App(_, l, _) -> List.iter (iter f) (App.args_of a)

exception Found of t

let choose p a =
  let select x = if p x then raise(Found(x)) in
    try
      iter select a;
      raise Not_found
    with
	Found(x) -> x
	
let rec for_all p a  =
  p a && 
  match a with
    | Var _ -> true
    | App(_, l, _) -> List.for_all (for_all p) l

let rec subterm a b =
  let rec sub_a b = 
    eq a b ||
    match b with
      | Var _ -> false
      | App(_, bl, _) -> sub_a_l bl
  and sub_a_l = function
    | [] -> false
    | b :: bl -> sub_a b || sub_a_l bl
  in
    sub_a b    

let occurs x b = subterm x b

let is_pure i =
  let rec loop = function
    | Var _ -> true
    | App(f, al, _) -> Sym.theory_of f = i && List.for_all loop al
  in
    loop

(** Facts are partitioned into 
  - facts over {i variable} terms
  - facts over {i pure} terms with all function symbols drawn 
  from a single theory [i]
  - facts over {i mixed} terms. *)
type status = 
  | Variable
  | Pure of Th.t
  | Mixed of Th.t * t

let mk_pure =
  let u = Pure(Th.u)
  and la = Pure(Th.la)
  and bv = Pure(Th.bv)
  and p = Pure(Th.p)
  and cop = Pure(Th.cop)
  and app = Pure(Th.app) 
  and set = Pure(Th.set)
  and arr = Pure(Th.arr)
  and nl = Pure(Th.nl) in
    function
      | Th.Uninterpreted -> u
      | Th.Shostak(i) -> 
	  (match i with
             | Th.LA -> la
             | Th.BV -> bv
             | Th.P -> p
             | Th.COP -> cop
             | Th.APP -> app
             | Th.SET -> set)
      | Th.Can(i) ->
	  (match i with Th.NL -> nl | Th.ARR -> arr)

  

let pp_status fmt = function
  | Variable ->
      Format.fprintf fmt "Var"
  | Pure(i) ->
      Format.fprintf fmt "Pure(%s)" (Th.to_string i)
  | Mixed(i, a) -> 
      Format.fprintf fmt "Mixed(%s, %s)" (Th.to_string i) (Pretty.to_string pp a)

let rec status = function
  | Var _ -> 
      Variable
  | App(f, al, _) -> 
      let i = Sym.theory_of f in
      let rec loop = function
	| [] ->
	    mk_pure(i)
	| a :: al -> 
	    (match status a with
	       | Variable -> loop al
	       | Pure(j) -> 
		   if i = j then loop al else Mixed(j, a)
	       | (Mixed _ as m) -> m)
      in
	loop al



(** {6 Sets and maps of terms.} *)

module Set2 = Set.Make(
  struct
    type t = trm * trm
    let compare (a1, a2) (b1, b2) =
      let cmp1 = compare a1 b1 in
	if cmp1 <> 0 then cmp1 else compare a2 b2
  end)

module Set = Set.Make(
  struct
    type t = trm
    let compare = compare
  end)

module Map = Map.Make(
  struct
    type t = trm
    let compare = compare
  end)


(** Set of variables. *)
let rec vars_of a = 
  match a with
    | Var _ -> 
	Var.Set.singleton a
    | App(_, al, _) ->
	List.fold_left 
	  (fun acc b ->
	     Var.Set.union (vars_of b) acc)
	  Var.Set.empty
	al


(** {6 Encoding of equalities} *)

module Equal = struct
   type t = trm * trm
   let lhs (a, _) = a
   let rhs (_, b) = b
   let pp fmt (a, b) = Pretty.infix pp "=" pp fmt (a, b)
   let make = orient
   let destruct e = e
   let compare (a1, b1) (a2, b2) =
     let res = cmp a1 a2 in if res = 0 then cmp b1 b2 else res
   let is_var (a, b) = is_var a && is_var b       
   let is_pure i (a, b) = is_pure i a && is_pure i b
end



(** {6 Term Substitution} *)

type apply = t * t -> t -> t
type map = (t -> t) -> (t -> t)

module Subst = struct

  type t = (trm * trm) list

  let lookup el x =
    let rec loop = function
      | [] -> raise Not_found
      | (a, b) :: el -> if eq x a then b else loop el
    in
      loop el

  let invlookup el pred =
    fst(List.find  (fun (_, b) -> pred b) el)

  let empty = []

  let fuse apply (y, b) sl = 
    List.fold_right
      (fun (x, c) acc ->
	 let c' = apply (y, b) c in
	   if eq x c' then acc else
	     (x, c') :: acc)
      sl []


  let compose apply (y, b) sl =
    if eq y b then sl else
      (y, b) :: (fuse apply (y, b) sl)

  let pp = Pretty.map pp pp

  let apply map sl a = 
    let jl = ref [] in
    let lookup y =
      let rec loop = function
	| [] -> y
      | ((x, b), j) :: sl' ->
	  if eq x y then (jl := j :: !jl; b) else  loop sl'
      in
	loop sl
    in
    let b = map lookup a in
      (b, !jl)

  let fold = List.fold_right  

end

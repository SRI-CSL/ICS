
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
 * 
 * Author: Harald Ruess
 *)

open Mpa
open Format
open Sym


type t =
  | Var of Var.t
  | App of Sym.t * t list

type trm = t  
    (** Synonym for avoiding name clashes *)

(** {6 Term comparison} *)

let rec cmp a b =
  match a, b with  
    | Var(x), Var(y) -> Var.cmp x y
    | Var _, App _ -> 1
    | App _, Var _ -> -1
    | App(f, l), App(g, m) ->
	let c1 = Sym.cmp f g in
	if c1 != 0 then c1 else cmpl l m
 
and cmpl l m =
  let rec loop c l m =
    match l, m with
      | [], [] -> c
      | [], _  -> -1
      | _,  [] -> 1
      | x:: xl, y:: yl -> 
	  if c != 0 then loop c xl yl else loop (cmp x y) xl yl
  in
  loop 0 l m



let (<<<) a b = (cmp a b <= 0)

let orient ((a, b) as e) =
  if cmp a b >= 0 then e else (b, a)


(** {6 Hashing} *)

let rec hash = function
  | Var(x) -> 
      Var.hash x
  | App(f, l) -> 
      ((Sym.hash f) + (List.fold_left (fun h a -> h + hash a) 1 l)) land 0x3FFFFFFF


(** {6 Recognizers} *)

let is_var = function Var _ -> true | _ -> false
let is_app = function App _ -> true | _ -> false
let is_const = function App(_,[]) -> true | _ -> false


(** {6 Pretty-Printing} *)

let pretty = ref true  (* Infix/Mixfix output when [pretty] is true. *)

let rec pp fmt a =
  let str = Pretty.string fmt in
  let term = pp fmt in
  let args =  Pretty.tuple pp fmt in
    match a with
      | Var(x) -> 
	  Var.pp fmt x
      | App(f, l) ->
	  (match f, l with
	     | Pair(Car), [App(Coproduct(OutR), [x])] ->
		 str "hd"; str "("; term x; str ")"
	     | Pair(Cdr), [App(Coproduct(OutR), [x])] ->
		 str "tl"; str "("; term x; str ")"
	     | Coproduct(InL), [App(Pair(Cons), [x; xl])] ->
		 Pretty.infix pp "::" pp fmt (x, xl)
	     | Coproduct(InR), [App(Pair(Cons), [])] ->
		 str "[]"
	     | _ -> 
		 Sym.pp pp fmt (f, l))

let to_string = Pretty.to_string pp


(** {6 Variables} *)

module Var = struct

  (** Constructing hashconsed external variables *)
  let mk_var =
    let module ExternalHash = Hashtbl.Make(
    struct
      type t = Name.t * Dom.t option
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
	      let x = Var(Var.mk_external n d) in
		ExternalHash.add table (n, d) x; x


  (** Constructing hashconsed binding variables *)
  let mk_free = 
    let table = Hashtbl.create 17 in
    let _ =  Tools.add_at_reset (fun () -> Hashtbl.clear table) in 
      fun k ->
	try
	  Hashtbl.find table k
	with
	    Not_found ->
	      let x = Var(Var.mk_free k) in
		Hashtbl.add table k x; x
		  

  (** Global variable for creating fresh variables. *)
  let k = ref (-1)
  let _ = Tools.add_at_reset (fun () -> k := 0)

  let index = function
    | Some(k) -> k
    | None -> incr(k); !k


  (** Constructing hashconsed rename variables *)
  let mk_rename =
    let module RenameHash = Hashtbl.Make(
      struct
	type t = Name.t * int * Dom.t option
	let equal (x1, k1, d1) (x2, k2, d2) = 
	  Name.eq x1 x2 && k1 = k2 && d1 = d2
	let hash  = Hashtbl.hash
      end)
    in
    let table = RenameHash.create 17 in
    let _ =  Tools.add_at_reset (fun () -> RenameHash.clear table) in 
      fun n i d -> 
	let k = index i in
	  try
	    RenameHash.find table (n, k, d)
	  with
	      Not_found ->
		let x = Var(Var.mk_rename n k d) in
		  RenameHash.add table (n, k, d) x; x


  (** Constructing hashconsed slack ariables. *)
  let mk_slack =
    let module SlackHash = Hashtbl.Make(
      struct
	type t = int * Var.slack
	let equal (k1, m1) (k2, m2) = 
	  k1 = k2 && m1 = m2
	let hash  = Hashtbl.hash
      end)
    in
    let table = SlackHash.create 17 in
    let _ =  Tools.add_at_reset (fun () -> SlackHash.clear table) in 
      fun i sl -> 
	let k = index i in 
	  try
	    SlackHash.find table (k, sl)
	  with
	      Not_found ->
		let x = Var(Var.mk_slack k sl) in
		  SlackHash.add table (k, sl) x; x


  (** Construct hashconsed fresh variables local to some theory. *)
  let mk_fresh =
    let module FreshHash = Hashtbl.Make(
      struct
	type t = Th.t * int * Dom.t option
	let equal (th1, k1, d1) (th2, k2, d2) = 
	  k1 = k2 && th1 = th2 && d1 = d2
	let hash  = Hashtbl.hash
      end)
    in
    let table = FreshHash.create 17 in
    let _ =  Tools.add_at_reset (fun () -> FreshHash.clear table) in 
      fun th i d -> 
	let k = index i in 
	  try
	    FreshHash.find table (th, k, d)
	  with
	      Not_found ->
		let x = Var(Var.mk_fresh th k d) in
		  FreshHash.add table (th, k, d) x; x


  (** {7 Recognizers} *)

  let is_zero_slack = function Var(x) -> Var.is_zero_slack x | _ -> false
  let is_nonneg_slack = function Var(x) -> Var.is_nonneg_slack x | _ -> false
  let is_slack x = is_zero_slack x || is_nonneg_slack x
  let is_external = function Var(x) -> Var.is_var x | _ -> false
  let is_rename = function Var(x) -> Var.is_rename x | _ -> false
  let is_fresh i = function Var(x) -> Var.is_fresh i x | _ -> false
  let is_internal = function Var(x) -> Var.is_internal x | _ -> false
  let is_free = function Var(x) -> Var.is_free x | _ -> false


  (** {7 Accessors} *)

  let dom_of = function
    | Var(x) -> Var.dom_of x
    | _ -> raise Not_found

       
  let name_of = function 
    | Var(x) -> Var.name_of x 
    | _ -> raise Not_found

  let is_dom d a =
    try Dom.eq (dom_of a) d with Not_found -> false

  let is_int = function
    | Var(x) -> Var.is_int x
    | _ -> false

  let is_real = function
    | Var(x) -> Var.is_real x
    | _ -> false

       (** Create a term variable from a variable. *)
  let of_var = function
    | Var.External(n, d) -> mk_var n d
    | Var.Rename(n, i, d) ->  mk_rename n (Some(i)) d
    | Var.Slack(i, m) -> mk_slack (Some(i)) m
    | Var.Fresh(th, i, d) -> mk_fresh th (Some(i)) d
    | Var.Bound(n) -> mk_free n


	(** Set of variables. *)
  module Set = struct
    
    let empty = Var.Set.empty
    let is_empty = Var.Set.is_empty
		     
    (** [restrict f] restrict a function of terms to variables. *)
    let restrict f = function
      | Var(x) -> f x
      | App _ -> failwith "Fatal error: variable function applied to application."
	  
    let inject f x = f (of_var x)
		       
    let mem = restrict Var.Set.mem
    let add = restrict Var.Set.add
    let singleton = restrict Var.Set.singleton
    let remove = restrict Var.Set.remove

    let union = Var.Set.union
    let inter = Var.Set.inter
    let diff = Var.Set.diff
    let compare = Var.Set.compare
    let equal = Var.Set.equal
    let subset = Var.Set.subset

    let iter f = Var.Set.iter (inject f)
    let for_all p = Var.Set.for_all (inject p)
    let exists p = Var.Set.exists (inject p)
    let filter p = Var.Set.filter (inject p)
    let partition p = Var.Set.partition (inject p)
    let cardinal = Var.Set.cardinal
    let map f = Var.Set.fold (fun x -> add (inject f x))   
    let elements s = Var.Set.fold (fun x acc ->  of_var x :: acc) s []

    let pp fmt s =
      Pretty.set pp fmt (elements s)

  end 


end 

module App = struct

  let mk_const =
    let table = Sym.Hash.create 17 in
    let _ =  Tools.add_at_reset (fun () -> Sym.Hash.clear table) in 
      fun f ->
	try
	  Sym.Hash.find table f
	with
	    Not_found ->
	      let c = App(f, []) in
		Sym.Hash.add table f c; c

  let mk_app f l = 
    match l with
      | [] -> mk_const f
      | _ -> App(f, l)

  let destruct a =
    match a with App(f,l) -> (f,l) | _ -> raise Not_found

  let sym_of a = 
    match a with App(f,_) -> f | _ -> raise Not_found

  let args_of a = 
    match a with App(_,l) -> l | _ -> raise Not_found

  let theory_of = function
    | App(f, _) -> Th.of_sym f 
    | _ -> raise Not_found

end


(** Syntactic term equality *)
let rec eq a b = 
  a == b  ||   (* variables and constants are hashconsed *)
  (match a, b with
     | App(f, ((_ :: _) as l)), App(g, ((_ :: _) as m)) -> 
	 Sym.eq f g && eql l m
     | _ -> false)

and eql al bl =
  try List.for_all2 eq al bl with Invalid_argument _ -> false

let eq a b =
  Trace.func "foo11" "eq" (Pretty.pair pp pp) Pretty.bool
    (fun (a, b) -> eq a b) (a, b)


let cmp a b =
  Trace.func "foo11" "cmp" (Pretty.pair pp pp) Pretty.number
    (fun (a, b) -> cmp a b) (a, b)



(** Some recognizers. *)

let is_equal a b =
  if eq a b then Three.Yes else 
    match a, b with                
      | App(c, []), App(d, []) 
	  when Th.of_sym c = Th.of_sym d -> Three.No
      | _ -> Three.X

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
    | App(_, l) -> List.fold_right (fold f) l acc

let rec iter f a  =
  match a with
    | Var _ -> f a
    | App(_, l) -> List.iter (iter f) (App.args_of a)

let rec for_all p a  =
  p a && 
  match a with
    | Var _ -> true
    | App(_, l) -> List.for_all (for_all p) l


let rec subterm a b  =
  eq a b ||
  match b with
    | Var _ -> false
    | App(_, l) -> List.exists (subterm a) (App.args_of b)

let subterm a b =
  Trace.func "foo" "subterm" (Pretty.pair pp pp) Pretty.bool
    (fun (a, b) -> subterm a b)
    (a, b)

let occurs x b = subterm x b

let is_pure i =
  let rec loop = function
    | Var _ -> true
    | App(f, al) -> Th.of_sym f = i && List.for_all loop al
  in
    loop




(** {6 Sets and maps of terms.} *)


module Set2 = Set.Make(
  struct
    type t = trm * trm
    let compare (a1, a2) (b1, b2) =
      let cmp1 = cmp a1 b1 in
	if cmp1 <> 0 then cmp1 else cmp a2 b2
  end)

module Set = Set.Make(
  struct
    type t = trm
    let compare = cmp
  end)

module Map = Map.Make(
  struct
    type t = trm
    let compare = cmp
  end)


(** Set of variables. *)
let rec vars_of a = 
  match a with
    | Var _ -> 
	Set.singleton a
    | App(_, al) ->
	List.fold_left 
	  (fun acc b ->
	     Set.union (vars_of b) acc)
	  Set.empty
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


(** {6 Encoding of disequalities} *)

module Diseq = struct
   type t = trm * trm
   let lhs (a, _) = a
   let rhs (_, b) = b
   let pp fmt (a, b) = Pretty.infix pp "<>" pp fmt (a, b)
   let make = orient
   let destruct e = e
   let eq (a1, b1) (a2, b2) = eq a1 a2 && eq b1 b2
   let compare (a1, b1) (a2, b2) =
     let res = cmp a1 a2 in if res = 0 then cmp b1 b2 else res
end




(** {6 Encoding of arithmetic constraints} *)

module Nonneg = struct
   type t = trm
   let pp fmt a = pp fmt a; Format.fprintf fmt " >= 0"
   let make = function
     | App(Sym.Arith(Sym.Multq(q)), [x]) when Q.is_pos q -> x
     | a -> a
   let term_of a = a
   let compare  = cmp
end

module Pos = struct
   type t = trm
   let pp fmt a = pp fmt a; Format.fprintf fmt " > 0"
   let make  = function
     | App(Sym.Arith(Sym.Multq(q)), [x]) when Q.is_pos q -> x
     | a -> a
   let term_of a = a
   let compare  = cmp
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


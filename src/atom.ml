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

open Mpa

type atom =
  | True
  | Equal of Term.t * Term.t
  | Diseq of Term.t * Term.t
  | Nonneg of Term.t
  | Pos of Term.t
  | False

and t = atom * int

let atom_of (a, _) = a
let index_of (_, i) = i


(** Bindings [a |-> (a, i)] with [a] an atom and [i] a {i unique} index.
  That is, for  [a |-> (a, i)] and [b |-> (b, j)],  [i = j] iff [equal a b]. *)
module AtomTbl = Hashtbl.Make(
  struct 
    type t = atom
    let equal a b = 
      match a, b with
	| True, True -> true
	| False, False -> true
	| Equal(a1, b1), Equal(a2, b2) -> Term.eq a1 a2 && Term.eq b1 b2
	| Diseq(a1, b1), Diseq(a2, b2) -> Term.eq a1 a2 && Term.eq b1 b2
	| Nonneg(a), Nonneg(b) -> Term.eq a b
	| Pos(a), Pos(b) -> Term.eq a b
	| _ -> false
    let hash = function
      | True -> 0
      | Equal(a, b) -> (17 + Term.hash a + Term.hash b) land 0x3FFFFFFF
      | Diseq(a, b) -> (631 + Term.hash a + Term.hash b) land 0x3FFFFFFF
      | Nonneg(a) -> Term.hash a
      | Pos(a) -> Term.hash a
      | False -> 1
  end)
  
let heap = AtomTbl.create 117
let index = Array.make 20000 (Obj.magic 0)
let max = ref (Obj.magic 0) 
  
let genidx () =
  let idx  = !max in
    assert(!max < max_int);
    incr max;
    idx

let mk_true = (True, 0)
let mk_false = (False, 1)

let of_atom a =
  try
    AtomTbl.find heap a
  with
      Not_found ->
	let i = genidx() in
	let ai = (a, i) in
	  AtomTbl.add heap a ai;   
	  Array.set index i ai;
	  ai

let of_index i = Array.get index i


(** [0] and [1] are reserved for [True], [False]. Then
  initialize and register initialization for resetting. *)
let initialize () = 
  max := 2;
  AtomTbl.add heap True mk_true;
  AtomTbl.add heap False mk_false


let _ = initialize ()
let _ = Tools.add_at_reset initialize

let equal (_, i) (_, j) = (i == j)

let compare (_, i) (_, j) = 
  if i < j then -1 else if i = j then 0 else 1
  
let is_true = function True, _ -> true | _ -> false

let is_false = function False, _ -> true | _ -> false


(* Construct an equality atom. *)
let mk_equal (a, b) =
  if Term.eq a b then mk_true else 
    let (a', b') = Term.orient(Nonlin.crossmultiply (a, b)) in
      of_atom(Equal(a', b'))

let mk_diseq (a, b) =
  if Term.eq a b then mk_false else
    if Term.eq a Boolean.mk_true then
      mk_equal (b, Boolean.mk_false)
    else if Term.eq a Boolean.mk_false then
      mk_equal (b, Boolean.mk_true)
    else if Term.eq b Boolean.mk_true then
      mk_equal (a, Boolean.mk_false)
    else if Term.eq b Boolean.mk_false then
      mk_equal (a, Boolean.mk_true)
    else
      let (a', b') = Term.orient(Nonlin.crossmultiply (a, b)) in
	of_atom (Diseq(a', b'))

let mk_nonneg a =
  try
    let q = Arith.d_num a in
      if Mpa.Q.is_nonneg q then mk_true else mk_false
  with
      Not_found -> 
	(try
	   let (q, x) = Arith.d_multq a in
	     if Mpa.Q.is_nonneg q then 
	       of_atom(Nonneg(x))
	     else 
	       of_atom(Nonneg(a))
	 with
	     Not_found -> of_atom(Nonneg(a)))

let mk_pos a =
  try
    let q = Arith.d_num a in
      if Mpa.Q.is_pos q then mk_true else mk_false
  with
      Not_found ->
	if Arith.is_int a then       (* [a > 0] iff [a - 1 >= 0] for [a] an integer. *)
	  mk_nonneg (Arith.mk_decr a)
	else 
	  of_atom(Pos(a))

let mk_neg a = mk_pos (Arith.mk_neg a)
let mk_nonpos a = mk_nonneg (Arith.mk_neg a)

let mk_ge (a, b) = mk_nonneg (Arith.mk_sub a b)  (* [a >= b] iff [a - b >= 0] *)
let mk_gt (a, b) = mk_pos (Arith.mk_sub a b)     (* [a > b] iff [a - b > 0] *)
let mk_le (a, b) = mk_nonneg (Arith.mk_sub b a)  (* [a <= b] iff [b - a >= 0] *)
let mk_lt (a, b) = mk_pos (Arith.mk_sub b a)     (* [a < b] iff [b - a > 0] *)


let apply rho = failwith "atom.apply: to do" 

(** {6 Pretty-printing} *)

let pp fmt (a, _) =
  match a with
    | True -> Pretty.string fmt "True"
    | False -> Pretty.string fmt "False"
    | Equal(a, b) -> Pretty.infix Term.pp "=" Term.pp fmt (a, b)
    | Diseq(a, b) -> Pretty.infix Term.pp "<>" Term.pp fmt (a, b)
    | Nonneg(a) -> Term.pp fmt a; Pretty.string fmt " >= 0"
    | Pos(a) ->  Term.pp fmt a; Pretty.string fmt " > 0"

let to_string a = Pretty.to_string pp a
      

(** {6 Negations of atoms} *)

let is_negatable _ = true

let negate (a, _) =
  match a with
    | True -> mk_false
    | False -> mk_true
    | Equal(a, b) -> mk_diseq (a, b)
    | Diseq(a, b) -> mk_equal (a, b)
    | Nonneg(a) -> mk_pos (Arith.mk_neg a)  (* [not(a >= 0)] iff [-a > 0] *)
    | Pos(a) -> mk_nonneg (Arith.mk_neg a)  (* [not(a > 0)] iff [-a >= 0] *)

let _ = Callback.register "atom_negate" negate


(** {6 Miscellaneous} *)

let vars_of (a, _) =
  match a with 
    | True -> Term.Var.Set.empty
    | False -> Term.Var.Set.empty
    | Equal(a, b) -> Term.Var.Set.union (Term.vars_of a) (Term.vars_of b)
    | Diseq(a, b) -> Term.Var.Set.union (Term.vars_of a) (Term.vars_of b)
    | Nonneg(a) -> Term.vars_of a
    | Pos(a) -> Term.vars_of a

let list_of_vars a = 
  Term.Var.Set.elements (vars_of a)


let occurs ((x, a) as p) =
  let rec term_occurs = function
    | Term.Var _ as y -> Term.eq x y
    | Term.App(_, sl, _) -> List.exists term_occurs sl
  in
    match a with
      | True -> false
      | False -> false
      | Equal(a, b) -> term_occurs a || term_occurs b
      | Diseq(a, b) -> term_occurs a || term_occurs b
      | Nonneg(a) -> term_occurs a
      | Pos(a) -> term_occurs a

let is_connected (a, _) (b, _) =
  let rec term_is_connected = function
    | Term.Var _ as x -> occurs (x, b)
    | Term.App(_, sl, _) -> List.exists term_is_connected sl
  in
    match a with
      | True -> false
      | False -> false
      | Equal(a, b) -> term_is_connected a || term_is_connected b
      | Diseq(a, b) -> term_is_connected a || term_is_connected b
      | Nonneg(a) -> term_is_connected a
      | Pos(a) -> term_is_connected a


module Set = struct
  type t = Ptset.t
  type elt = atom * int
  let empty = Ptset.empty
  let is_empty = Ptset.is_empty
  let mem (_, i) = Ptset.mem i
  let add (_, i) = Ptset.add i
  let singleton (_, i) = Ptset.singleton i
  let remove (_, i) = Ptset.remove i
  let union = Ptset.union
  let subset = Ptset.subset
  let inter = Ptset.inter
  let diff = Ptset.diff
  let equal = Ptset.equal
  let compare = Ptset.compare
  let elements s = Ptset.fold (fun i acc -> of_index i :: acc) s []
  let choose s = of_index (Ptset.choose s)
  let cardinal = Ptset.cardinal
  let inj f i =  f (of_index i)
  let iter f = Ptset.iter (inj f)
  let fold f = Ptset.fold (inj f)
  let for_all p = Ptset.for_all (inj p)
  let exists p = Ptset.exists (inj p)
  let filter p = Ptset.filter (inj p)
  let partition p = Ptset.partition (inj p)
  let max_elt s = of_index (Ptset.max_elt s)
  let min_elt s = of_index (Ptset.max_elt s)
end

module Map = struct
  type (+'a) t = 'a Ptmap.t
  type key = atom * int
  let empty = Ptmap.empty
  let add (_, i) = Ptmap.add i
  let find (_, i) = Ptmap.find i
  let remove (_, i) = Ptmap.remove i
  let mem (_, i) = Ptmap.mem i
  let inj f i =  f (of_index i)
  let iter f = Ptmap.iter (inj f)
  let map f =
    let f' i = failwith "Idxatom.Map.map: to do"
    in
      Ptmap.map f'
  let mapi f = Ptmap.mapi (inj f)
  let fold f = Ptmap.fold (inj f)
end

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


(** Equality, disequality, and inequality atoms. *)

let to_option f a =
  try Some(f a) with Not_found -> None

module Equal = struct
   type t = Term.t * Term.t
   let lhs (a, _) = a
   let rhs (_, b) = b
   let pp = Pretty.infix Term.pp "=" Term.pp
   let make (a, b) =  Term.orient (a, b)
   let make_inorder (a, b) =  (a, b)
   let destruct e = e
   let both_sides p (a, b) = p a && p b
   let is_var = both_sides Term.is_var
   let is_pure i = both_sides (Term.is_pure i)
   let eq (a1, b1) (a2, b2) = a1 == a2 && b1 == b2
   let compare (a1, b1) (a2, b2) = 
     let cmp = Term.compare a1 a2 in
       if cmp <> 0 then cmp else Term.compare b1 b2
   let vars_of (a, b) = Term.Var.Set.union (Term.vars_of a) (Term.vars_of b)
   let holds (a, b) = if Term.eq a b then Three.Yes else Three.X

   let map2 (id, h) (f, g) ((a, b) as e) =
   let (a', alpha) = f a in               
   let (b', beta) = g b in
     if a == a' && b == b' then (e, id e) else 
       let e' = make (a', b') in
	 (e', h e' alpha beta)

   let map (id, h) f = map2 (id, h) (f, f)

   let status (a, b) =
     match Term.status a, Term.status b with
       | Term.Variable, Term.Variable -> Term.Variable
       | (Term.Mixed _ as s1) , _ -> s1
       | _, (Term.Mixed _ as s2) -> s2
       | (Term.Pure(i) as s1), Term.Variable -> s1
       | Term.Variable, (Term.Pure(j) as s2) -> s2
       | Term.Pure(i), (Term.Pure(j) as s2) -> 
	   if i = j then s2 else Term.Mixed(i, a)

end


module Diseq = struct
   type t = Term.t * Term.t
   let lhs (a, _) = a
   let rhs (_, b) = b
   let pp = Pretty.infix Term.pp "<>" Term.pp
   let destruct d = d
   let both_sides p (a, b) = p a && p b
   let eq (a1, b1) (a2, b2) = Term.eq a1 a2 && Term.eq b1 b2
   let compare (a1, b1) (a2, b2) = 
     let cmp = Term.compare a1 a2 in
       if cmp <> 0 then cmp else Term.compare b1 b2
   let is_var (a, b) = Term.is_var a && Term.is_var b
   let vars_of (a, b) = Term.Var.Set.union (Term.vars_of a) (Term.vars_of b)
   let make (a, b) = Term.orient (a, b)
   let map2 (id, h) (f, g) ((a, b) as d) =
   let (a', alpha) = f a in               
   let (b', beta) = g b in
     if a == a' && b == b' then d, id d else 
       let d' = make (a', b') in (d', h d' alpha beta)
   let map h f = map2 h (f, f)
   let holds (a, b) =
     if Term.eq a b then Three.No else Three.X

   let status (a, b) =
     match Term.status a, Term.status b with
       | (Term.Mixed _ as s1), _ -> s1
       | _, (Term.Mixed _ as s2) -> s2
       | Term.Variable, Term.Variable -> Term.Variable
       | Term.Pure(i), (Term.Pure(j) as s2) -> 
	   if i = j then s2 else Term.Mixed(i, a)
       | (Term.Pure(i) as s1), Term.Variable -> s1
       | Term.Variable, (Term.Pure(j) as s2) -> s2

end

module Nonneg = struct
  type t = Term.t
  let pp fmt a = Pretty.post Term.pp fmt (a, ">=0")
  let make a = a

  let destruct c = c

  let map (id, h) f a =
    let (a', rho') = f a in            
      if a == a' then (a, id a) else 
	let a'' = make a' in
	  (a'', h a'' rho')

  let status = Term.status
end

module Pos = struct

  type t = Term.t
  let pp fmt a = Pretty.post Term.pp fmt (a, ">0")

  let destruct c = c

  let make a = a

  let map (id, h) f a =
    let (a', rho') = f a in            
      if a == a' then (a, id a) else 
	let a'' = make a' in
	  (a'', h a'' rho')

  let status = Term.status
end


type atom =
  | TT
  | Equal of Term.t * Term.t
  | Diseq of Term.t * Term.t
  | Nonneg of Term.t
  | Pos of Term.t
  | FF

and t = atom * int

let atom_of (a, _) = a
let index_of (_, i) = i


(** {6 Pretty-printing} *)

let is_false = function FF, _ -> true | _ -> false

let pp fmt (atm, i) =
  match atm with 
    | TT -> Pretty.string fmt "tt"
    | FF -> Pretty.string fmt "ff"
    | Equal(a, b) -> Equal.pp fmt (a, b)
    | Diseq(a, b) -> Diseq.pp fmt (a, b)
    | Nonneg(a) -> Nonneg.pp fmt a
    | Pos(a) ->  Pos.pp fmt a


let to_string a =
  Pretty.to_string pp a


(** Bindings [a |-> (a, i)] with [a] an atom and [i] a {i unique} index.
  That is, for  [a |-> (a, i)] and [b |-> (b, j)],  [i = j] iff [equal a b]. *)
module AtomTbl = Hashtbl.Make(
  struct 
    type t = atom
    let equal a b = 
      match a, b with
	| TT, TT -> true
	| FF, FF -> true
	| Equal(a1, b1), Equal(a2, b2) -> Term.eq a1 a2 && Term.eq b1 b2
	| Diseq(a1, b1), Diseq(a2, b2) -> Term.eq a1 a2 && Term.eq b1 b2
	| Nonneg(a), Nonneg(b) -> Term.eq a b
	| Pos(a), Pos(b) -> Term.eq a b
	| _ -> false
    let hash = function
      | TT -> 0
      | Equal(a, b) -> (17 + Term.hash a + Term.hash b) land 0x3FFFFFFF
      | Diseq(a, b) -> (631 + Term.hash a + Term.hash b) land 0x3FFFFFFF
      | Nonneg(a) -> Term.hash a
      | Pos(a) -> Term.hash a
      | FF -> 1
  end)
  
let heap = AtomTbl.create 117
let index = Dynarray.make 2000
let max = ref 0

let mk_true = (TT, 0)
let mk_false = (FF, 1)

let of_atom a =
  try
    AtomTbl.find heap a
  with
      Not_found ->
	let i = Dynarray.length index in
	let ai = (a, i) in
	  AtomTbl.add heap a ai;
	  Dynarray.add index ai;
	  assert(Dynarray.get index i == ai);
	  ai

let of_index i = Dynarray.get index i


(** [0] and [1] are reserved for [TT], [FF]. Then
  initialize and register initialization for resetting. *)
let _ = 
  max := 2;
  AtomTbl.add heap TT mk_true;
  Dynarray.add index mk_true;
  AtomTbl.add heap FF mk_false;
  Dynarray.add index mk_false


let equal (_, i) (_, j) = (i == j)

let compare (_, i) (_, j) = 
  if i < j then -1 else if i == j then 0 else 1
  
let is_true = function TT, _ -> true | _ -> false

let is_false = function FF, _ -> true | _ -> false

let of_equal (a, b) = of_atom(Equal(a, b))
let of_diseq (a, b) = of_atom(Diseq(a, b))
let of_nonneg a = of_atom(Nonneg(a))
let of_pos a = of_atom(Pos(a))


(* Construct an equality atom. *)
let mk_equal (a, b) =
  match Equal.holds (a, b) with
    | Three.Yes -> mk_true
    | Three.No -> mk_false
    | Three.X -> of_equal(Equal.make(a, b))


(** Boolean constants for normalizing some diequalities. *)
module Boolean = struct

  let tt () = Sym.Bv.mk_const(Bitv.create 1 true)
  let ff () =  Sym.Bv.mk_const(Bitv.create 1 false)

  let mk_true () = Term.App.mk_const (tt())
  let mk_false () = Term.App.mk_const (ff())

  let is_true = function
    | Term.App(f, [], _) when Sym.eq f (tt()) -> true
    | _ -> false

  let is_false = function
    | Term.App(f, [], _) when Sym.eq f (ff()) -> true
    | _ -> false

end 

let mk_diseq (a, b) =
  if Term.eq a b then mk_false else
    if Boolean.is_true a then
      mk_equal (b, (Boolean.mk_false()))
    else if Boolean.is_false a then
      mk_equal (b, (Boolean.mk_true()))
    else if Boolean.is_true b then
      mk_equal (a, (Boolean.mk_false()))
    else if Boolean.is_false b then
      mk_equal (a, (Boolean.mk_true()))
    else
      of_diseq(Diseq.make (a, b))


let mk_nonneg a =
  let b = Nonneg.make a in
    of_nonneg b

let mk_pos a =
  let b = Pos.make a in
    of_pos b

let is_pure i (a, _) =
  match a with 
    | TT -> true
    | Equal(a, b) -> Term.is_pure i a && Term.is_pure i b 
    | Diseq(a, b) -> Term.is_pure i a && Term.is_pure i b 
    | Nonneg(a) -> Term.is_pure i a
    | Pos(a) -> Term.is_pure i a
    | FF -> true



(** {6 Negations of atoms} *)

let is_negatable _ = true

let negate mk_neg (a, _) =
  match a with
    | TT -> mk_false
    | FF -> mk_true
    | Equal(a, b) -> mk_diseq (a, b)
    | Diseq(a, b) -> mk_equal (a, b)
    | Nonneg(a) -> mk_pos (mk_neg a)  (* [not(a >= 0)] iff [-a > 0] *)
    | Pos(a) -> mk_nonneg (mk_neg a)  (* [not(a > 0)] iff [-a >= 0] *)



(** {6 Miscellaneous} *)

let vars_of (a, _) =
  match a with 
    | TT -> Term.Var.Set.empty
    | FF -> Term.Var.Set.empty
    | Equal(a, b) -> Equal.vars_of (a, b)
    | Diseq(a, b) -> Diseq.vars_of (a, b)
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
      | TT -> false
      | FF -> false
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
      | TT -> false
      | FF -> false
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

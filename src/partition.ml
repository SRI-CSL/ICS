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

(** Datatype for storing variable equalities and disequalities. *)

(** Sets of equalities and disequalities over variables.
  See also modules {!V.t} and {!D.t}. *)
type t = {
  mutable v : V.t;                   (* Variable equalities. *)
  mutable d : D.t;                   (* Variables disequalities. *)
  mutable equal : Fact.Equal.t list; (* Fresh equalities and disequalities *)
  mutable diseq : Fact.Diseq.Set.t;           
}

let v_of p = p.v
let d_of p = p.d


(** Empty partition. *)
let empty = {
  v = V.empty; 
  d = D.empty; 
  equal = [];
  diseq = Fact.Diseq.Set.empty
}

(** Shallow copying. *)
let copy s = {
  v = V.copy s.v; 
  d = s.d; 
  equal = s.equal; 
  diseq = s.diseq
}

let is_empty p =
  V.is_empty p.v &&
  D.is_empty p.d

(** Pretty-printing *)
let pp fmt p = 
  Pretty.string fmt "[";
  V.pp fmt p.v; 
  D.pp fmt p.d;
  Pretty.string fmt "]"

(** Test if states are unchanged. *)
let eq p q = 
  V.eq p.v q.v && 
  D.eq p.d q.d

(** Choose [x] satisfying [apply] in equivalence class for [y].  *)
let choose p apply y = 
  if Term.is_var y then
    let rhs x = try Some(apply x) with Not_found -> None in
      V.choose p.v rhs y
  else
    raise Not_found

(** Iterate over the [x]'s satisfying [f] in the equivalence 
  class of [y]. *)
let iter_if p f y =
  let f' x = try f x with Not_found -> () in
    V.iter p.v f' y

let fold p f z acc = 
  let f' x (y, rho)= 
    f (Fact.Equal.make x y rho)
  in
  let (z, _) = V.find p.v z in
    acc  (* to do *)
  
(** Canonical representative modulo [p]. *)
let find p = V.find p.v

let is_canonical p = V.is_canonical p.v

    

(** All disequalities of some variable [x]. *)
let diseqs p x =
  let (y, rho) = find p x in                   (* [rho |- x = y] *)
  let ds = D.diseqs p.d y in
    if Term.eq x y then ds else
      D.Set.fold 
	(fun (z, tau) ->                       (* [tau |- y <> z] *)
	   let sigma = Jst.dep2 tau rho in
	     D.Set.add (z, sigma))
	ds D.Set.empty


(** Domain constraints for some variable [x]. *)
let cnstrnt p x =
  let (y, rho) = find p x in
    if Term.eq x y then 
      V.cnstrnt p.v x
    else 
      let (c, tau) = V.cnstrnt p.v x in
	(c, Jst.dep2 rho tau)


(** {6 Predicates} *)

let is_canonical p x =
  let (x', _) = find p x in
    Term.eq x' x

let is_int p x =
  try
    (match cnstrnt p x with
       | Var.Cnstrnt.Real(Dom.Int), rho -> Some(rho)
       | _ -> None)
  with
      Not_found -> None

(** Apply the equality test on variables only. *)
let is_equal p x y =
  if Term.is_var x && Term.is_var y then
    V.is_equal p.v x y
  else 
    None


(** Apply the equality test on canonical variables. *)
let is_diseq p = 
  Jst.Pred2.apply 
    (find p) 
    (D.is_diseq p.d)


(** Test for equality or disequality of canonical variables. *)
let is_equal_or_diseq p x y =
  if Term.is_var x && Term.is_var y then
    Jst.Rel2.apply
      (find p)
      (Jst.Rel2.yes_or_no
	 (V.is_equal p.v)            (* positive test *)
	 (D.is_diseq p.d))           (* negative test *)
      x y
  else 
    Jst.Three.X


(** Merge a variable equality. Makes sure that domain
 constraints on variables are not being lost. *)
let rec merge p e =   
  assert(Fact.Equal.is_var e);
  let e = Fact.Equal.map (find p) e in        (* get canonical forms *)
    assert(Fact.Equal.both_sides (is_canonical p) e);  
    merge1 p e
	  
and merge1 p e =
  let x, y, _ = e in
    if Term.eq x y then () else 
      let (d', ds') = D.merge e p.d in 
	p.d <- d'; 
	V.merge e p.v;
	p.equal <- e :: p.equal;
	p.diseq <- Fact.Diseq.Set.union ds' p.diseq 

(** Create a fresh variable that is smaller than [x] and [y]
  according to variable ordering {!Var.cmp} and which incorporates
  the intersection of the domains of [x] and [y]. *)
and create_fresh_var d x y =
  match Term.Var.is_slack x, Term.Var.is_slack y with
    | false, false -> 
	Term.Var.mk_rename (Name.of_string "w") None (Var.Cnstrnt.Real(d))
    | _ ->
	if Term.Var.is_zero_slack x || Term.Var.is_zero_slack y then
	  Term.Var.mk_slack None Var.Zero
	else 
	  Term.Var.mk_slack None (Var.Nonneg(d))


(** Add a disequality of the form [x <> y]. *)
let dismerge p d =  
  assert(Fact.Diseq.is_var d);
  let d' = Fact.Diseq.map (find p) d in
  let (x', y', rho') = d' in
    if Term.eq x' y' then
      raise(Jst.Inconsistent(rho'))
    else 
      let (d', ds') =  D.add d' p.d in
	p.d <- d'; 
	p.diseq <- Fact.Diseq.Set.union ds' p.diseq


(** Garbage collection of noncanonical variables satisfying [f]. Since variable
  disequalities and constraints are always in canonical form, only variable equalities
  need to be considered. *)
let gc f p = 
  V.gc f p.v
    
(** Choose a fresh variable equality or disequality. *)
let fresh_equal p =
  match p.equal with
    | [] -> 
	raise Not_found
    | e :: equal' ->
	p.equal <- equal'; e

let fresh_diseq p =
  let d = Fact.Diseq.Set.choose p.diseq in
    p.diseq <- Fact.Diseq.Set.remove d p.diseq; d


(** Difference. *)
let diff p q =
  let d' = D.diff p.d q.d
  and v' = V.diff p.v q.v in
    {p with v = v'; d = d'}   (* nondestructive *)
  


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


(** Equality, disequality, and inequality atoms. *)

type t =
  | TT
  | Equal of Term.t * Term.t
  | Diseq of Term.t * Term.t
  | Nonneg of Term.t
  | Pos of Term.t
  | Cnstrnt of Term.t * Cnstrnt.t
  | FF

let is_false = function FF -> true | _ -> false

let is_true = function TT -> true | _ -> false

type atom = t (* nickname *)

(** Bindings [a |-> (a, i)] with [a] an atom and [i] a {i unique} index.
  That is, for  [a |-> (a, i)] and [b |-> (b, j)],  [i = j] iff [equal a b]. *)
module Cache = Weak.Make(
  struct 
    type t = atom
    let equal a b =  
      match a, b with
	| Equal(s1, t1), Equal(s2, t2) -> Term.eq s1 s2 && Term.eq t1 t2
	| Diseq(s1, t1), Diseq(s2, t2) -> Term.eq s1 s2 && Term.eq t1 t2
	| Nonneg(s), Nonneg(t) -> Term.eq s t
        | Pos(s), Pos(t) -> Term.eq s t
	| Cnstrnt(s, c), Cnstrnt(t, d) -> Term.eq s t && c = d
	| TT, TT -> true
	| FF, FF -> true
	| _ -> false
    let hash = Hashtbl.hash_param 2 2
  end)
  
let universe = Cache.create 117

let mk_true = TT
let mk_false = FF

let hashcons a = 
  try 
    Cache.find universe a
  with 
      Not_found -> 
	Cache.add universe a;
	a
	
let mk_equal s t =
  match Term.is_equal s t with
    | Three.Yes -> mk_true
    | Three.No -> mk_false
    | Three.X -> 
	let s, t = Term.orient (s, t) in
	  hashcons (Equal(s, t))
	 
let mk_diseq s t =
  match Term.is_equal s t with
    | Three.Yes -> mk_false
    | Three.No -> mk_true
    | Three.X -> 
	let s, t = Term.orient (s, t) in
	  hashcons (Diseq(s, t))

let mk_nonneg t = 
  match Term.is_nonneg t with
    | Three.Yes -> mk_true
    | Three.No -> mk_false
    | Three.X -> hashcons(Nonneg(t))

let mk_pos t = 
  match Term.is_pos t with
    | Three.Yes -> mk_true
    | Three.No -> mk_false
    | Three.X -> hashcons(Pos(t))

let mk_cnstrnt t c =
  match Term.is_cnstrnt t c with
    | Three.Yes -> mk_true
    | Three.No -> mk_false
    | Three.X -> hashcons(Cnstrnt(t, c))

(** Because of hashconsing, equality is just identity. *)
let eq = (==) 

let compare a b =
  if a == b then 0 else Pervasives.compare a b

let iter f = function
  | TT -> ()
  | FF -> ()
  | Equal(s, t) -> f s; f t
  | Diseq(s, t) -> f s; f t
  | Nonneg(s) -> f s
  | Pos(s) -> f s
  | Cnstrnt(s, _) -> f s

let for_all f = function
  | TT -> true
  | FF -> true
  | Equal(s, t) -> f s && f t
  | Diseq(s, t) -> f s && f t
  | Nonneg(s) -> f s
  | Pos(s) -> f s
  | Cnstrnt(s, _) -> f s

let exists f = function
  | TT -> false
  | FF -> false
  | Equal(s, t) -> f s || f t
  | Diseq(s, t) -> f s || f t
  | Nonneg(s) -> f s
  | Pos(s) -> f s
  | Cnstrnt(s, _) -> f s



(** Pretty-printing *)
let pp fmt a =
  let term = Term.pp fmt
  and string = Format.fprintf fmt
  and cnstrnt = Cnstrnt.pp fmt in
    string "@[";
    (match a with
       | TT -> string "tt"
       | FF -> string "ff"
       | Equal(s, t) -> term s; string " = "; term t
       | Diseq(s, t) -> term s; string " <> "; term t
       | Nonneg(s) -> term s; string " >=0"
       | Pos(s) -> term s; string " >0"
       | Cnstrnt(s, c) -> term s; string " in "; cnstrnt c);
    string "@]@;"

let to_string a =
  pp Format.str_formatter a;
  Format.flush_str_formatter ()

let is_pure i = 
  for_all (Term.is_pure i)

let rec is_disjoint a1 a2 =
  match a1, a2 with
    | TT, FF -> true
    | FF, TT -> true
    | Equal(s1, t1), Diseq(s2, t2) ->
	(Term.eq s1 s2 && Term.eq t1 t2) ||
	(Term.eq s1 t2 && Term.eq t1 s2)
    | Diseq(s1, t1), Equal(s2, t2) ->
	(Term.eq s1 s2 && Term.eq t1 t2) ||
	(Term.eq s1 t2 && Term.eq t1 s2)
    | Cnstrnt(s1, c1), Cnstrnt(s2, c2) -> 
	Term.eq s1 s2 && Cnstrnt.disjoint c1 c2
    | _ -> 
	false

let map f = 
  let rec mapf a = 
    match a with
      | TT -> mk_true
      | FF -> mk_false
      | Equal(s, t) ->
	  let s' = f s and t' = f t in
	    if s == s' && t == t' then a else mk_equal s' t'
      | Diseq(s, t) ->
	  let s' = f s and t' = f t in
	    if s == s' && t == t' then a else mk_diseq s' t'
      | Nonneg(s) ->
	  let s' = f s in
	    if s == s' then a else mk_nonneg s'
      | Pos(s) ->
	  let s' = f s in
	    if s == s' then a else mk_pos s'
      | Cnstrnt(s, c) -> 
	  let s' = f s in
	    if s == s' then a else mk_cnstrnt s' c
  in
    mapf

let replace a b atm =
  map (Term.replace a b) atm

let eval mdl = 
  map (Term.Model.value mdl)

let validates mdl atm =
  match eval mdl atm with
    | TT -> true
    | _ -> false

(** {6 Negations of atoms} *)

let is_negatable = function
  | Cnstrnt _ -> false
  | _ -> true
	
let negate mk_neg = function
  | TT -> mk_false
  | FF -> mk_true
  | Equal(s, t) -> mk_diseq s t
  | Diseq(s, t) -> mk_equal s t
  | Nonneg(t) -> mk_pos (mk_neg t)
  | Pos(t) -> mk_nonneg (mk_neg t)
  | _ -> invalid_arg "Atom.Negate: constraints not negatable"


(** {6 Status} *)

let status atm =
  let binary a b =
    let s = Term.status a in
      match s with
      | Term.Mixed _ -> s
      | _ ->
	  let t = Term.status b in
	    match s, t with
	      | _, Term.Mixed _ -> t
	      | Term.Variable, Term.Variable -> Term.Variable
	      | Term.Variable, Term.Pure _ -> t
	      | Term.Pure _, Term.Variable -> s
	      | Term.Pure(i), Term.Pure(j) ->
		  if Theory.eq i j then s else Term.Mixed(i, a)
	      | _ -> invalid_arg "Atom.status: unreachable."
  in
  let rec of_atom = function
    | Equal(s, t) -> binary s t
    | Diseq(s, t) -> binary s t
    | Nonneg(s) -> Term.status s
    | Pos(s) -> Term.status s
    | Cnstrnt(s, _) -> Term.status s
    | TT | FF -> invalid_arg "Atom.status: invalid argument."
  in
    of_atom atm
	

(** {6 Miscellaneous} *)

let vars_of a =
  let xs = Term.Set.empty () in
  let addvars t = Term.Set.union (Term.vars_of t) xs in
    iter addvars a;
    xs
	  
let list_of_vars a = 
  Term.Set.to_list (vars_of a)

let occurs x a =
  let rec occx b =
    if Term.is_var b then 
      Term.eq x b 
    else
      Term.Args.exists occx (Term.args_of b)
  in
    exists occx a
   
let is_connected a1 a2 =  
  let rec term_is_connected b =
    if Term.is_var b then occurs b a2 else
      Term.Args.exists term_is_connected (Term.args_of b)
  in
    exists term_is_connected a1

module Set = Sets.Make(
  struct
    type t = atom
    let compare = compare
    let pp = pp
  end)

module Map = Maps.Make(
  struct
    type t = atom
    let compare = compare
    let pp = pp
  end)

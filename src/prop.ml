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

(** Propositional logic solver based on lazy theorem proving. *)

type t =
  | True
  | False
  | Var of Name.t
  | Atom of Atom.t
  | Disj of t * t * int
  | Conj of t * t * int
  | Iff of t * t * int
  | Ite of t * t * t * int
  | Neg of t * int

let is_true = function True -> true | _ -> false
let is_false = function False -> true | _ -> false
let is_var = function Var _ -> true | _ -> false
let is_atom = function Atom _ -> true | _ -> false
let is_disj = function Disj _ -> true | _ -> false
let is_conj = function Conj _ -> true | _ -> false
let is_iff = function Iff _ -> true | _ -> false
let is_ite = function Ite _ -> true | _ -> false
let is_neg = function Neg _ -> true | _ -> false

let d_var = function Var(x) -> x | _ -> invalid_arg "wrong propositional argument"
let d_atom = function Atom(a) -> a | _ -> invalid_arg "wrong propositional argument"
let d_disj = function Disj(p, q, _) -> (p, q) | _ -> invalid_arg "wrong propositional argument"
let d_conj = function Conj(p, q, _) -> (p, q) | _ -> invalid_arg "wrong propositional argument"
let d_iff = function Iff(p, q, _) -> (p, q) | _ -> invalid_arg "wrong propositional argument"
let d_ite = function Ite(p, q, r, _) -> (p, q, r) | _ -> invalid_arg "wrong propositional argument"
let d_neg = function Neg(p, _) -> p | _ -> invalid_arg "wrong propositional argument"


type prp = t

let get p = p

let rec pp fmt = function
  | True -> 
      Pretty.string fmt "tt"
  | False -> 
      Pretty.string fmt "ff"
  | Var(x) -> 
      Name.pp fmt x
  | Atom(a) -> 
      Atom.pp fmt a
  | Disj(p, q, _) -> 
      Pretty.infix pp " | " pp fmt (p, q)
  | Conj(p, q, _) -> 
      Pretty.infix pp " & " pp fmt (p, q)
  | Iff(p, q, _) -> 
      Pretty.infix pp " <=> " pp fmt (p, q)
  | Neg(p, _) -> 
      Pretty.string fmt "~("; pp fmt p; Pretty.string fmt ")"
  | Ite(p, q, r, _) ->
      Pretty.mixfix "if" pp "then" pp "else" pp "end" fmt (p, q, r)

let hash = function
  | True -> 0
  | False -> 1
  | Var(n) -> Name.hash n
  | Atom(a) -> Atom.index_of a
  | Disj(_, _, hsh) -> hsh
  | Iff(_, _, hsh) -> hsh
  | Neg(_, hsh) -> hsh
  | Ite(_, _, _, hsh) -> hsh
  | Conj(_, _, hsh) -> hsh


let rec equal p q =
  hash p == hash q &&
  (match p, q with
     | True, True ->
	 true
     | False, False -> 
	 true
     | Var(x), Var(y) -> 
	 Name.eq x y
     | Atom(a), Atom(b) -> 
	 Atom.equal a b
     | Disj(p1, q1, _), Disj(p2, q2, _) ->
	 equal p1 p2 && equal q1 q2 
     | Iff(p1, q1, _), Iff(p2, q2, _) ->
	 equal p1 p2 && equal q1 q2
     | Neg(p1, _), Neg(p2, _) -> 
	 equal p1 p2
     | Ite(p1, q1, r1, _), Ite(p2, q2, r2, _) ->
	 equal p1 p2 && equal q1 q2 && equal r1 r2
     | Conj(p1, q1, _), Conj(p2, q2, _) ->
	 equal p1 p2 && equal q1 q2 
     | _ -> 
	 false)


let is_true = function
  | True -> true
  | _ -> false
	
let mk_true = 
  True

let mk_false = 
  False

let mk_var = 
  let table = Name.Hash.create 23 in 
  let _ = Tools.add_at_reset (fun () -> Name.Hash.clear table) in
    fun n ->
      try
	Name.Hash.find table n
      with
	  Not_found -> 
	    let x = Var(n) in
	      Name.Hash.add table n x; x

let mk_poslit = 
  let module Table = Hashtbl.Make(
    struct
      type t = Atom.t
      let equal = Atom.equal
      let hash = Atom.index_of
    end)
  in
  let memo = Table.create 23 in
  let _ = Tools.add_at_reset (fun () -> Table.clear memo) in
    fun a -> 
      try
	Table.find memo a 
      with
	  Not_found -> 
	    let pl = match Atom.atom_of a with
	      | Atom.TT -> True
	      | Atom.FF -> False
	      | _ ->  Atom(a)
	    in
	      Table.add memo a pl; pl

let mk_neglit = 
  let module Table = Hashtbl.Make(
    struct
      type t = Atom.t
      let equal = Atom.equal
      let hash = Atom.index_of
    end)
  in
  let memo = Table.create 23 in
  let _ = Tools.add_at_reset (fun () -> Table.clear memo) in
    fun a -> 
      try
	Table.find memo a 
      with
	  Not_found ->
	    let nl = mk_poslit (Atom.negate Arith.mk_neg a) in
	      Table.add memo a nl; nl

let mk_neg =
  let module Table = Hashtbl.Make(
    struct
      type t = prp
      let equal = (==)
      let hash = hash
    end)
  in
  let memo = Table.create 23 in
  let _ = Tools.add_at_reset (fun () -> Table.clear memo) in
    fun p -> 
      try
	Table.find memo p 
      with
	  Not_found -> 
	    let np = match p with
	      | Neg(q, _) -> q
	      | Atom(a) -> 
		  (match Atom.atom_of a with
		     | Atom.TT ->
			 mk_false
		     | Atom.FF -> 
			 mk_true
		     | Atom.Diseq(s, t) -> (* [not(s <> t)] iff [s = t]. *)
			 mk_poslit (Atom.mk_equal (s, t))
		     | Atom.Pos(t) -> 
			 let b = Atom.mk_nonneg (Arith.mk_neg t) in 
			   mk_poslit b      (* [not(t > 0)] iff [-t >= 0]. *)
		     | Atom.Nonneg(t) -> 
			 let b = Atom.mk_pos (Arith.mk_neg t) in
			   mk_poslit b      (* [not(t >= 0)] iff [-t > 0]. *)
		     | Atom.Equal _ -> 
			 let hsh = (1234 + hash p) land 0x3FFFFFFF in
			   Neg(p, hsh))
	      | _ -> 
		  let hsh = (1234 + hash p) land 0x3FFFFFFF in
		    Neg(p, hsh)
	    in
	      Table.add memo p np; np

let mk_disj2 = 
  let simplified p1 p2 =
    match p1, p2 with
      | True, _ -> True
      | _, True -> True
      | False, _ -> p2
      | _, False -> p1
      | (Disj(p1, q1, hsh_p1q1) as pq1), (Disj(p2, q2, hsh_p2q2) as pq2) -> 
	  assert(not(is_disj p1));
	  assert(not(is_disj p2));
	  (match is_disj q1 , is_disj q2 with
	     | false, false -> 
		 let hsh = (hsh_p1q1 + hsh_p2q2) land 0x3FFFFFFF in
		   Disj(pq1, pq2, hsh)
	     | false, true -> 
		 let hsh_q1p2q2 = (hash q1 + hsh_p2q2) land 0x3FFFFFFF in
		 let hsh_p1q1p2p2 = (hash p1 + hsh_q1p2q2) land 0x3FFFFFFF in
		   Disj(p1, Disj(q1, pq2, hsh_q1p2q2), hsh_p1q1p2p2) 
	     | _ ->
		 let hsh_q1 = hash q1 and hsh_q2 = hash q2 in
		 let hsh_q1q2 = (hsh_q1 + hsh_q2) land 0x3FFFFFFF in
		 let hsh_p2q1q2 = (hash p2 + hsh_q1q2) land 0x3FFFFFFF in
		 let hsh_p1p2q1q2 = (hash p1 +  hsh_p2q1q2) land 0x3FFFFFFF in
		   Disj(p1, Disj(p2, Disj(q1, q2, hsh_q1q2), hsh_p2q1q2), hsh_p1p2q1q2))
      | Disj(_, _, hsh1), _ -> 
	  assert(not(is_disj p2));
	  let hsh = (hash p2 + hsh1) land 0x3FFFFFFF in
	    Disj(p2, p1, hsh)
      | _, Disj(_, _, hsh2) ->  
	  assert(not(is_disj p1));
	  let hsh = (hash p1 + hsh2) land 0x3FFFFFFF in
	    Disj(p1, p2, hsh)
      | _ ->
	  let hsh_p1 = hash p1 and hsh_p2 = hash p2 in
	  let hsh = (hsh_p1 + hsh_p2) land 0x3FFFFFFF in
	    if hsh_p1 <= hsh_p2 then
	      Disj(p1, p2, hsh)
	    else 
	      Disj(p2, p1, hsh)
  in
  let module Table = Hashtbl.Make(
    struct
      type t = prp * prp
      let equal (p1, q1) (p2, q2) =
	p1 == p2 && q1 == q2
      let hash (p, q) =
	(hash p + hash q) land 0x3FFFFFFF
    end)
  in
  let memo = Table.create 23 in
  let _ = Tools.add_at_reset (fun () -> Table.clear memo) in
    fun p q -> 
      if p == q then p else 
	let ((p, q) as pq) = if hash p <= hash q then (p, q) else (q, p) in
	  try
	    Table.find memo pq
	  with
	      Not_found -> 
		let disj = simplified p q in
		(* let disj = Disj(p, q, (hash p + hash q) land 0x3FFFFFFF) in *)
		  Table.add memo pq disj; disj

let mk_conj2 = 
  let simplified p1 p2 =
    match p1, p2 with
      | True, _ -> p2
      | _, True -> p1
      | False, _ -> False
      | _, False -> False
      | Conj(p1, q1, _), Conj(p2, q2, _) -> 
	  assert(not(is_conj  p1));
	  assert(not(is_conj p2));
	  let hsh_q1 = hash q1 and hsh_q2 = hash q2 in
	  let hsh_q1q2 = (hsh_q1 + hsh_q2) land 0x3FFFFFFF in
	  let hsh_p2q1q2 = (hash p2 + hsh_q1q2) land 0x3FFFFFFF in
	  let hsh_p1p2q1q2 = (hash p1 +  hsh_p2q1q2) land 0x3FFFFFFF in
	    Conj(p1, Conj(p2, Conj(q1, q2, hsh_q1q2), hsh_p2q1q2), hsh_p1p2q1q2)
      | Conj(_, _, hsh1), _ -> 
	  assert(not(is_conj p2));
	  let hsh = (hash p2 + hsh1) land 0x3FFFFFFF in
	    Conj(p2, p1, hsh)
      | _, Conj(_, _, hsh2) ->  
	  assert(not(is_conj p1));
	  let hsh = (hash p1 + hsh2) land 0x3FFFFFFF in
	    Conj(p1, p2, hsh)
      | _ ->
	  let hsh_p1 = hash p1 and hsh_p2 = hash p2 in
	  let hsh = (hsh_p1 + hsh_p2) land 0x3FFFFFFF in
	    if hsh_p1 <= hsh_p2 then
	      Conj(p1, p2, hsh)
	    else 
	      Conj(p2, p1, hsh)
  in
  let module Table = Hashtbl.Make(
    struct
      type t = prp * prp
      let equal (p1, q1) (p2, q2) =
	p1 == p2 && q1 == q2
      let hash (p, q) =
	(hash p + hash q) land 0x3FFFFFFF
    end)
  in
  let memo = Table.create 23 in
  let _ = Tools.add_at_reset (fun () -> Table.clear memo) in
    fun p q -> 
      if p == q then p else 
	let ((p, q) as pq) = if hash p <= hash q then (p, q) else (q, p) in
	  try
	    Table.find memo pq
	  with
	      Not_found -> 
		let conj = simplified p q in
		  Table.add memo pq conj; conj
		    
let rec mk_disj = function
  | [] -> mk_false
  | [p] -> p
  | p :: pl -> mk_disj2 p (mk_disj pl)

let rec mk_conj = function
  | [] -> mk_true
  | [p] -> p
  | p :: pl -> mk_conj2 p (mk_conj pl)



let mk_iff =
  let module Table = Hashtbl.Make(
    struct
      type t = prp * prp
      let equal (p1, q1) (p2, q2) = 
	p1 == p2 && q1 == q2
      let hash (p, q) = (hash p + hash q) land 0x3FFFFFFF
    end)
 in
 let memo = Table.create 57 in
 let _ = Tools.add_at_reset (fun () -> Table.clear memo) in
  fun p q ->                                     (* order arguments *)
    let ((p, q) as pq) = if hash p <= hash q then (p, q) else (q, p) in 
      try
	Table.find memo pq
      with
	  Not_found -> 
	    let iff = match p with
	      | True -> q
	      | False -> mk_neg q
	      | _ -> 
		  (match q with
		     | True -> p
		     | False -> mk_neg p
		     | _ -> 
			 if p == q then True else
			   let hsh = (hash p + hash q) land 0x3FFFFFFF in
			     Iff(p, q, hsh))
	    in
	      Table.add memo pq iff; iff

let mk_ite = 
 let module Table = Hashtbl.Make(
    struct
      type t = prp * prp * prp
      let equal (p1, q1, r1) (p2, q2, r2) = 
	p1 == p2 && q1 == q2 && r1 == r2
      let hash (p, q, r) =
	(hash p + hash q + hash r) land 0x3FFFFFFF
    end)
 in
 let memo = Table.create 57 in
  let _ = Tools.add_at_reset (fun () -> Table.clear memo) in
    fun p q r -> 
      let pqr = (p, q, r) in
	try
	  Table.find memo pqr
	with
	    Not_found -> 
	      let ite = match p with
		| True -> q
		| False -> r
		| _ -> 
		    if q == r then q else
		      let hsh = (hash p + hash q + hash r) land 0x3FFFFFFF in
			Ite(p, q, r, hsh)
	      in
		Table.add memo pqr ite; ite


(** Mapping an atom transformer. *)
let map atmf p =
  let module Table = Hashtbl.Make(
    struct
      type t = prp
      let equal = (==)
      let hash = hash
    end)
  in
  let memo = Table.create 5 in
  let rec apply p =
    match p with
      | True | False | Var _ -> p 
      | _ -> 
	  (try
	     Table.find memo p
	   with
	       Not_found ->
		 let p' = do_apply p in
		   Table.add memo p p'; p')
  and do_apply p =
    match p with
      | Atom(a) -> 
	  let a' = atmf a in
	    if a == a' then p else mk_poslit a' 
      | Disj(q1, q2, _) ->
	  let q1' = apply q1 in
	  let q2' = apply q2 in
	    if q1 == q1' && q2 == q2' then p else mk_disj2 q1' q2'
      | Conj(q1, q2, _) ->
	  let q1' = apply q1 in
	  let q2' = apply q2 in
	    if q1 == q1' && q2 == q2' then p else mk_conj2 q1' q2'
      | Iff(q1, q2, _) ->
	  let q1' = apply q1 in
	  let q2' = apply q2 in
	    if q1 == q1' && q2 == q2' then p else mk_iff q1' q2'
      | Ite(q1, q2, q3, _) ->
	  let q1' = apply q1 in
	  let q2' = apply q2 in
	  let q3' = apply q3 in
	    if q1 == q1' && q2 == q2' &&q3 == q3' then p else mk_ite q1' q2' q3'
      | Neg(q, _) ->
	  let q' = apply q in
	    if q == q' then p else mk_neg q'
      | True -> p
      | False -> p
      | Var _ -> p
  in
    apply p



(** {6 Translations to/from ICSAT propositions} *)

type prop = int

external icsat_initialize : unit -> unit = "icsat_initialize"
external icsat_finalize : unit -> unit = "icsat_finalize"

external icsat_mk_true : unit -> prop = "icsat_mk_true"
external icsat_mk_false : unit -> prop = "icsat_mk_false"
external icsat_mk_var : string -> prop = "icsat_mk_var"
external icsat_mk_atom : int -> int -> prop = "icsat_mk_atom"

external icsat_mk_or2 : prop -> prop -> prop = "icsat_mk_or2"
external icsat_mk_and2 : prop -> prop -> prop = "icsat_mk_and2"
external icsat_mk_iff : prop -> prop -> prop = "icsat_mk_iff"
external icsat_mk_implies : prop -> prop -> prop = "icsat_mk_implies"
external icsat_mk_xor : prop -> prop -> prop = "icsat_mk_xor"
external icsat_mk_not : prop -> prop = "icsat_mk_not"
external icsat_mk_ite : prop -> prop -> prop -> prop = "icsat_mk_ite"

external icsat_is_true : prop -> bool = "icsat_is_true"
external icsat_is_false : prop -> bool = "icsat_is_false"
external icsat_is_not : prop -> bool = "icsat_is_not"
external icsat_is_or : prop -> bool = "icsat_is_or"
external icsat_is_iff : prop -> bool = "icsat_is_iff"
external icsat_is_ite : prop -> bool = "icsat_is_ite"
external icsat_is_var : prop -> bool = "icsat_is_var"
external icsat_is_atom : prop -> bool = "icsat_is_atom"

external icsat_d_var : prop -> string = "icsat_d_var"
external icsat_d_atom : prop -> int = "icsat_d_atom"
external icsat_d_not : prop -> prop = "icsat_d_not"
external icsat_num_arguments : prop -> int = "icsat_num_arguments"
external icsat_get_argument : prop -> int -> prop = "icsat_get_argument"
external icsat_get_assignment : int -> int = "icsat_get_assignment"

external icsat_print_statistics : unit -> unit = "icsat_print_statistics"

(** Parameter settings for SAT solver *)

external set_verbose : bool -> unit = "icsat_set_verbose"
external set_assertion_frequency : int -> unit = "icsat_set_assertion_frequency"
external set_remove_subsumed_clauses : bool -> unit = "icsat_set_remove_subsumed_clauses"
external set_validate_counter_example : bool -> unit = "icsat_set_validate_counter_example"
external set_polarity_optimization : bool -> unit = "icsat_set_polarity_optimization"
external set_clause_relevance : int -> unit = "icsat_set_clause_relevance"
external set_cleanup_period : int -> unit = "icsat_set_cleanup_period"
external set_num_refinements : int -> unit = "icsat_set_num_refinements"

let debug = (Version.debug() <> 0)

let validate_explanations = ref false
let show_explanations = ref false

let set_validate b =
  validate_explanations := b;
  set_validate_counter_example b

let reduce_explanation = ref false


(** {6 Translating to and from propositions} *)

(** Identification [a |-> i] of atoms [a] in a propositional
 formula with {i consecutive} natural numbers as required by ICSAT. *)
let atom_to_id_tbl = ref Atom.Map.empty

let atom_to_id a =
  try
    Atom.Map.find a !atom_to_id_tbl 
  with
      Not_found -> 
	let b = Atom.negate Arith.mk_neg a in
	let i = Atom.index_of a   (* returns identifier [i] unique to [a]. *)
	and j = Atom.index_of b in
	let id = icsat_mk_atom i j in
	  atom_to_id_tbl := Atom.Map.add a id !atom_to_id_tbl; 
	  id

(** Identification [n <-> i] of variable names [n] in a 
  propositional formula with consecutive indices as required by ICSAT. *)
let vartbl = Name.Hash.create 17 (* internal [id] of ICSAT for a variable *)

let mk_icsat_id x =
  try
    Name.Hash.find vartbl x
  with
      Not_found ->
	let id = icsat_mk_var (Name.to_string x) in
	  Name.Hash.add vartbl x id; id

let var_to_id = mk_icsat_id

type valuation = T | F | X    (* true, false, don't care *)

let disj u v =
  match u, v with
    | T, _ -> T
    | _, T -> T
    | F, _ -> v
    | _, F -> u
    | X, X -> X

let rec disjl = function
  | [] -> F
  | [v] -> v
  | v :: vl -> disj v (disjl vl)

let equiv u v =
  match u, v with
    | X, _ -> X
    | _, X -> X
    | T, T -> T
    | F, F -> T
    | _ -> F

let conditional u v w = 
  match u, v, w with
    | T, _, _ -> v
    | F, _, _ -> w
    | _ when v = w -> v
    | X, _, _ when v <> w -> X
    | _ -> invalid_arg "ite"

let neg = function
  | T -> F
  | F -> T
  | X -> X


let current_boolean_valuation () =
  Name.Hash.fold
    (fun x id acc ->
       match icsat_get_assignment id with
	 | (-1) -> Name.Map.add x F acc
	 | 0 -> Name.Map.add x X acc                   (* don't care *)
	 | 1 -> Name.Map.add x T acc       
	 | _ -> failwith "ICSAT: invalid return value of icsat_get_assignment")
    vartbl Name.Map.empty

let current_atom_valuation () = 
  Atom.Map.fold
    (fun a id acc ->
       (match icsat_get_assignment id with
	  | (-1) -> Atom.Map.add a F acc  
	  | 0 -> Atom.Map.add a X acc
	  | 1 -> Atom.Map.add a T acc    
	  | _ -> failwith "ICSAT: invalid return value of icsat_get_assignment"))
    !atom_to_id_tbl Atom.Map.empty

let is_model_of propval atomval =
 let module Table = Hashtbl.Make(
   struct
     type t = prp
     let equal = (==)
     let hash = hash
   end)
 in
 let memo = Table.create 5 in
 let rec eval p =
   try
     Table.find memo p
   with
       Not_found -> 
	 let e = do_eval p in
	   Table.add memo p e; e
 and do_eval p = 
   match p with
    | True -> 
	T
    | False -> 
	F
    | Var(x) -> 
	assert(Name.Map.mem x propval);
	Name.Map.find x propval
    | Atom(a) -> 
	assert(Atom.Map.mem a atomval);
	Atom.Map.find a atomval
    | Disj(p, q, _) ->
	(match eval p with
	   | T -> T
	   | _ -> eval q)
    | Conj(p, q, _) ->
	(match eval p with
	   | F -> F
	   | _ -> eval q)
    | Iff(p, q, _) ->
	equiv (eval p) (eval q)
    | Ite(p, q, r, _) ->
	let ep = eval p in
	  (match ep with
	     | T -> (eval q)
	     | F -> (eval r)
	     | _ -> 
		 conditional ep (eval q) (eval r))
    | Neg(p, _) ->
	neg (eval p)
 in
   eval


(** Translate propositional formula to one understood by ICSAT. 
  For big formulas this causes stack overflow. *)
let to_prop p =
  let module Table = Hashtbl.Make(
    struct
      type t = prp
      let equal = (==)
      let hash = hash
    end)
  in
  let memo = Table.create 5 in
  let rec translate p =
    try
      Table.find memo p
    with
	Not_found ->
	  let p' = do_translate p in
	    Table.add memo p p'; p'
  and do_translate p =
    match p with
      | True -> 
	  icsat_mk_true()
      | False -> 
	  icsat_mk_false()
      | Var(x) -> 
	  var_to_id x
      | Atom(a) -> 
	  atom_to_id a
      | Disj(p, q, _) ->
	  let p' = translate p in
	  let q' = translate q in
	    icsat_mk_or2 p' q'
      | Conj(p, q, _) ->
	  let p' = translate p in
	  let q' = translate q in
	    icsat_mk_and2 p' q'
      | Iff(p, q, _) ->
	  let p' = translate p in
	  let q' = translate q in
	    icsat_mk_iff p' q'
      | Ite(p, q, r, _) ->
	  let p' = translate p in
	  let q' = translate q in
	  let r' = translate r in
	    icsat_mk_ite p' q' r'
      | Neg(p, _) ->
	  let p' = translate p in
	    icsat_mk_not p'
  in
    translate p

let rec of_prop p =
  if icsat_is_true p then
    mk_true
  else if icsat_is_false p then
    mk_false
  else if icsat_is_var p then
    mk_var (Name.of_string (icsat_d_var p))
  else if icsat_is_not p then
    mk_neg (of_prop p)
  else if icsat_is_or p then
    mk_disj (List.map of_prop (d_disj_prop p))
  else if icsat_is_ite p then
    mk_ite (of_prop (icsat_get_argument p 0))
           (of_prop (icsat_get_argument p 1))
           (of_prop (icsat_get_argument p 2))
  else if icsat_is_iff p then
    mk_iff (of_prop (icsat_get_argument p 0))
           (of_prop (icsat_get_argument p 1))
  else 
    failwith "Fatal error: unknown ICSAT proposition"

and d_disj_prop p = 
  let n = icsat_num_arguments p in
  let args = ref [] in
    for i = 0 to n - 1 do
      args := (icsat_get_argument p i) :: !args
    done;
    !args



(** {6 Atoms} *)

let is_connected i j =
  let a = Atom.of_index i 
  and b = Atom.of_index j in
    Atom.is_connected a b
let _ = Callback.register "atom_is_connected" is_connected

let atom_pp i =
  let a = Atom.of_index i in
    Atom.pp Format.std_formatter a;
    Format.print_flush ()
let _ = Callback.register "prop_atom_pp" atom_pp


(** {6 Lists} *)

(** Callbacks for processing lists. *)
module Lists = struct
  let is_nil l = (l = [])
  let _ = Callback.register "prop_is_nil" is_nil
  let _ = Callback.register "prop_head" List.hd
  let _ = Callback.register "prop_tail" List.tl
  let _ = Callback.register "prop_length" List.length
end


(** {6 Stack} *)

let stack = Stack.create()

let initial = ref (Context.empty, [])

let stack_reset () =   (* reinitialize to starting state *)
  Stack.clear stack;  
  Stack.push !initial stack
let _ = Callback.register "prop_reset" stack_reset

let dup () = 
  let (s, al) = Stack.top stack in
    Stack.push (s, []) stack
let _ = Callback.register "prop_dup" dup

let push (s, al) = Stack.push (s, al) stack

let pop () = 
  let _ = Stack.pop stack in ()
let _ = Callback.register "prop_pop" pop

let top () = 
  let (s, _) = Stack.top stack in
    s
let _ = Callback.register "prop_top" top

let stackpp () =
  Stack.iter (fun (s, _) -> Context.pp Format.std_formatter s) stack
let _ = Callback.register "prop_stackpp" stackpp


(** Interface for shipping explanations to SAT solver *)
module Explanation = struct

  let explained = ref false
  let explanation = Stack.create ()

  let reset () =
    explained := false;
    Stack.clear explanation

  let to_list () = 
    let l = ref [] in
      Stack.iter (fun i -> l := (Atom.of_index i) :: !l) explanation;
      !l

  let pp fmt () =
    Pretty.set Atom.pp fmt (to_list ())

  let is_inconsistent s =
    let l = to_list () in
      Context.is_inconsistent s l

  (** Following disabled. *)
  let rec semantic_reduce hyps =
    match Context.addl Context.empty (Atom.Set.elements hyps) with
      | Context.Status.Inconsistent(rho) ->
	  (try
	     let hyps' = Jst.axioms_of rho in
	       if 3 * (Atom.Set.cardinal hyps') < 2 * (Atom.Set.cardinal hyps) then
		 semantic_reduce hyps'
	       else 
		 hyps' 
	   with
	       Not_found -> hyps)
      | _ -> 
	  hyps

  let install hyps =
    assert(not(Atom.Set.is_empty hyps));
      explained := true;
      assert(Stack.is_empty explanation);
      Atom.Set.iter
	(fun a -> 
	   let i =  Atom.index_of a in
	     Stack.push i explanation)
	hyps;
      if !show_explanations then
	begin
	  Format.eprintf "\nExplanation: \n";
	  Pretty.set Atom.pp Format.err_formatter (Atom.Set.elements hyps)
	end;
      if !validate_explanations then
	if not(is_inconsistent Context.empty) then
	  begin
	    Format.eprintf "\n Suspicious explanation:";
            pp Format.err_formatter ();
	    Format.eprintf "@."
	  end 
	

  let noinstall () =
    explained := false

  let is_explained () = !explained
  let _ = Callback.register "prop_is_explained" is_explained

  let size () = Stack.length explanation
  let _ = Callback.register "prop_explain_size" size
	    
  let is_empty () = Stack.is_empty explanation
  let _ = Callback.register "prop_explain_is_empty" is_empty
	    
  let pop () = Stack.pop explanation
  let _ = Callback.register "prop_explain_pop" pop

end 


let add i =
  let s = top() in
  let a = Atom.of_index i in
    match Context.add s a with
      | Context.Status.Valid _ -> 
	  (let (s, al) = Stack.pop stack in
	     push (s, a :: al);
	     Explanation.noinstall();
	     1)
      | Context.Status.Inconsistent(rho) ->
	  (try
	     let hyps = Jst.axioms_of rho in
	       Explanation.install hyps;
	       0
	   with
	       Not_found ->   (* No explanation generated *)
		   Explanation.noinstall();
		 0)
      | Context.Status.Ok(s) -> 
	  (let (_, al) = Stack.pop stack in
	     push (s, a :: al);
	     Explanation.noinstall();
	     1)

let _ = Callback.register "prop_add" add


(** Scratch state *)

let scratch = ref Context.empty 

let reset_scratch_context () = 
  scratch := Context.empty
let _ = Callback.register "reset_scratch_context" reset_scratch_context

let add_scratch_context i =
  let a = Atom.of_index i in
    match Context.add !scratch a with
      | Context.Status.Valid _ -> 1
      | Context.Status.Inconsistent _ -> 0
      | Context.Status.Ok(s) -> (scratch := s; 1)
let _ = Callback.register "add_scratch_context" add_scratch_context


(** Calling external SAT solver *)

external icsat_sat : prop -> bool -> bool = "icsat_sat"

let used = ref false

let initialize s = 
  if !used then
    invalid_arg "SAT solver already in use"
  else 
    begin
      used := true;
      Explanation.reset();   (* Initialize explanation mechanism *)
      icsat_initialize();    (* Initialize SAT solver *)
      Stack.clear stack;     (* Initialize stack *)
      Stack.push (s, []) stack;
      initial := (s, []);    (* Initial context *)
      scratch := s;          (* Initialize scratch area *)
      atom_to_id_tbl :=  Atom.Map.empty;
      Name.Hash.clear vartbl
    end 

let finalize () =
  icsat_finalize ();
  used := false

let statistics = ref false

module Assignment = struct

  type t = {
    valuation : (Name.t * bool) list;
    literals : Atom.Set.t
  }

  let pp fmt rho =
    if rho.valuation <> [] then
	Pretty.map Name.pp Pretty.bool fmt rho.valuation;
    if not(Atom.Set.is_empty rho.literals) then
      Pretty.list Atom.pp fmt (Atom.Set.elements rho.literals)

end

let rec sat s p =
  try
    initialize s;
    let result =
      let mode = Jst.Mode.get() != Jst.Mode.No in
	if icsat_sat (to_prop p) mode then
	  begin
	    debug_output();
	    check_assignment p;
	    Some(assignment(), top())
	  end 
	else 
	  None
    in
      if !statistics then
	icsat_print_statistics();
      finalize();
      result
  with
      exc ->
	finalize ();
	raise exc

and debug_output () =
  if debug then
    let fmt = Format.std_formatter in
    let bl = ref [] in
      Stack.iter
	(fun (s, al) -> bl := (List.rev al) @ !bl)    
	stack;
      List.iter 
	(fun a -> 
	   Pretty.string fmt "\nassert ";
           Atom.pp fmt a; 
	   Pretty.string fmt ".")
	!bl

and check_assignment p = 
  if !validate_explanations then
    let propval = current_boolean_valuation ()
    and atomval = current_atom_valuation () in
      match is_model_of propval atomval p with
	| T -> 
	    Format.eprintf "Ok: assignment validation succeeds.@.";
	| F -> 
	    Format.eprintf "Fatal error: assignment falsifies formula.@.";
	    exit (1)
	| X -> 
	    Format.eprintf "Warning: not sure about status of assignment.@.";
	    ()

        
and assignment () =
  let valuation =
    Name.Hash.fold
      (fun x id acc ->
	 match icsat_get_assignment id with
	   | (-1) -> (x, false) :: acc
	   | 0 -> acc                   (* don't care *)
	   | 1 -> (x, true) :: acc       
	   | _ -> failwith "ICSAT: invalid return value of icsat_get_assignment")
      vartbl []
  in
  let literals = 
    Atom.Map.fold
      (fun a id acc ->
	  (match icsat_get_assignment id with
	     | (-1) -> Atom.Set.add (Atom.negate Arith.mk_neg a) acc  
	     | 0 -> acc                      (* don't care *)
	     | 1 -> Atom.Set.add a acc       (* true *)
	     | _ -> failwith "ICSAT: invalid return value of icsat_get_assignment"))
      !atom_to_id_tbl Atom.Set.empty
  in
    { Assignment.valuation = valuation; Assignment.literals = literals }

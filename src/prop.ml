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

type t =
  | True
  | False
  | Var of Name.t
  | Atom of Atom.t
  | Disj of t list
  | Iff of t * t
  | Ite of t * t * t
  | Neg of t
  | Let of Name.t * t * t

let rec pp fmt = function
  | True -> Pretty.string fmt "tt"
  | False -> Pretty.string fmt "ff"
  | Var(x) -> Name.pp fmt x
  | Atom(a) -> Atom.pp fmt a
  | Disj(pl) -> Pretty.infixl pp " | " fmt pl
  | Iff(p, q) -> Pretty.infix pp " <=> " pp fmt (p, q)
  | Neg(p) -> 
      Pretty.string fmt "~("; pp fmt p; Pretty.string fmt ")"
  | Let(x, p, q) ->
      Pretty.string fmt "let ";
      Name.pp fmt x;
      Pretty.string fmt " := ";
      pp fmt p;
      Pretty.string fmt " in ";
      pp fmt q;
      Pretty.string fmt " end"
  | Ite(p, q, r) ->
      Pretty.string fmt "if ";
      pp fmt p;
      Pretty.string fmt " then ";
      pp fmt q;
      Pretty.string fmt " else ";
      pp fmt r;
      Pretty.string fmt " end"

let rec occurs x = function
  | True -> false
  | False -> false
  | Var(y) -> Name.eq x y
  | Atom _ -> false
  | Disj(pl) -> List.exists (occurs x) pl
  | Iff(p, q) -> occurs x p || occurs x q
  | Neg(p) -> occurs x p
  | Let(y, p, q) -> if Name.eq x y then false else occurs x p || occurs x q
  | Ite(p, q, r) -> occurs x p || occurs x q || occurs x r

let mk_true = True
let mk_false = False
let mk_var n = Var(n)
let mk_poslit = function
  | Atom.True -> True
  | Atom.False -> False
  | a -> Atom(a)
let mk_neglit a = mk_poslit (Atom.negate a)
let mk_disj pl = Disj(pl)
let mk_iff p q = Iff(p, q)
let mk_ite p q r = Ite(p, q, r)
let mk_neg p = Neg(p)
let mk_conj pl = mk_neg (mk_disj (List.map mk_neg pl))
let mk_let x p q = Let(x, p, q)

let rec apply rho q =
  match q with
    | Var(x) -> 
	(try
	   List.assoc x rho
	 with
	     Not_found -> q)
    | (True | False | Atom _) -> q
    | Disj(ql) ->
	mk_disj (List.map (apply rho) ql)
    | Iff(q1, q2) -> 
	mk_iff (apply rho q1) (apply rho q2)
    | Ite(q1, q2, q3) -> 
	mk_ite (apply rho q1) (apply rho q2) (apply rho q3)
    | Neg(q1) ->
	mk_neg (apply rho q1)
    | Let(x, q1, q2) -> 
	apply ((x, q1) :: rho) q2


(** {6 Translations to/from ICSAT propositions} *)

type prop = int

external icsat_initialize : unit -> unit = "icsat_initialize"
external icsat_finalize : unit -> unit = "icsat_finalize"

external icsat_mk_true : unit -> prop = "icsat_mk_true"
external icsat_mk_false : unit -> prop = "icsat_mk_false"
external icsat_mk_var : string -> prop = "icsat_mk_var"
external icsat_mk_atom : int -> int -> prop = "icsat_mk_atom"

external icsat_mk_or : prop list -> prop = "icsat_mk_or"
external icsat_mk_and : prop list -> prop = "icsat_mk_and"
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
external set_remove_subsumed_clauses : bool -> unit = "icsat_set_remove_subsumed_clauses"
external set_validate_counter_example : bool -> unit = "icsat_set_validate_counter_example"
external set_polarity_optimization : bool -> unit = "icsat_set_polarity_optimization"
external set_clause_relevance : int -> unit = "icsat_set_clause_relevance"
external set_cleanup_period : int -> unit = "icsat_set_cleanup_period"
external set_num_refinements : int -> unit = "icsat_set_num_refinements"


(** Translating to and from propositions *)

module Atomtbl = Hashtbl.Make(
  struct
    type t = Atom.t
    let equal = Atom.eq
    let hash = Hashtbl.hash
  end)


module Inttbl = Hashtbl.Make(
  struct
    type t = int
    let equal = (=)
    let hash = Hashtbl.hash
  end)

module Nametbl = Hashtbl.Make(
  struct
    type t = Name.t
    let equal = Name.eq
    let hash = Hashtbl.hash
  end)

let id = ref 0
let atomtbl = Atomtbl.create 17
let inttbl = Inttbl.create 17

let idtbl = Atomtbl.create 17   (* internal [id] of ICSAT of an atom. *)
let vartbl = Nametbl.create 17  (* internal [id] of ICSAT for a variable *)

let atom_to_id a =
  try
    Atomtbl.find atomtbl a
  with
      Not_found ->
	let i = !id in
	  id := !id + 1;
	  Atomtbl.add atomtbl a i;
	  Inttbl.add inttbl i a;
	  i

let id_to_atom i =
  try
    Inttbl.find inttbl i
  with
      Not_found -> failwith "Fatal error: no atom for ICSAT identifier"

let atom_to_icsat_id a =
  try
    Atomtbl.find idtbl a
  with
      Not_found -> failwith "ICSAT: no such atom id"

let var_to_icsat_id x =
  try
    Nametbl.find vartbl x
  with
      Not_found -> failwith "ICSAT: no such var id"


let to_prop p =
  let rec translate rho p =
    match p with
      | True -> 
	  icsat_mk_true()
      | False -> 
	  icsat_mk_false()
      | Var(x) -> 
	  (try
	     let q = List.assoc x rho in
	       translate rho q
	   with
	       Not_found -> 
		 let id = icsat_mk_var (Name.to_string x) in
		   if not(Nametbl.mem vartbl x) then
		     Nametbl.add vartbl x id;
		   id)
      | Atom(a) -> 
	  assert(Atom.is_negatable a);
	  let b = Atom.negate a in
	  let i = atom_to_id a in
	  let j = atom_to_id b in
	  let id = icsat_mk_atom i j in
	    Trace.msg "foo6" "Idtbl.add" (a, id) (Pretty.pair Atom.pp Pretty.number);
	    if not(Atomtbl.mem idtbl a) then
	      Atomtbl.add idtbl a id;
	    id
      | Let(x, p, q) ->
	  (match p with
	     | Var _ ->
		 raise (Invalid_argument "No variable definitions")
	     | _ ->
		 translate ((x, p) :: rho) q)
      | Disj(pl) ->
	  icsat_mk_or (List.map (translate rho) pl)
      | Iff(p, q) ->
	  icsat_mk_iff (translate rho p) (translate rho q)
      | Ite(p, q, r) ->
	  icsat_mk_ite (translate rho p) (translate rho q) (translate rho r)
      | Neg(p) ->
	  icsat_mk_not (translate rho p)
  in
    translate [] p

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
    mk_disj (List.map of_prop (d_disj p))
  else if icsat_is_ite p then
    mk_ite (of_prop (icsat_get_argument p 0))
           (of_prop (icsat_get_argument p 1))
           (of_prop (icsat_get_argument p 2))
  else if icsat_is_iff p then
    mk_iff (of_prop (icsat_get_argument p 0))
           (of_prop (icsat_get_argument p 1))
  else 
    failwith "Fatal error: unknown ICSAT proposition"

and d_disj p = 
  let n = icsat_num_arguments p in
  let args = ref [] in
    for i = 0 to n - 1 do
      args := (icsat_get_argument p i) :: !args
    done;
    !args

  
(** {6 Lists} *)

let is_nil = function [] -> true | _ -> false
let _ = Callback.register "prop_is_nil" is_nil

let head = List.hd
let _ = Callback.register "prop_head" head

let tail = List.tl
let _ = Callback.register "prop_tail" tail

let length = List.length
let _ = Callback.register "prop_length" length

(** {6 Atoms} *)

let is_connected i j =
  let a = id_to_atom i 
  and b = id_to_atom j in
    Atom.is_connected a b
let _ = Callback.register "atom_is_connected" is_connected

let atom_pp i =
  Trace.msg "foo23" "Atom.pp" i Pretty.number;
  let a = id_to_atom i in
    Atom.pp Format.std_formatter a
let _ = Callback.register "prop_atom_pp" atom_pp

(** {6 Stack} *)

let stack = Stack.create()

let initial = ref Context.empty

let stack_reset () =   (* reinitialize to starting state *)
  Stack.clear stack;  
  Stack.push !initial stack
let _ = Callback.register "prop_reset" stack_reset

let dup () = Stack.push (Stack.top stack) stack
let _ = Callback.register "prop_dup" dup

let push s = Stack.push s stack

let pop () = 
  let _ = Stack.pop stack in ()
let _ = Callback.register "prop_pop" pop

let top () = Stack.top stack
let _ = Callback.register "prop_top" top

let add i =
  let a = id_to_atom i in
  match Context.add (top()) a with
    | Context.Status.Valid -> 1
    | Context.Status.Inconsistent -> 0
    | Context.Status.Ok(s) -> (pop (); push s; 1)
let _ = Callback.register "prop_add" add


(** Scratch state *)

let scratch = ref Context.empty 

let reset_scratch_context () =
  scratch := Context.empty
let _ = Callback.register "reset_scratch_context" reset_scratch_context

let add_scratch_context i =
  let a = id_to_atom i in
    match Context.add !scratch a with
      | Context.Status.Valid -> 1
      | Context.Status.Inconsistent -> 0
      | Context.Status.Ok(s) -> (scratch := s; 1)
let _ = Callback.register "add_scratch_context" add_scratch_context


(** Calling external SAT solver *)

external icsat_sat : prop -> bool = "icsat_sat"

let init s = 
  icsat_initialize();    (* Initialize SAT solver *)
  Stack.clear stack;     (* Initialize stack *)
  Stack.push s stack;
  initial := s;          (* Initial context *)
  scratch := s;          (* Initialize scratch area *)
  id := 0;               (* Initialize translation to props *)
  Atomtbl.clear atomtbl;
  Inttbl.clear inttbl;
  Atomtbl.clear idtbl;
  Nametbl.clear vartbl

let finalize () =
  icsat_finalize ()

let statistics = ref false

module Assignment = struct

  type t = {
    valuation : (Name.t * bool) list;
    literals : Atom.t list
  }

  let pp fmt rho =
    if rho.valuation <> [] then
      Pretty.list (Pretty.assign Name.pp Pretty.bool) fmt rho.valuation;
    if rho.literals <> [] then
      Pretty.list Atom.pp fmt rho.literals

end

let rec sat s p =
  try
    init s;
    let result = 
      if icsat_sat (to_prop p) then 
	Some(assignment ())
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
        
and assignment () =
  let valuation =
    Nametbl.fold
      (fun x id acc ->
	 match icsat_get_assignment id with
	   | (-1) -> (x, false) :: acc
	   | 0 -> acc                   (* don't care *)
	   | 1 -> (x, true) :: acc       
	   | _ -> failwith "ICSAT: invalid return value of icsat_get_assignment")
      vartbl []
  in
  let literals = 
    Atomtbl.fold
      (fun a id acc ->
	 Trace.msg "foo5" "Atom" (id, a) (Pretty.pair Pretty.number Atom.pp);
	  (match icsat_get_assignment id with
	     | (-1) -> Atom.negate a :: acc  
	     | 0 -> acc               (* don't care *)
	     | 1 -> a :: acc          (* true *)
	     | _ -> failwith "ICSAT: invalid return value of icsat_get_assignment"))
      idtbl []
  in
    { Assignment.valuation = valuation; Assignment.literals = literals }
	

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

let mk_true = True
let mk_false = False
let mk_var n = Var(n)
let mk_poslit a = Atom(a)
let mk_neglit a = Atom(Atom.negate a)
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

type prop

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

(** Parameter settings for SAT solver *)

external set_verbose : bool -> unit = "icsat_set_verbose"
external set_remove_subsumed_clauses : bool -> unit = "icsat_set_remove_subsumed_clauses"
external set_validate_counter_example : bool -> unit = "icsat_set_validate_counter_example"
external set_polarity_optimization : bool -> unit = "icsat_set_polarity_optimization"
external set_clause_relevance : int -> unit = "icsat_set_clause_relevance"
external set_cleanup_period : int -> unit = "icsat_set_cleanup_period"


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

let id = ref 0
let atomtbl = Atomtbl.create 17
let inttbl = Inttbl.create 17



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
	       Not_found -> icsat_mk_var (Name.to_string x))
      | Atom(a) -> 
	  assert(Atom.is_negatable a);
	  let b = Atom.negate a in
	  let i = atom_to_id a in
	  let j = atom_to_id b in
	    icsat_mk_atom i j
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
  Inttbl.clear inttbl

let finalize () =
  icsat_finalize ()

let rec sat s p =
  try
    init s;
    let result = 
      if icsat_sat (to_prop p) then 
	Some(assignment p Atom.Set.empty)
      else 
	None
    in
      finalize();
      result
  with
      exc ->
	finalize ();
	raise exc
        
and assignment p acc = 
  match apply [] p with
    | True -> acc
    | False -> acc
    | Var _ -> acc
    | Atom(a) -> 
	let i = atom_to_id a in
	  Atom.Set.add a acc
    | Disj(pl) ->
	List.fold_right assignment pl acc
    | Iff(p, q) -> 
	assignment p (assignment q acc)
    | Ite(p, q, r) -> 
	assignment p (assignment q (assignment r acc))
    | Neg(p) ->
	assignment p acc
    | Let _ ->
	failwith "Failed invariant: 'let' not eliminated"

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


type t

external icsat_initialize : unit -> unit = "icsat_initialize"
external icsat_finalize : unit -> unit = "icsat_finalize"

external icsat_mk_true : unit -> t = "icsat_mk_true"
external icsat_mk_false : unit -> t = "icsat_mk_false"
external icsat_mk_var : string -> t = "icsat_mk_var"
external icsat_mk_atom : int -> int -> t = "icsat_mk_atom"

external icsat_mk_or : t list -> t = "icsat_mk_or"
external icsat_mk_and : t list -> t = "icsat_mk_and"
external icsat_mk_iff : t -> t -> t = "icsat_mk_iff"
external icsat_mk_implies : t -> t -> t = "icsat_mk_implies"
external icsat_mk_xor : t -> t -> t = "icsat_mk_xor"
external icsat_mk_not : t -> t = "icsat_mk_not"
external icsat_mk_ite : t -> t -> t -> t = "icsat_mk_ite"

external icsat_is_true : t -> bool = "icsat_is_true"
external icsat_is_false : t -> bool = "icsat_is_false"
external icsat_is_not : t -> bool = "icsat_is_not"
external icsat_is_or : t -> bool = "icsat_is_or"
external icsat_is_iff : t -> bool = "icsat_is_iff"
external icsat_is_ite : t -> bool = "icsat_is_ite"
external icsat_is_var : t -> bool = "icsat_is_var"
external icsat_is_atom : t -> bool = "icsat_is_atom"

external icsat_d_var : t -> string = "icsat_d_var"
external icsat_d_atom : t -> int = "icsat_d_atom"
external icsat_d_not : t -> t = "icsat_d_not"
external icsat_num_arguments : t -> int = "icsat_num_arguments"
external icsat_get_argument : t -> int -> t = "icsat_get_argument"

let _ = icsat_initialize()  (* Initialize SAT solver *)

let mk_true = icsat_mk_true
let mk_false = icsat_mk_false

let mk_var str = icsat_mk_var (Name.to_string str)

let mk_disj = icsat_mk_or

(** Translating between Atoms and ids *)

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

let reset () = 
  id := 0; 
  Atomtbl.clear atomtbl;
  Inttbl.clear inttbl

let _ =  Tools.add_at_reset reset

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

let mk_poslit a =   
  assert(Atom.is_negatable a);
  let b = Atom.negate a in
  let i = atom_to_id a in
  let j = atom_to_id b in
    icsat_mk_atom i j

let mk_neglit a = 
  mk_poslit (Atom.negate a)

let mk_iff = icsat_mk_iff
  
let mk_neg = icsat_mk_not

let mk_conj = icsat_mk_and

let mk_ite = icsat_mk_ite

let is_true = icsat_is_true
let is_false = icsat_is_false

let is_atom = icsat_is_atom
let is_neg = icsat_is_not

let is_disj = icsat_is_or
let is_ite =  icsat_is_ite
let is_iff = icsat_is_iff

let is_var = icsat_is_var

let d_var p =
  let str = icsat_d_var p in
    Name.of_string str

let d_atom p =
  let i = icsat_d_atom p in
    id_to_atom i

let d_neg = icsat_d_not

let d_disj p = 
  let n = icsat_num_arguments p in
  let args = ref [] in
    for i = 0 to n - 1 do
      args := (icsat_get_argument p i) :: !args
    done;
    !args

let d_ite p =
  (icsat_get_argument p 0, icsat_get_argument p 1, icsat_get_argument p 2)

let d_iff p = 
  (icsat_get_argument p 0, icsat_get_argument p 1)

(** Lists *)

let is_nil = function [] -> true | _ -> false
let _ = Callback.register "prop_is_nil" is_nil

let head = List.hd
let _ = Callback.register "prop_head" head

let tail = List.tl
let _ = Callback.register "prop_tail" tail

let length = List.length
let _ = Callback.register "prop_length" length


(** Stack *)

let stack = Stack.create()

let init s = 
  Stack.clear stack;
  Stack.push s stack

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

let add a =
  match Context.add (top()) a with
    | Context.Status.Valid -> 1
    | Context.Status.Inconsistent -> 0
    | Context.Status.Ok(s) -> (pop (); push s; 1)
let _ = Callback.register "prop_add" add


(** Scratch state *)

let scratch = ref Context.empty 

let prop_assert_in_scratch_context a =
  match Context.add !scratch a with
    | Context.Status.Valid -> 1
    | Context.Status.Inconsistent -> 0
    | Context.Status.Ok(s) -> (scratch := s; 1)



(** Calling external SAT solver *)

external icsat : t -> bool = "icsat_sat"

let rec sat s p =
  try
    init s;       (* Initialize stack *)
    initial := s; (* Initial context *)
    scratch := s; (* Initialize scratch area *)
    let result = 
      if not(icsat p) then
	None
      else 
	Some(assignment p Atom.Set.empty)
    in
      reset ();
      result
  with
      exc ->
	reset ();
	raise exc
    
    
and assignment p acc =
  if is_true p || is_false p || is_var p then acc
  else if is_atom p then
    let a = d_atom p in
      Atom.Set.add a acc    (* how to test for assignment? *)
  else if is_neg p then
    assignment (d_neg p) acc
  else if is_disj p then
    List.fold_right assignment (d_disj p) acc
  else if is_ite p then
    let (q1, q2, q3) = d_ite p in
      assignment q1 (assignment q2 (assignment q3 acc))
  else if is_iff p then
    let (q1, q2) = d_iff p in
      assignment q1 (assignment q2 acc)
  else
    failwith "Fatal error: unknown ICSAT proposition"

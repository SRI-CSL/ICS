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

type dt

let get p = p

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
      Pretty.mixfix "let" Name.pp ":=" pp "in" pp "end" fmt (x, p, q)
  | Ite(p, q, r) ->
      Pretty.mixfix "if" pp "then" pp "else" pp "end" fmt (p, q, r)

let mk_true = True
let mk_false = False
let mk_var n = Var(n)
let mk_poslit a =
  match Atom.atom_of a with
    | Atom.True -> True
    | Atom.False -> False
    | _ ->  Atom(a)
let mk_neglit a = mk_poslit (Atom.negate a)
let mk_disj pl = Disj(pl)
let mk_iff p q = Iff(p, q)
let mk_ite p q r = Ite(p, q, r)
let mk_neg p = Neg(p)
let mk_conj pl = mk_neg (mk_disj (List.map mk_neg pl))
let mk_let x p q = Let(x, p, q)

let is_true = function True -> true | _ -> false
let is_false = function False -> true | _ -> false
let is_var = function Var _ -> true | _ -> false
let is_atom = function Atom _ -> true | _ -> false
let is_disj = function Disj _ -> true | _ -> false
let is_iff = function Iff _ -> true | _ -> false
let is_ite = function Ite _ -> true | _ -> false
let is_neg = function Neg _ -> true | _ -> false
let is_let = function Let _ -> true | _ -> false

let d_var = function Var(x) -> x | _ -> invalid_arg "wrong propositional argument"
let d_atom = function Atom(a) -> a | _ -> invalid_arg "wrong propositional argument"
let d_disj = function Disj(dl) -> dl | _ -> invalid_arg "wrong propositional argument"
let d_iff = function Iff(p, q) -> (p, q) | _ -> invalid_arg "wrong propositional argument"
let d_ite = function Ite(p, q, r) -> (p, q, r) | _ -> invalid_arg "wrong propositional argument"
let d_neg = function Neg(p) -> p | _ -> invalid_arg "wrong propositional argument"
let d_let = function Let(x, p, q) -> (x, p, q) | _ -> invalid_arg "wrong propositional argument"


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

let print_consistent_context = ref true


(** {6 Translating to and from propositions} *)

(** Identification [a |-> i] of atoms [a] in a propositional
 formula with {i consecutive} natural numbers as required by ICSAT. *)
let atom_to_id_tbl = ref Atom.Map.empty

let atom_to_id a =
  try
    Atom.Map.find a !atom_to_id_tbl 
  with
      Not_found -> 
	let b = Atom.negate a in
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


(** Translate propositional formula to one understood by ICSAT. *)
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
	       Not_found -> var_to_id x)
      | Atom(a) -> 
	  atom_to_id a
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

  let install hyps = 
    explained := true;
    assert(Stack.is_empty explanation);
    Atom.Set.iter
      (fun a -> 
	 let i =  Atom.index_of a in
	 Stack.push i explanation)
      hyps

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
  let a = Atom.of_index i in
    match Context.add (top()) a with
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

let initialize s = 
  Explanation.reset();   (* Initialize explanation mechanism *)
  icsat_initialize();    (* Initialize SAT solver *)
  Stack.clear stack;     (* Initialize stack *)
  Stack.push (s, []) stack;
  initial := (s, []);    (* Initial context *)
  scratch := s;          (* Initialize scratch area *)
  atom_to_id_tbl :=  Atom.Map.empty;
  Name.Hash.clear vartbl

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
	Pretty.map Name.pp Pretty.bool fmt rho.valuation;
    if rho.literals <> [] then
      Pretty.list Atom.pp fmt rho.literals

end

let rec sat s p =
  try
    initialize s;
    let result = 
      let mode = Jst.Mode.get() != Jst.Mode.No in
	if icsat_sat (to_prop p) mode then
	  begin
	    debug();
	    Some(assignment (), top())
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

and debug () =
  if !print_consistent_context then
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
	     | (-1) -> Atom.negate a :: acc  
	     | 0 -> acc               (* don't care *)
	     | 1 -> a :: acc       (* true *)
	     | _ -> failwith "ICSAT: invalid return value of icsat_get_assignment"))
      !atom_to_id_tbl []
  in
    { Assignment.valuation = valuation; Assignment.literals = literals }


(*i
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
 i*)

(*i*)
open Name.Map
(*i*)

(*s Global state. *)

type t = {
  mutable current : Context.t;
  mutable symtab : Symtab.t;
  mutable inchannel : in_channel;
  mutable outchannel : Format.formatter;
  mutable eot : string;
  mutable counter : int
}

let init () = {
  current = Context.empty;
  symtab = Symtab.empty;
  inchannel = Pervasives.stdin;
  outchannel = Format.std_formatter;
  eot = "";
  counter = 0
}

let s = init ()

(*s Initialize. *)

let initialize pp eot inch outch =
  Term.pretty := pp;
  s.eot <- eot;
  s.inchannel <- inch;
  s.outchannel <- outch
  

(*s Accessors to components of global state. *)

let current () = s.current
let symtab () = s.symtab
let eot () = s.eot
let inchannel () = s.inchannel
let outchannel () = s.outchannel


(*s Adding to symbol table *)

let def n =
  Trace.proc "istate" "def" Term.pp 
  (fun a ->
     let e = Symtab.Def(a) in
       s.symtab <- Symtab.add n e s.symtab)
  
let sgn n a =
  let e = Symtab.Arity(a) in
  s.symtab <- Symtab.add n e s.symtab

let typ n c =
  let e = Symtab.Type(c) in
  s.symtab <- Symtab.add n e s.symtab


let entry_of n = 
  try
    Some(Symtab.lookup n s.symtab)
  with
      Not_found -> None
			   
(*s Type from the symbol table. *)

let type_of n =
  match Symtab.lookup n s.symtab with
    | Symtab.Type(c) -> Some(c)
    | _ -> None

(*s Get context for name in symbol table *)

let context_of n = 
  match Symtab.lookup n s.symtab with
    | Symtab.State(c) -> c
    | _ -> raise (Invalid_argument("No context of name " ^ (Name.to_string n)))

(*s Getting the width of bitvector terms from the signature. *)

let width_of a =
  if Term.is_var a then
    let n = Term.name_of a in
    try
      match Symtab.lookup n s.symtab with
	| Symtab.Arity(i) -> Some(i)
	| _ -> None
    with
	Not_found -> None
  else
    Bitvector.width a

(*s Resetting all of the global state. *)

let reset () = 
  Tools.do_at_reset ();
  s.current <- Context.empty;
  s.symtab <- Symtab.empty;
  s.counter <- 0

(*s Getting either current context or explicitly specified context. *)

let get_context = function
  | None -> s.current
  | Some(n) -> context_of n

(*s Set input and output channels. *)

let set_inchannel ch = 
  s.inchannel <- ch

let set_outchannel fmt = 
  s.outchannel <- fmt

let flush () = Format.fprintf s.outchannel "@?"
let nl () = Format.fprintf s.outchannel "\n"


(*s Context. *)

let ctxt_of = function
  | None -> Context.ctxt_of s.current
  | Some(n) -> Context.ctxt_of (context_of n)

(*s Canonization w.r.t current state. *)

let can = 
  Trace.func "istate" "can" Atom.pp Atom.pp
    (Tools.profile "can"
       (fun p -> Can.atom s.current p))

let cant a = 
  Can.term s.current a

let sigma f l =
  Context.sigma s.current f l



(*s Create a fresh name for a state. *)

let rec fresh_state_name () =
  s.counter <- s.counter + 1;
  let n = Name.of_string ("s" ^ (string_of_int s.counter)) in
  try
    let _ = Symtab.lookup n s.symtab in  (* make sure state name is really fresh. *)
    fresh_state_name ()
  with
      Not_found -> 
	n

(*s Change current state. *)

let save arg =
  let n = match arg with
    | None -> fresh_state_name ()
    | Some(n) -> n
  in
  let e = Symtab.State s.current in
  s.symtab <- Symtab.add n e s.symtab;
  n

let restore n =
  try
    match Symtab.lookup n s.symtab with
      | Symtab.State(t) -> s.current <- t
      | _ -> raise Not_found
  with
      Not_found -> raise (Invalid_argument "Not a state name")

let remove n =      
  s.symtab <- Symtab.remove n s.symtab

let forget () =
  s.current <- Context.empty

(*s Adding a new fact *)

let process n =
  let t = (get_context n) in
    Tools.profile "process"
      (fun a -> 
	 let status = Process.atom t a in
	   match status with      (* Update state and install new name in symbol table *)
	     | Process.Ok(t') -> 
		 s.current <- t';
		 let n = save None in
		   Process.Ok(n)
	     | Process.Valid ->
		 Process.Valid
	     | Process.Inconsistent ->
		 Process.Inconsistent)

let valid n a =
  match Process.atom (get_context n) a with 
    | Process.Valid -> true
    | _ -> false

let unsat n a =
  match Process.atom (get_context n) a with 
    | Process.Inconsistent -> true
    | _ -> false

(*s Accessors. *)

let diseq n a =
  let s = get_context n in
  let a' = Can.term s a in
  try
    Context.d s a'
  with
      Not_found -> Term.Set.empty

let cnstrnt n a =
  let s = get_context n in
  let a' = Can.term s a in
  try
    Some(Context.cnstrnt s a')
  with
      Not_found -> None


(*s Applying maps. *)


let find n i x = Context.find i (get_context n) x
let inv n i b = Context.inv i (get_context n) b
let use n i = Context.use i (get_context n)

(*s Solution sets. *)

let solution n i = 
  Solution.fold
    (fun x (a, _) acc -> (x, a) :: acc)
    (Context.eqs_of (get_context n) i)
    []

(*s Variable partitioning. *)

let partition () = 
  Term.Map.fold
    (fun x ys acc ->
       (x, Term.Set.elements ys) :: acc)
    (V.partition (Context.v_of s.current))
    []

(*s Solver. *)

let solve i (a, b) = 
  try
    let e = Fact.mk_equal a b None in
      List.map (fun e' -> 
		  let (x, b, _) = Fact.d_equal e' in
		    (x, b))
	(Th.solve i e)
  with
    | Exc.Inconsistent -> raise(Invalid_argument("Unsat"))
    | Exc.Unsolved -> raise(Invalid_argument("Unsolvable"))
 
(*s Equality/disequality test. *)

let is_equal a b =
  Can.eq s.current a b

let is_int a =
  try
    let c = Context.cnstrnt s.current a in
     Cnstrnt.dom_of c = Dom.Int
  with
      Not_found -> false
	

(*s Splitting. *)

let split () = Context.split s.current

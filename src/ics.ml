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

(** {6 Initialization} *)

open Format

external ics_is_licensed : unit -> bool = "ics_is_licensed"

let is_licensed = ref false

let license_check () = 
  if !is_licensed then
    ()
  else if ics_is_licensed() then
    begin
      is_licensed := true;
    end 
  else 
    begin
      Format.eprintf "\nExit...";
      exit(-1)
    end

let init (n) =
  license_check ();
  if n = 0 then
    Sys.catch_break true                 (** raise [Sys.Break] exception upon *)
                                         (** user interrupt. *)

let _ = Callback.register "init" init

let version = Version.print
let _ = Callback.register "version" version

let api_error str = 
  let str' = Format.sprintf "ICS API Error: %s.@." str in
    invalid_arg str'


(** {6 Controls} *)

let reset () = Tools.do_at_reset ()
let _ = Callback.register "reset" reset

let gc () = Gc.full_major ()
let _ = Callback.register "gc" gc

let flush = print_flush
let _ = Callback.register "flush" flush

(** Sleeping. *)
let sleep = Unix.sleep
let _ = Callback.register "sleep" sleep


(** {6 Status flag} *)

module Status = struct

  type t =
    | Valid of Judgement.atom
    | Unsat of Judgement.unsat
    | Ok of Context.t

  let pp fmt = function
    | Valid _ -> Format.fprintf fmt "valid"
    | Unsat _ -> Format.fprintf fmt "unsat"
    | Ok _ -> Format.fprintf fmt "ok"

end



(** {6 Values and Kinds} *)

type top
  (** Encompassing all Ocaml objects. *)

let inj (o: 'o) = ((Obj.magic o): top)
let out (all: top) = Obj.magic all

module Kind = struct

  module Ground = struct

    type t = 
	Name | Bool | Int | Rat 
      | Theory | Funsym | Term | Atom | Prop | Cnstrnt 
      | Justification | Model
      | Context | Status

    let to_string = function
      | Name -> "name"
      | Bool -> "bool"
      | Int -> "int"
      | Rat -> "rat"
      | Theory -> "theory"
      | Funsym -> "funsym"
      | Term -> "term"
      | Atom -> "atom" 
      | Prop -> "prop"
      | Cnstrnt -> "cnstrnt"
      | Justification-> "justification"
      | Model -> "model"
      | Context -> "context"
      | Status -> "status"


    let to_name (o: top) = ((out o) : Name.t)
    let to_bool (o: top) = ((out o) : bool)
    let to_int (o: top) = ((out o) : int)
    let to_rat (o: top) = ((out o) : Mpa.Q.t)
    let to_funsym (o: top) = ((out o) : Funsym.t)
    let to_term (o: top) = ((out o) : Term.t)
    let to_atom (o: top) = ((out o) : Atom.t)
    let to_prop (o: top) = ((out o) : Prop.t)
    let to_cnstrnt (o: top) = ((out o) : Cnstrnt.t)
    let to_justification (o: top) = ((out o) : Judgement.atom)
    let to_model (o: top) = ((out o) : Term.Model.t)
    let to_theory (o: top) = ((out o) : Theory.t)
    let to_context (o: top) = ((out o) : Context.t)
    let to_status (o: top) = ((out o) : Status.t)


    let eq k (o1: top) (o2: top) =
      match k with
	| Name -> Name.eq (to_name o1) (to_name o2)
	| Bool -> to_bool o1 = to_bool o2
	| Int -> to_int o1 = to_int o2
	| Rat -> Mpa.Q.equal (to_rat o1) (to_rat o2)
	| Funsym -> Funsym.eq (to_funsym o1) (to_funsym o2)
	| Term -> Term.eq (to_term o1) (to_term o2)
	| Atom -> Atom.eq (to_atom o1) (to_atom o2)
	| Prop ->(to_prop o1) == (to_prop o2)
	| Cnstrnt -> (to_cnstrnt o1) = (to_cnstrnt o2)
	| Justification -> (to_justification o1) == (to_justification o2)
	| Model -> (to_model o1) == (to_model o2)
	| Theory -> Theory.eq (to_theory o1) (to_theory o2)
	| Context -> Context.eq (to_context o1) (to_context o2)
	| Status -> (to_status o1) == (to_status o2)

    let pp k fmt (o: top) =
      match k with
	| Name -> Name.pp fmt (to_name o)
	| Bool -> Pretty.bool fmt (to_bool o)
	| Int -> Pretty.number fmt (to_int o)
	| Rat -> Mpa.Q.pp fmt (to_rat o)
	| Funsym -> Funsym.pp fmt (to_funsym o)
	| Term -> Term.pp fmt (to_term o)
	| Atom -> Atom.pp fmt (to_atom o)
	| Prop -> Prop.pp fmt (to_prop o)
	| Cnstrnt -> Cnstrnt.pp fmt (to_cnstrnt o)
	| Justification -> Judgement.pp fmt (to_justification o)
	| Model -> Term.Model.pp fmt (to_model o)
	| Theory -> Theory.pp fmt (to_theory o)
	| Context -> Context.pp fmt (to_context o)
	| Status -> Status.pp fmt (to_status o)
	
  end 

  type t =
    | Ground of Ground.t
    | Pair of t * t
    | List of t

  let mk_ground g = Ground(g)

  let mk_name = mk_ground Ground.Name
  let mk_int = mk_ground Ground.Int
  let mk_bool = mk_ground Ground.Bool
  let mk_rat = mk_ground Ground.Rat
  let mk_funsym = mk_ground Ground.Funsym
  let mk_term = mk_ground Ground.Term
  let mk_atom = mk_ground Ground.Atom
  let mk_prop = mk_ground Ground.Prop
  let mk_cnstrnt = mk_ground Ground.Cnstrnt
  let mk_justification = mk_ground Ground.Justification
  let mk_model = mk_ground Ground.Model
  let mk_theory = mk_ground Ground.Theory
  let mk_status = mk_ground Ground.Status
  let mk_context = mk_ground Ground.Context

  let mk_pair k l = Pair(k, l)

  let mk_list k = List(k)

  let rec to_string = function
    | Ground(g) -> Ground.to_string g
    | Pair(k1, k2) -> Format.sprintf "(%s * %s)" (to_string k1) (to_string k2)
    | List(k) -> Format.sprintf "%s list" (to_string k)

  let compatible = (=)

  let rec eq k (o1: top) (o2: top) =
    match k with
      | Ground(gnd) -> 
	  Ground.eq gnd o1 o2
      | Pair(k1, k2) ->
	  eq k1 (fst (out o1)) (fst (out o2)) &&
	  eq k2 (snd (out o1)) (snd (out o2))
      | List(k) ->
	  try 
	    List.for_all2 (eq k) (out o1) (out o2) 
	  with    
	      Invalid_argument _ -> false

  let rec pp k fmt (o: top) = 
    match k with
      | Ground(gnd) -> 
	  Ground.pp gnd fmt o
      | Pair(k1, k2) ->
	  Pretty.pair (pp k1) (pp k2) fmt (out o)
      | List(k) ->
	  Pretty.list (pp k) fmt (out o)
	  
end 

(** A {i value} is an index to a table which contains an index to a 
  pair [(k, o)] where [k] is a kind and [o] a corresponding object. *)
module Value = struct

  type t = int

  module Intmap = Hashtbl.Make(
    struct
      type t = int
      let equal = (=)
      let hash i = i
    end)

  type idx = int

  let val2idx: idx Intmap.t = Intmap.create 1023               (* Bindings [v |-> i]. *)
  let idx2pair: (Kind.t * top) Intmap.t = Intmap.create 1023 (* Bindings [i|->(k,o)]. *)
  let freelist: idx list ref = ref []                        (* List of free indices. *)
  let nonce = ref 2                                      (* New value uses only once. *)

  let ff: t = 0    
  let tt: t = 1

  let reset () =
    Intmap.clear val2idx;
    Intmap.clear idx2pair;
    freelist := [];
    nonce := 2

  let is_registered (v: t) =
    Intmap.mem val2idx v

  let get = function
    | 0 -> (Kind.mk_bool, inj false)
    | 1 -> (Kind.mk_bool, inj true)
    | v -> 
	try
	  let idx = Intmap.find val2idx v in
	    assert(Intmap.mem idx2pair idx);
	    Intmap.find idx2pair idx
	with
	    Not_found -> invalid_arg ("Index " ^ string_of_int v ^ " not registered.")
	
  let rec register ko =
    incr nonce;
    assert(not(Intmap.mem val2idx !nonce));
    let idx = find_free () in
      assert(not(Intmap.mem idx2pair idx));
      Intmap.add val2idx !nonce idx;
      Intmap.add idx2pair idx ko;
      !nonce

  and find_free () =
    match !freelist with
      | [] ->  
	  assert(not(Intmap.mem idx2pair !nonce));
	  !nonce
      | idx :: idxl -> 
	  assert(not(Intmap.mem idx2pair idx));
	  freelist := idxl;
	  idx
	  
  let deregister = function
    | 0 -> ()
    | 1 -> ()
    | v -> 
	try
	  let idx = Intmap.find val2idx v in
	    assert(Intmap.mem idx2pair idx);
	    Intmap.remove val2idx v;
	    Intmap.remove idx2pair idx;
	    freelist := idx :: !freelist
	with
	    Not_found -> ()

  let is_kind (k: Kind.t) (v: t) = 
    let l, _ = get v in
      Kind.compatible k l

  let pp fmt (v: t) = 
    let k, o = get v in
      Format.fprintf fmt "@[<addr=%d;@ kind=%s;@ ;val=" v (Kind.to_string k);
      Kind.pp k fmt o;
      Format.fprintf fmt ">@]@."

  let to_bool = function
    | 0 -> false
    | 1 -> true
    | v -> api_error ("Value " ^ string_of_int v ^ " not a Boolean value.")
	
  let of_bool = function true -> 1 | false -> 0
   
  (** Like [get] but with kind-checking. *)
  let lookup (k: Kind.t) (v: t) =
    let l, o = get v in
      if Kind.compatible k l then o else 
	api_error (Format.sprintf "Value %s of kind %s but %s expected"
		     (string_of_int v) (Kind.to_string l) (Kind.to_string k))

  let to_name (v: t) = out (lookup Kind.mk_name v)
  let of_name (n: Name.t) = register (Kind.mk_name, inj n)

  let to_int (v: t) = out (lookup Kind.mk_int v)
  let of_int (i: int) = register (Kind.mk_int, inj i)

  let to_rat (v: t) = out (lookup Kind.mk_rat v)
  let of_rat (q: Mpa.Q.t) = register (Kind.mk_rat, inj q)

  let to_funsym (v: t) = out (lookup Kind.mk_funsym v)
  let of_funsym (f: Funsym.t) = register (Kind.mk_funsym, inj f)

  let to_term (v: t) = out (lookup Kind.mk_term v)
  let of_term (t: Term.t) = register (Kind.mk_term, inj t)

  let to_atom (v: t) = out (lookup Kind.mk_atom v)
  let of_atom (a: Atom.t) = register (Kind.mk_atom, inj a)

  let to_prop (v: t) = out (lookup Kind.mk_prop v)
  let of_prop (p: Prop.t) = register (Kind.mk_prop, inj p)

  let to_cnstrnt (v: t) = out (lookup Kind.mk_cnstrnt v)
  let of_cnstrnt (c: Cnstrnt.t) = register (Kind.mk_cnstrnt, inj c)

  let to_justification (v: t) = out (lookup Kind.mk_justification v)
  let of_justification (jst: Judgement.atom) = register (Kind.mk_justification, inj jst)

  let to_model (v: t) = out (lookup Kind.mk_model v)
  let of_model (jst: Judgement.atom) = register (Kind.mk_model, inj jst)

  let to_theory (v: t) = out (lookup Kind.mk_theory v)
  let of_theory (th: Theory.t) = register (Kind.mk_theory, inj th)

  let to_context (v: t) = out (lookup Kind.mk_context v)
  let of_context (ctxt: Context.t) = register (Kind.mk_context, inj ctxt)

  let to_status (v: t) = out (lookup Kind.mk_status v)
  let of_status (st: Status.t) = register (Kind.mk_status, inj st)

  let is_nil u =
    match get u with
      | Kind.List _, o -> out o = []
      | _ -> false

  let to_list k (v: t) = out (lookup (Kind.mk_list k) v)
  let of_list k (l: 'a list) = register (Kind.mk_list k, inj l)

  let to_pair k1 k2 (v: t) = out (lookup (Kind.mk_pair k1 k2) v)
  let of_pair k1 k2 (p: 'a * 'b) = register (Kind.mk_pair k1 k2, inj p)

end

(** {6 Values} *)

type value = Value.t

let is_registered u = Value.of_bool (Value.is_registered u)
let _ = Callback.register "is_registered" is_registered

let deregister (v: Value.t) = Value.deregister v; Value.tt
let _ = Callback.register "deregister" deregister

let kind v =
  let k, _ = Value.get v in
  let str = Kind.to_string k in
    Value.of_name (Name.of_string str)
let _ = Callback.register "kind" kind

let pp v = 
  let fmt = Format.std_formatter in
    Value.pp fmt v; Value.tt
let _ = Callback.register "pp" pp

let eq u1 u2 =
  let k1, o1 = Value.get u1 
  and k2, o2 = Value.get u2 in
    Value.of_bool (Kind.compatible k1 k2 && Kind.eq k1 o1 o2)
let _ = Callback.register "eq" eq


(** {6 Name Values} *)

let is_name u = Value.of_bool (Value.is_kind Kind.mk_name u)
let _ = Callback.register "is_name" is_name

let intern str = Value.of_name (Name.of_string str)
let _ = Callback.register "intern" intern

let extern u = Name.to_string (Value.to_name u)
let _ = Callback.register "extern" extern


(** {6 Booleans} *)

let is_bool u = Value.of_bool (Value.is_kind Kind.mk_bool u)
let _ = Callback.register "is_bool" is_bool

let ff () = Value.ff
let _ = Callback.register "ff" ff

let tt () = Value.tt
let _ = Callback.register "tt" tt


(** {6 Integers} *)

let is_int u = Value.of_bool (Value.is_kind Kind.mk_int u)
let _ = Callback.register "is_int" is_int

let integerize = Value.of_int
let _ = Callback.register "integerize" integerize  


(** {6 Multi-precision rationals} *)

let is_rat u = Value.of_bool (Value.is_kind Kind.mk_rat u)
let _ = Callback.register "is_rat" is_rat

let rat_of_int u = Value.of_rat (Mpa.Q.of_int (Value.to_int u))
let _ = Callback.register "rat_of_int" rat_of_int

let rat_of_ints u v = Value.of_rat (Mpa.Q.div (Value.to_int u) (Value.to_int v))
let _ = Callback.register "rat_of_ints" rat_of_ints


(** {6 Pairs} *)

let is_pair u =
  match Value.get u with
    | Kind.Pair _, _ -> Value.of_bool true
    | _ -> Value.of_bool false
let _ = Callback.register "is_pair" is_pair

let pair u1 u2 = 
  let k1, o1 = Value.get u1 and k2, o2 = Value.get u2 in
  let k = Kind.mk_pair k1 k2 and o = inj (o1, o2) in
    Value.register (k, o)
let _ = Callback.register "pair" pair

let fst u =
  let k, o = Value.get u in
    match k with
      | Kind.Pair(k1, _) -> 
	  let o1 = inj(fst(out o)) in
	    Value.register (k1, o1)
    | _ -> 
	api_error(Format.sprintf "value %d not a pair" u)
let _ = Callback.register "fst" fst

let snd u = 
  let k, o = Value.get u in
    match k with
      | Kind.Pair(_,k2) -> 
	  let o2 = inj (fst(out o)) in
	    Value.register (k2, o2)
      | _ -> 
	  api_error(Format.sprintf "value %d not a pair" u)  
let _ = Callback.register "snd" snd


(** {6 Lists} *)

let is_list u = 
  match Value.get u with
    | Kind.List _, _ -> Value.of_bool true
    | _ -> Value.of_bool false
let _ = Callback.register "is_list" is_list

let is_nil u = Value.of_bool (Value.is_nil u)
let _ = Callback.register "is_nil" is_nil
    
let is_cons u =
 let k, o = Value.get u in
    match k with
      | Kind.List _ -> Value.of_bool (out o <> [])
      | _ -> Value.of_bool false
let _ = Callback.register "is_cons" is_cons

let nil () =
  let arbitrary = Kind.mk_bool in 
  Value.register (Kind.mk_list arbitrary, inj [])
let _ = Callback.register "is_nil" is_nil

let cons u1 u2 =
  let k1, o1 = Value.get u1 in
    if Value.is_nil u2 then  (* special case for [nil] as it contains arbitrary kind. *)
      Value.register (Kind.mk_list k1, inj [o1])
    else 
      let k2, o2 = Value.get u2 in
	match k2 with
	  | Kind.List(l2) when Kind.compatible k2 l2 -> 
	      Value.register (k2, inj (out o1 :: out o2))
	  | _ ->
	      api_error "value %d not a compatible list" u2
let _ = Callback.register "cons" cons 

let head u =
  let k, o = Value.get u in
    match k with
      | Kind.List(l) -> Value.register (l, inj (List.hd (out o)))
      | _ -> api_error "value %d not a list" u
let _ = Callback.register "head" head

let tail u =
  let k, o = Value.get u in
    match k with
      | Kind.List(l) -> Value.register (l, inj (List.tl (out o)))
      | _ -> api_error "value %d not a list" u
let _ = Callback.register "tail" tail



(** {6 Theories} *)

let is_theory u = Value.of_bool (Value.is_kind Kind.mk_theory u)
let _ = Callback.register "is_theory" is_theory

let theory_of_name u =
  Value.of_theory (Theory.of_string (Name.to_string (Value.to_name u)))
let _ = Callback.register "theory_of_name" theory_of_name

let description u = 
  Value.of_name (Name.of_string (Theory.Description.get (Value.to_theory u)))
let _ = Callback.register "description" description


(** {6 Function Symbols} *)

let is_funsym u = Value.of_bool (Value.is_kind Kind.mk_funsym u)
let _ = Callback.register "is_funsym" is_funsym
  
let funsym_make u v =
  let th = Value.to_theory u in
  let n = Value.to_name v in
    Value.of_funsym (Funsym.create th n)
let _ = Callback.register "funsym_make" funsym_make


let funsym_theory_of u = 
  let f = Value.to_funsym u in
  let th = Funsym.theory_of f in
    Value.of_name (Name.of_string (Theory.to_string th))
let _ = Callback.register "funsym_theory_of" funsym_theory_of

let funsym_name_of u = 
  Value.of_name (Funsym.name_of (Value.to_funsym u))
let _ = Callback.register "funsym_name_of" funsym_name_of


(** {6 Terms} *)

let is_term u =  Value.of_bool (Value.is_kind Kind.mk_term u)
let _ = Callback.register "is_term" is_term

let term_of_name u = 
  let s = Name.to_string (Value.to_name u) in
  let lb = Lexing.from_string s in 
  let a = Parser.termeof Lexer.token lb in
    Value.of_term a
let _ = Callback.register "term_of_name" term_of_name

let term_to_name u = 
  Value.of_name (Name.of_string (Pretty.to_string Term.pp (Value.to_term u)))
let _ = Callback.register "term_to_name" term_to_name

let term_input () =
  let ch = Istate.Inchannel.get() in
  let lb = Lexing.from_channel ch in 
    Value.of_term (Parser.termeof Lexer.token lb)
let _ = Callback.register "term_input" term_input

let term_output u = 
  let ch = Istate.Outchannel.get() in
  Term.pp ch (Value.to_term u); Value.tt
let _ = Callback.register "term_output" term_output

let term_mk_var u = 
  Value.of_term (Term.mk_var (Value.to_name u))
let _ = Callback.register "term_mk_var" term_mk_var

let rec term_mk_app u v =
  let f = Value.to_funsym u in
  let args = Value.to_list Kind.mk_term v in 
    Value.of_term (Term.sigma (Value.to_funsym u) args)
let _ = Callback.register "term_mk_app" term_mk_app

let term_funsym_of u =
  Value.of_funsym (Term.sym_of (Value.to_term u))
let _ = Callback.register "term_funsym_of" term_funsym_of

let term_args_of u =
  Value.of_list Kind.mk_term (Term.Args.to_list (Term.args_of (Value.to_term u)))
let _ = Callback.register "term_args_of" term_args_of

(** {6 Derived Term Constructors} *)

let term_mk_num u = 
  Value.of_term (Linarith.mk_num (Value.to_rat u))
let _ = Callback.register "term_mk_num" term_mk_num

let term_mk_multq u v = 
  Value.of_term (Linarith.mk_multq (Value.to_rat u) (Value.to_term v))
let _ = Callback.register "term_mk_multq" term_mk_multq

let term_mk_add u v = 
  Value.of_term (Linarith.mk_add (Value.to_term u) (Value.to_term v))
let _ = Callback.register "term_mk_add" term_mk_add

let term_mk_mult u v = 
  Value.of_term (Nl.Nonlin.mk_mult (Value.to_term u) (Value.to_term v))
let _ = Callback.register "term_mk_mult" term_mk_mult


(** {6 Constraints} *)

let is_cnstrnt u = Value.of_bool (Value.is_kind Kind.mk_cnstrnt u)
let _ = Callback.register "is_cnstrnt" is_cnstrnt

let cnstrnt_int () = Value.of_cnstrnt Cnstrnt.Int
let _ = Callback.register "cnstrnt_int" cnstrnt_int

let cnstrnt_real () = Value.of_cnstrnt Cnstrnt.Real
let _ = Callback.register "cnstrnt_real" cnstrnt_real

let cnstrnt_nonint () = Value.of_cnstrnt Cnstrnt.Nonint
let _ = Callback.register "cnstrnt_nonint" cnstrnt_nonint

let cnstrnt_bv u = 
  Value.of_cnstrnt (Cnstrnt.Bitvector(Value.to_int u))
let _ = Callback.register "cnstrnt_bv" cnstrnt_bv

  
(** {6 Atoms} *)

let is_atom u = Value.of_bool (Value.is_kind Kind.mk_atom u)
let _ = Callback.register "is_atom" is_atom

let atom_of_name u = 
  let s = Name.to_string (Value.to_name u) in
  let lb = Lexing.from_string s in 
  let atm = Parser.atomeof Lexer.token lb in
    Value.of_atom atm
let _ = Callback.register "atom_of_name" atom_of_name

let atom_to_name u = 
  Value.of_name (Name.of_string (Pretty.to_string Atom.pp (Value.to_atom u)))
let _ = Callback.register "atom_to_name" atom_to_name

let atom_mk_equal u v = 
  Value.of_atom (Atom.mk_equal (Value.to_atom u) (Value.to_atom v))  (* CHANGE THIS!!! *)
let _ = Callback.register "atom_mk_equal" atom_mk_equal  

let atom_mk_diseq u v =  
  Value.of_atom (Atom.mk_diseq (Value.to_atom u) (Value.to_atom v))
let _ = Callback.register "atom_mk_diseq" atom_mk_diseq

let atom_mk_true () = 
  Value.of_atom Atom.mk_true
let _ = Callback.register "atom_mk_true" atom_mk_true

let atom_mk_false () =  
  Value.of_atom Atom.mk_false
let _ = Callback.register "atom_mk_false" atom_mk_false

let atom_mk_lt u v =
  Value.of_atom (Linarith.Atom.mk_lt (Value.to_term u) (Value.to_term v))
let _ = Callback.register "atom_mk_lt"  atom_mk_lt

let atom_mk_le u v = 
  Value.of_atom (Linarith.Atom.mk_le (Value.to_term u) (Value.to_term v))
let _ = Callback.register "atom_mk_le"  atom_mk_le

let atom_mk_gt u v =
  Value.of_atom (Linarith.Atom.mk_gt (Value.to_term u) (Value.to_term v))
let _ = Callback.register "atom_mk_gt" atom_mk_gt

let atom_mk_ge u v = 
  Value.of_atom (Linarith.Atom.mk_ge (Value.to_term u) (Value.to_term v))
let _ = Callback.register "atom_mk_ge" atom_mk_ge

let atom_negate u = Value.of_atom (Atom.negate Linarith.mk_neg (Value.to_atom u))
let _ = Callback.register "atom_negate" atom_negate


(** {6 Propositions} *)

let is_prop u = Value.of_bool (Value.is_kind Kind.mk_prop u)
let _ = Callback.register "is_atom" is_atom

let prop_of_name u = 
  let s = Name.to_string (Value.to_name u) in
  let lb = Lexing.from_string s in 
  let p = Parser.propeof Lexer.token lb in
    Value.of_prop p
let _ = Callback.register "prop_of_name" prop_of_name

let prop_to_name u = 
  Value.of_name (Name.of_string (Pretty.to_string Prop.pp (Value.to_prop u)))
let _ = Callback.register "prop_to_name" prop_to_name
 
let prop_mk_true () = Value.of_prop Prop.mk_true
let _ = Callback.register "prop_mk_true" prop_mk_true

let prop_mk_false () = Value.of_prop Prop.mk_false
let _ = Callback.register "prop_mk_false" prop_mk_false

let prop_mk_var u = Value.of_prop (Prop.mk_var (Value.to_name u))
let _ = Callback.register "prop_mk_var" prop_mk_var

let prop_mk_poslit u = Value.of_prop (Prop.mk_poslit (Value.to_atom u))
let _ = Callback.register "prop_mk_poslit" prop_mk_poslit

let prop_mk_neglit u = Value.of_prop (Prop.mk_neglit (Value.to_atom u))
let _ = Callback.register "prop_mk_neglit" prop_mk_neglit

let prop_mk_ite u v w = 
  Value.of_prop (Prop.mk_ite (Value.to_prop u) (Value.to_prop v) (Value.to_prop w))
let _ = Callback.register "prop_mk_ite" prop_mk_ite

let prop_mk_conj u = 
  Value.of_prop (Prop.mk_conj (Value.to_list Kind.mk_prop u))
let _ = Callback.register "prop_mk_conj" prop_mk_conj

let prop_mk_disj u = 
  Value.of_prop (Prop.mk_disj (Value.to_list Kind.mk_prop u))
let _ = Callback.register "prop_mk_disj" prop_mk_disj

let prop_mk_iff u v = 
  Value.of_prop (Prop.mk_iff (Value.to_prop u) (Value.to_prop v))
let _ = Callback.register "prop_mk_iff" prop_mk_iff
 
let prop_mk_neg u = 
  Value.of_prop (Prop.mk_neg (Value.to_prop u))
let _ = Callback.register "prop_mk_neg" prop_mk_neg




(** {6 Justifications} *)

let is_justification u = Value.of_bool (Value.is_kind Kind.mk_justification u)
let _ = Callback.register "is_justification" is_justification

let justification_to_atoms u =
  Value.of_list Kind.mk_atom (Atom.Set.to_list (failwith "to do"))
          (* (Jst.axioms_of (Value.to_justification u))) *)
let _ = Callback.register "justification_to_atoms" justification_to_atoms


(** {6 Logical contexts} *)

let is_context u =  Value.of_bool (Value.is_kind Kind.mk_context u)
let _ = Callback.register "is_context" is_context

let context_empty () =
  license_check(); 
  Value.of_context Context.empty
let _ = Callback.register "context_empty" context_empty

let context_ctxt_of u = 
  Value.of_list Kind.mk_atom (Context.ctxt (Value.to_context u))
let _ = Callback.register "context_ctxt_of" context_ctxt_of


let context_use u v w = 
  let th = Value.to_theory u
  and c = Value.to_context v 
  and x = Value.to_term w in
  let ys = failwith "to do" (* Combine.Config.Component.dep th (Context.config c) x  *)in
  let yl = failwith "to do" (* Term.Vset.to_list ys *) in
   failwith "to do"
(*
    Value.of_list Kind.mk_term (Term.Vset.to_list ys)
*)

let _ = Callback.register "context_use" context_use

let context_inv u v =
  let c = Value.to_context u and a = Value.to_term v in
  let cfg = Context.config c in
    Value.of_term (Combine.Config.Inv.lookup cfg a)

let _ = Callback.register "context_inv" context_inv

let context_find u v w = 
  let th = Value.to_theory u 
  and c = Value.to_context v 
  and a = Value.to_term w in
    Value.of_term (Combine.Config.Find.lookup th (Context.config c) a)
let _ = Callback.register "context_find" context_find

let context_occ u v w =
  Value.of_bool 
    (Combine.Config.occ
       (Value.to_theory u) 
       (Context.config (Value.to_context v))
       (Value.to_term w))
let _ = Callback.register "context_occ" context_occ

(** Processing of new equalities. *)

let is_status u = Value.of_bool (Value.is_kind Kind.mk_status u)
let _ = Callback.register "is_status" is_status

let is_consistent u = 
  Value.of_bool (match Value.to_status u with Status.Ok _ -> true | _ -> false)
let _ = Callback.register "is_consistent" is_consistent

let is_redundant u = 
  Value.of_bool (match Value.to_status u with Status.Valid _ -> true | _ -> false)
let _ = Callback.register "is_redundant" is_redundant

let is_inconsistent u = 
  Value.of_bool (match Value.to_status u with Status.Unsat _ -> true | _ -> false)
let _ = Callback.register "is_inconsistent" is_inconsistent  

let d_consistent u =
  match Value.to_status u with
    | Status.Ok s -> Value.of_context s
    | _ -> raise Not_found

let _ = Callback.register "d_consistent" d_consistent 

let d_valid u =
  match Value.to_status u with
    | Status.Valid(j) -> Value.of_justification j
    | _ -> raise Not_found
let _ = Callback.register "d_valid" d_valid

let d_inconsistent u =
  match Value.to_status u with
    | Status.Unsat(j) -> Value.of_justification j
    | _ -> raise Not_found
let _ = Callback.register "d_inconsistent" d_inconsistent

let process u v =
  let s = Value.to_context u 
  and atm = Value.to_atom v in
    let result = 
      try
	let s' = Context.add s atm in
	  Status.Ok(s')
      with
	| Judgement.Unsat(rho) -> Status.Unsat(rho)
	| Judgement.Valid(rho) -> Status.Valid(rho)
    in
      Value.of_status result

let _ = Callback.register "process" process   


let prop_sat u v =
  let s = Value.to_context u and p = Value.to_prop v in
    match Prop.sat s p with
      | None -> Value.ff
      | Some(rho, _) -> Value.tt
let _ = Callback.register "prop_sat" prop_sat


(** Eval-Print Loops. *)

let cmd_rep = Cmd.rep
let _ = Callback.register "cmd_rep" cmd_rep

let cmd_batch () = 
  let ch = Istate.Inchannel.get () in
    Value.of_int (Cmd.batch ch)
let _ = Callback.register "cmd_batch" cmd_batch


let can u v = 
  let s = Value.to_context u and a = Value.to_term v in
    Value.of_term (Combine.Config.Can.term (Context.config s) a)
let _ = Callback.register "can" can


(** {6 Parameters} *)

let set u v = 
  Ref.set (Value.to_name u) (Name.to_string (Value.to_name v))
let _ = Callback.register "set" set
 
let get u = 
  Value.of_name (Name.of_string (Ref.get (Value.to_name u)))
let _ = Callback.register "set" set

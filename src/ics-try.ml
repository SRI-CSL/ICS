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


(** {6 Values and Kinds} *)

type top
  (** Encompassing all Ocaml objects. *)

module Kind = struct

  (** Open-ended type of ground kinds. *)
  module Ground = struct

    type methods = { 
      eq : top -> top -> bool; 
      pp : Format.formatter -> top -> unit
    }

    (** Bindings [n -> (eq, pp)] for defining ground types. *)
    let table = Name.Hash.create 9

    let mem g = Name.Hash.mem table g

    let to_string g =
      if mem g then Name.to_string g else    
	api_error ("No such kind: " ^ Name.to_string g)

    let get g = 
      try Name.Hash.find table g with Not_found -> 
	api_error ("No such kind: " ^ Name.to_string g)

    let eq g = (get g).eq
    let pp g = (get g).pp
 
    module Register(K: GROUND): sig end = struct

      let name = Name.of_string K.name

      let inj (k: K.t) =
	let (obj: top) = Obj.magic k in
	  obj

      let out (obj: top) = 
	let (k: K.t) = Obj.magic gnd in
	  k

      let eq (gnd1: t) (gnd2: t) = 
	K.eq (out gnd1) (out gnd2)

      let pp fmt (gnd: t) =
	K.pp fmt (out gnd)

      let _ =
	if Name.Hash.mem table name then
	  invalid_arg ("Duplicate kind entry: " ^ K.name)
	else
	  Name.Hash.add table name { eq = eq; pp = pp }
    end 

    module Int = Register(
      struct
	let name = "int"
	type t = int
	let eq = (=)
	let pp fmt i = Format.fprintf fmt "%d" i
      end)

    module Name = Register(
      struct
	let name = "name"
	type t = Name.t
	let eq = Name.eq
	let pp = Name.pp
      end)

  end

  type t =
    | Ground of Name.t
    | Pair of t * t
    | Triple of t * t * t
    | Option of t
    | List of t

  let mk_ground g =
    if Ground.mem g then Ground(g) else 
      api_error ("No such kind: " ^ Name.to_string g)

  let mk_name = mk_ground (Name.of_string "name")
  let mk_int = mk_ground (Name.of_string "int")

  let mk_pair k l = Pair(k, l)

  let mk_triple k l m = Triple(k, l, m)

  let mk_option k = Option(k)

  let mk_list k = List(k)

  let rec to_string = function
    | Ground(g) -> 
	Ground.to_string g
    | Pair(k1, k2) -> 
	Format.sprintf "(%s * %s)" (to_string k1) (to_string k2)
    | Triple(k1, k2, k3) -> 
	Format.sprintf "(%s * %s * %s)" (to_string k1) (to_string k2) (to_string k3)
    | Option(k) -> 
	Format.sprintf "%s option" (to_string k)
    | List(k) -> 
	Format.sprintf "%s list" (to_string k)

  let compatible = (=)

  let pp fmt k =
    Format.fprintf fmt "%s" (to_string k)

  let rec eval k (obj: all) =
    match k with
      | Ground(g) -> failwith"to do"
      | Pair(l1, l2) ->
	  let (x, y) = obj in
	    (eval l1 x, eval l2 y)
      | Triple(l1, l2, l3) ->
	  let (x, y, z) = obj in
	    (eval l1 x, eval l2 y, eval l3 z)
      | Option(l) -> 
	  (match obj with
	     | None -> None
	     | Some(x) -> Some(eval l x))
      | List(l) ->
	  List.map (eval l) l
	  
	  
    

end 

open Kind

(** For each index there is a corresponding value and kind. *)
module Heap = struct

  let initial_heap_size = 1024   (* does not work for 2 *)

   (** Tables
     - a {i heap} maps indices [i] to values of type [Type.t].
     - {i free} contains indices to unregistered entries.
     - {i top} always points to a free index. *)
  let value = ref (Array.make initial_heap_size (Obj.magic 0))
  let kind = ref (Array.make initial_heap_size (Obj.magic 0))
  let free = ref Ptset.empty
  let top = ref 0

  let init () = 
    value := Array.make initial_heap_size (Obj.magic 0);
    kind := Array.make initial_heap_size ((Obj.magic 0): Kind.t);
    free := Ptset.empty;
    top := 0
   
  let _ = Tools.add_at_reset init

  let is_free i = 
    Ptset.mem i !free

  let length () = 
    assert(Array.length !value = Array.length !kind);  
    Array.length !value 

  let is_registered i =
    not(is_free i) && i < Array.length !value

  let rec next_free () = 
    try
      let i = Ptset.choose !free in
	free := Ptset.remove i !free;
	i
    with
	Not_found ->
	  if !top >= Array.length !value then
	    resize ();
	  assert(!top < Array.length !value); 
	  incr top;
	  !top

  and resize () =
    let l' = 2 * Array.length !value in
    let value' = Array.create l' (Obj.magic 0) in
    let kind' = Array.create l' (Obj.magic 0) in
      value := Array.append !value value';
      kind := Array.append !kind kind'

  let register (knd: Kind.t) obj =
    let idx = next_free () in
      Array.unsafe_set !value idx (Obj.magic obj);
      Array.unsafe_set !kind idx knd;
      Trace.msg 2 "Ics.Heap.register" idx Pretty.number;
      idx
    
  let get idx =
    assert(is_registered idx);
    Array.unsafe_get !value idx

  let kind_of idx =
    Trace.msg 2 "Ics.Heap.kind_of" idx Pretty.number;
    assert(is_registered idx);
    Array.unsafe_get !kind idx 

  let deregister idx = 
    assert(is_registered idx);
    free := Ptset.add idx !free;
    assert(idx < Array.length !value);
    Array.unsafe_set !value idx (Obj.magic 0)

  let iter f = 
    for i = 0 to !top do
      if not(is_free i) then
	f (Array.unsafe_get !value i)
    done
    
  let to_list s = 
    let l = ref [] in
      for i = 0 to !top do
	if not(is_free i) then
	  l := Array.unsafe_get !value i :: !l
      done;
      !l
    
end

(** Realizes an indirection of {i nonce} indices as exported to an 
  index in the heap. *)
module Value = struct

  type t = int

  let current = ref 2

  let nonce () = 
    assert(!current < max_int);
    incr current;
    current

  module Table = Hashtbl.Make(
    struct
      type t = int
      let equal = (==)
      let hash i = i
    end)

  let table = Table.create 107
 
  let init () =
    current := 2;
    Table.clear table

  let _ = Tools.add_at_reset init

  let nil: t = 0
  let tt : t = 1

  let mem (v: t) =
    Table.mem table v

  let get (knd: Kind.t) (v: t) =
    Trace.msg 2 "Ics.get" knd Kind.pp;
    try
      let idx = Table.find table v in
      let found_knd = Heap.kind_of idx in
	if Kind.compatible knd found_knd then
	  Obj.magic (Heap.get idx)
	else 
	  api_error (Format.sprintf "value %d is of kind %s but kind %s expected" v 
		       (Kind.to_string found_knd) 
		       (Kind.to_string knd))
    with
	Not_found -> 
	  api_error (Format.sprintf "value %d not registered@." v)

  let kind_of (v: t) =
    try
      let idx = Table.find table v in
	Heap.kind_of idx
    with
	Not_found -> 
	  api_error (Format.sprintf "value %d not registered@." v)

  let untyped_get (v: t) =
    try
      let idx = Table.find table v in
	Obj.magic (Heap.get idx)
    with
	Not_found -> 
	  api_error (Format.sprintf "value %d not registered@." v)

  let make (knd: Kind.t) obj =
    let idx = Heap.register knd obj in
    let nonce = !current in
      Table.add table nonce idx;
      incr current;
      nonce

  let remove (v: t) = 
    try
      let idx = Table.find table v in
	Table.remove table idx;
	Heap.deregister idx
    with
	Not_found -> 
	  api_error (Format.sprintf "value %d not registered@." v)

  let of_string s = make Kind.mk_name (Name.of_string s)
  let of_int i = make Kind.mk_int i
  let of_bool = function true -> tt | false -> nil


end

(** {6 Values} *)

type value = Value.t

let nil () = Value.nil
let _ = Callback.register "nil" nil

let t () = Value.tt
let _ = Callback.register "t" t

let of_string = Value.of_string
let _ = Callback.register "of_string" of_string

let of_int = Value.of_int
let _ = Callback.register "of_int" of_int

let of_bool = Value.of_bool
let _ = Callback.register "of_bool" of_bool

let kind_of v =
  let k = Value.kind_of v in
    Value.make Kind.mk_name (Name.of_string (Kind.to_string k))
let _ = Callback.register "kind_of" kind_of

let is_nil = function 0 -> true | _ -> false
let _ = Callback.register "is_nil" is_nil

let is_t = function 1 -> true | _ -> false
let _ = Callback.register "is_t" is_t

let is_registered u = of_bool (Value.mem u)
let _ = Callback.register "is_registered" is_registered

let deregister = Value.remove 
let _ = Callback.register "deregister" deregister


let pp v = 
  let fmt = Format.std_formatter in
    match Value.kind_of v with
      | Kind.Ground(n) -> 
	  failwith "to do"
	  (* Kind.Ground.pp n fmt (Value.untyped_get v) *)
      | Kind.Pair(k, l) -> ()
      | Kind.Triple(k, l, m) -> ()
      | Kind.Option(k) -> ()
      | Kind.List(k) -> ()
let _ = Callback.register "pp" pp

let eq u v = 
  of_bool(
    u == v ||
    let k = Value.kind_of u in
    let l = Value.kind_of v in
      Kind.compatible k l && Value.get k u == Value.get l v)
let _ = Callback.register "eq" eq

let equal u1 u2 = 
  let rec eq u1 u2 =
    let k1 = Value.kind_of u1 and k2 = Value.kind_of u2 in
      match k1, k2 with
	| Ground(n1), Ground(n2) -> 
	    Name.eq n1 n2 &&
	    Ground.eq n1 (Value.get k1 u1) (Value.get k2 u2)
	| Pair(k1, l1), Pair(k2, l2) -> 
	    failwith "to do"
	| Triple(k1, l1, m1), Triple(k2, l2, m2) ->
	    failwith "to do"
	| Option(k1), Option(k2) -> 
	    failwith "to do"
	| List(k1), List(k2) -> 
	    failwith "to do"
	| _ ->
	    false
  in
    of_bool (eq u1 u2)
let _ = Callback.register "equal" equal

(*
let to_bool u = not(is_nil u)
let to_string u = Name.to_string (Value.get Kind.Ground.mk_name u)
let to_int u = Value.get Kind.Int u
let to_q u = Value.get Kind.Q u
*)

  
module Make(Ground: GROUND) = struct
  
  module Unit = Kind.Ground.Register(Ground)

  let ground = Kind.mk_ground (Name.of_string Ground.name)

  let is (v: Value.t) = 
    Kind.compatible ground (Value.kind_of v)
      
  let of_value (v: Value.t) = 
    Value.get ground v

  let to_value (obj: Ground.t) =
    Value.make ground obj

end

(** {6 Names} *)

module Name = Make(
  struct
    let name = "name"
    type t = Name.t
    let eq = Name.eq
    let pp = Name.pp
  end)

let is_name u = of_bool (Name.is u)
let _ = Callback.register "is_name" is_name

let name_of_string u = of_name (Name.of_string 
let _ = Callback.register "name_of_string" name_of_string

let name_to_string v = Name.to_string (to_name v)
let _ = Callback.register "name_to_string" name_to_string

let name_eq n m = of_bool (Name.eq (to_name n) (to_name m))
let _ = Callback.register "name_eq" name_eq


(** {6 Options} *)

let is_some u =
  let res = match Value.kind_of u with
    | Option _ -> 
	let p = Value.untyped_get u in
	  (match p with Some _ -> true | None -> false)
    | _ ->
	false
  in
    of_bool res
  
let is_none u = 
  of_bool 
    (match Value.kind_of u with
       | Option(k) -> 
	   (match Value.get k u with Some _ -> true | None -> false)
       | _ ->
	   false)

let value_of u = 
 match Value.kind_of u with
    | Option(k) -> 
	let p = Value.untyped_get u in
	  (match p with
	     | Some(x) -> Value.make k x
	     | None -> api_error (Format.sprintf "value %d does not represent a 'some' option" u))
    | _ ->
	api_error (Format.sprintf "value %d not of kind 'option'" u)
	
let _ = Callback.register "is_some" is_some
let _ = Callback.register "is_none" is_none
let _ = Callback.register "value_of" value_of



(** {6 Pairs} *)

let is_pair u =
  let res = 
    match Value.kind_of u with
      | Pair _ -> true
      | _ -> false
  in
    of_bool res
let _ = Callback.register "is_pair" is_pair

let pair u v = 
  let k = Value.kind_of u in
  let x = Value.get k u in
  let l = Value.kind_of v in
  let y = Value.get l v in
    Value.make (Kind.mk_pair k l) (x, y)
let _ = Callback.register "pair" pair

let fst u =
  match Value.kind_of u with
    | (Pair(k, _) as knd) ->
	let (x, _) = Value.get knd u in
	  Value.make k x
    | _ -> 
	api_error(Format.sprintf "value %d not a pair" u)
let _ = Callback.register "fst" fst

let snd u = 
  match Value.kind_of u with
    | (Pair(_, l) as knd) ->
	let (_, y) = Value.get knd u in
	  Value.make l y
    | _ -> 
	api_error(Format.sprintf "value %d not a pair" u)
let _ = Callback.register "snd" snd


(** {6 Lists} *)

let is_list u = 
  let rec loop v =
    is_nil v ||
    match Value.kind_of v with
	| (Pair(l, _) as knd) -> 
	    let (_, tl) = Value.get knd v in
	      loop tl    
	| _ -> 
	    false
  in
    of_bool (loop u)
  
let is_cons = is_pair

let cons = pair
let head = fst
let tail = snd

let of_list k al =
  List.fold_right
    (fun a acc -> Value.make (Kind.mk_pair k k) (a, acc))
    al (nil())

let to_list k u =
  let rec loop acc v =
    if is_nil v then acc else
      match Value.kind_of v with
	| (Pair(l1, l2) as knd) when Kind.compatible l1 k && Kind.compatible l2 k -> 
	    let (hd, tl) = Value.get knd v in
	      loop (hd :: acc) tl
	| _ -> 
	    api_error(Format.sprintf "value %d not a list of kind %s" u (Kind.to_string k))
  in
    List.rev (loop [] u)
       

let _ = Callback.register "is_nil" is_nil
let _ = Callback.register "is_list" is_list
let _ = Callback.register "nil" nil
let _ = Callback.register "cons" cons
let _ = Callback.register "head" head
let _ = Callback.register "tail" tail



(** {6 Triples} *)

let is_triple u = 
 of_bool (match Value.kind_of u with Triple _ -> true | _ -> false)
let _ = Callback.register "is_triple" is_triple

let triple u v w =
 let k = Value.kind_of u and x = Value.untyped_get u in
 let l = Value.kind_of v and y = Value.untyped_get v in
 let m = Value.kind_of w and z = Value.untyped_get w in
    Value.make (Kind.mk_triple k l m) (x, y, z)

let fst_of_triple u =  
  match Value.kind_of u with
    | Triple(k, _, _) ->
	let (x, _, _) = Value.untyped_get u in
	  Value.make k x
    | _ -> 
	api_error(Format.sprintf "value %d not a triple" u)

let snd_of_triple u =  
  match Value.kind_of u with
    | Triple(_, l, _) ->
	let (_, y, _) = Value.untyped_get u in
	  Value.make l y
    | _ -> 
	api_error(Format.sprintf "value %d not a triple" u)

let third_of_triple u =  
  match Value.kind_of u with
    | Triple(_, _, m) ->
	let (_, _, z) = Value.untyped_get u in
	  Value.make m z
    | _ -> 
	api_error(Format.sprintf "value %d not a triple" u)

let _ = Callback.register "triple" triple
let _ = Callback.register "fst_of_triple" fst_of_triple
let _ = Callback.register "snd_of_triple" snd_of_triple
let _ = Callback.register "third_of_triple" third_of_triple



(** {6 Parameters} *)

let set u v = 
  Ref.set (to_name u) (to_string v); t()
let _ = Callback.register "set" set
 
let get u = of_string (Ref.get (to_name u))
let _ = Callback.register "set" set

 
(** {6 Multi-precision arithmetic} *)

let is_num u =
  of_bool (match Value.kind_of u with Q -> true | _ -> false)
let _ = Callback.register "is_num" is_num

let ints_of_num v = 
  let q = Value.get Q v in
  let n = Mpa.Z.to_string (Mpa.Q.numerator q) in
  let m = Mpa.Z.to_string (Mpa.Q.denominator q) in
    Value.make 
      (mk_pair Name Name) 
      (Name.of_string n, Name.of_string m)
let _ = Callback.register "ints_of_num" ints_of_num

let num_of_int n = 
  let q = Mpa.Q.of_int n in
    Value.make Q q
let _ = Callback.register "num_of_int" num_of_int
		   
let num_of_ints u v = 
  let q = Mpa.Q.div (Mpa.Q.of_int u) (Mpa.Q.of_int v) in
    Value.make Q q
let _ = Callback.register "num_of_ints" num_of_ints

let string_of_num v =
  let q = Value.get Q v in
    Mpa.Q.to_string q
let _ = Callback.register "string_of_num" string_of_num

let num_of_string v = 
  let str = to_string v in
  let q = Mpa.Q.of_string str in
    Value.make Q q
let _ = Callback.register "num_of_string" num_of_string


(** {6 Domains} *)

let is_dom u = of_bool (Kind.compatible Dom (Value.kind_of u))
let _ = Callback.register "is_dom" is_dom

let dom_mk_int () = Value.make Dom Dom.Int
let _ = Callback.register "dom_mk_int" dom_mk_int

let dom_mk_real () = Value.make Dom Dom.Real
let _ = Callback.register "dom_mk_real" dom_mk_real

let dom_mk_nonint () = Value.make Dom Dom.Nonint
let _ = Callback.register "dom_mk_nonint" dom_mk_nonint

let dom_is_int v = of_bool (Value.get Dom v = Dom.Int)
let _ = Callback.register "dom_is_int" dom_is_int

let dom_is_real v = of_bool (Value.get Dom v = Dom.Real)
let _ = Callback.register "dom_is_real" dom_is_real

let dom_is_nonint v = of_bool (Value.get Dom v = Dom.Nonint)
let _ = Callback.register "dom_is_nonint" dom_is_nonint


(** {6 Constraints} *)

let is_cnstrnt u = of_bool (Kind.compatible Kind.Cnstrnt (Value.kind_of u))
let _ = Callback.register "is_cnstrnt" is_cnstrnt

let to_cnstrnt = Value.get Kind.Cnstrnt
let of_cnstrnt = Value.make Kind.Cnstrnt



(** {6 Theories} *)

let is_th u = of_bool (Kind.compatible Theory (Value.kind_of u))
let _ = Callback.register "is_th" is_th

let of_theory = Value.make Kind.Theory
let to_theory = Value.get Kind.Theory

let th_to_string u = 
  let i = to_theory u in
    Value.make Name (Name.of_string (Theory.to_string i))
let _ = Callback.register "th_to_string" th_to_string

let th_of_string u = of_theory (Theory.of_string (to_string u))
let _ = Callback.register "th_of_string" th_of_string

let th_description u = 
  let str = Theory.Description.get (to_theory u) in
     Value.make Name (Name.of_string str)
let _ = Callback.register "th_description" th_description

let th_pp u = 
  Theory.pp Format.std_formatter (to_theory u);
  Format.fprintf Format.std_formatter "@;"
let _ = Callback.register "th_pp" th_pp


(** {6 Function Symbols} *)

let is_sym u = of_bool (Kind.compatible Kind.Funsym (Value.kind_of u))
let _ = Callback.register "is_sym" is_sym
  
let to_sym = Value.get Kind.Funsym
let of_sym = Value.make Kind.Funsym

let sym_make u v =
  let i = to_theory u and str = to_string v in
    of_sym (Funsym.create i (Name.of_string str))
let _ = Callback.register "sym_make" sym_make

let sym_eq u v = of_bool (Funsym.eq (to_sym u) (to_sym v))
let _ = Callback.register "sym_eq" sym_eq

let sym_cmp u v = of_int (Funsym.cmp (to_sym u) (to_sym v))
let _ = Callback.register "sym_cmp" sym_cmp

let sym_theory_of f = of_theory (Funsym.theory_of (to_sym f))
let _ = Callback.register "sym_theory_of" sym_theory_of

let sym_name_of u = 
  Value.of_string (Name.to_string (Funsym.name_of (to_sym u)))
let _ = Callback.register "sym_name_of" sym_name_of


(** {6 Terms} *)

let is_term u = of_bool (Kind.compatible Term (Value.kind_of u))
let _ = Callback.register "is_term" is_term

let to_term u = Value.get Kind.Term u
let of_term a = Value.make Kind.Term a

let to_term_list = to_list Kind.Term

let term_of_string u =
  let s = Name.to_string (Value.get Name u) in
  let lb = Lexing.from_string s in 
    of_term (Parser.termeof Lexer.token lb)
let _ = Callback.register "term_of_string" term_of_string

let term_to_string u = 
  Value.of_string (Pretty.to_string Term.pp (to_term u))
let _ = Callback.register "term_to_string" term_to_string

let term_input u =
  let ch = Value.get InChannel u in
  let lb = Lexing.from_channel ch in 
    of_term (Parser.termeof Lexer.token lb)
let _ = Callback.register "term_input" term_input

let term_output u v = 
  Term.pp (Value.get OutChannel u) (to_term v)
let _ = Callback.register "term_output" term_output

let term_pp u =
  let a = Value.get Term u in
    Term.pp Format.std_formatter a; Format.print_flush ()
let _ = Callback.register "term_pp" term_pp

let term_mk_var u = 
  of_term (Term.mk_external_var (to_string u))
let _ = Callback.register "term_mk_var" term_mk_var

let term_mk_app u v =
  of_term (Term.sigma (to_sym u) (to_term_list v))
let _ = Callback.register "term_mk_app" term_mk_app

  
(** {6 Atoms} *)

let is_atom u = of_bool (Kind.compatible Kind.Atom (Value.kind_of u))
let _ = Callback.register "is_atom" is_atom

let to_atom u = Value.get Kind.Atom u
let of_atom a = Value.make Kind.Atom a

let atom_pp u = 
  Atom.pp Format.std_formatter (to_atom u);
  Format.print_flush ()
let _ = Callback.register "atom_pp" atom_pp

let atom_of_string u = 
  let s = to_string u in
  let lb = Lexing.from_string s in 
  let atm = Parser.atomeof Lexer.token lb in
    of_atom atm
let _ = Callback.register "atom_of_string" atom_of_string

let atom_to_string u = Pretty.to_string Atom.pp (to_atom u)
let _ = Callback.register "atom_to_string" atom_to_string

let atom_mk_equal u v = of_atom (Atom.mk_equal (to_atom u, to_atom v))
let _ = Callback.register "atom_mk_equal" atom_mk_equal  

let atom_mk_diseq u v =  of_atom (Atom.mk_diseq (to_atom u, to_atom v))
let _ = Callback.register "atom_mk_diseq" atom_mk_diseq

let atom_mk_true () = of_atom Atom.mk_true
let _ = Callback.register "atom_mk_true" atom_mk_true

let atom_mk_false () =  of_atom Atom.mk_false
let _ = Callback.register "atom_mk_false" atom_mk_false

let atom_mk_lt a b = failwith "atom:mk_lt: to do"
let _ = Callback.register "atom_mk_lt"  atom_mk_lt

let atom_mk_le u v = 
  of_atom (Atom.mk_ineq (Linarith.mk_sub (to_term v) (to_term u), Linarith.mk_zero()))
let _ = Callback.register "atom_mk_le"  atom_mk_le

let atom_mk_gt a b = failwith "atom:mk_gt: to do"
let _ = Callback.register "atom_mk_gt" atom_mk_gt

let atom_mk_ge u v = 
  of_atom (Atom.mk_ineq (Linarith.mk_sub (to_term u) (to_term v), Linarith.mk_zero()))
let _ = Callback.register "atom_mk_ge" atom_mk_ge


let atom_negate u = of_atom (Atom.negate Linarith.mk_neg (to_term u))
let _ = Callback.register "atom_negate" atom_negate



(** {6 Propositions} *)


let is_prop u = of_bool (Kind.compatible Kind.Prop (Value.kind_of u))
let _ = Callback.register "is_atom" is_atom

let to_prop u = Value.get Kind.Prop u
let of_prop a = Value.make Kind.Prop a

let to_prop_list u = failwith "to do"

let prop_of_string u =
  let lb = Lexing.from_string (to_string u) in 
  let p = Parser.propeof Lexer.token lb in
    of_prop p
let _ = Callback.register "prop_of_string" prop_of_string

let prop_to_string u = 
  Value.of_string (Pretty.to_string Prop.pp (to_prop u))
let _ = Callback.register "prop_to_string" prop_to_string

let prop_pp u = Prop.pp Format.std_formatter (to_prop u)
let _ = Callback.register "prop_pp" prop_pp
 
let prop_mk_true () = of_prop Prop.mk_true
let _ = Callback.register "prop_mk_true" prop_mk_true

let prop_mk_false () = of_prop Prop.mk_false
let _ = Callback.register "prop_mk_false" prop_mk_false

let prop_mk_var u = of_prop (Prop.mk_var (to_name u))
let _ = Callback.register "prop_mk_var" prop_mk_var

let prop_mk_poslit u = of_prop (Prop.mk_poslit (to_atom u))
let _ = Callback.register "prop_mk_poslit" prop_mk_poslit

let prop_mk_neglit u = of_prop (Prop.mk_neglit (to_atom u))
let _ = Callback.register "prop_mk_neglit" prop_mk_neglit

let prop_mk_ite u v w = of_prop (Prop.mk_ite (to_prop u) (to_prop v) (to_prop w))
let _ = Callback.register "prop_mk_ite" prop_mk_ite

let prop_mk_conj u = of_prop (Prop.mk_conj (to_prop_list u))
let _ = Callback.register "prop_mk_conj" prop_mk_conj

let prop_mk_disj u = of_prop (Prop.mk_disj (to_prop_list u))
let _ = Callback.register "prop_mk_disj" prop_mk_disj

let prop_mk_iff u v = of_prop (Prop.mk_iff (to_prop u) (to_prop v))
let _ = Callback.register "prop_mk_iff" prop_mk_iff
 
let prop_mk_neg u = of_prop (Prop.mk_neg (to_prop u))
let _ = Callback.register "prop_mk_neg" prop_mk_neg

let prop_is_true u = of_bool (try Prop.is_true (to_prop u) with _ -> false)
let _ = Callback.register "prop_is_true" prop_is_true

let prop_is_false u = of_bool (try Prop.is_false (to_prop u) with _ -> false)
let _ = Callback.register "prop_is_false" prop_is_false

let prop_is_var u = of_bool (try Prop.is_var (to_prop u) with _ -> false)
let _ = Callback.register "prop_is_var" prop_is_var

let prop_is_atom u = of_bool (try Prop.is_atom (to_prop u) with _ -> false)
let _ = Callback.register "prop_is_atom" prop_is_atom

let prop_is_ite u = of_bool (try Prop.is_ite (to_prop u) with _ -> false)
let _ = Callback.register "prop_is_ite" prop_is_ite

let prop_is_disj u = of_bool (try Prop.is_disj (to_prop u) with _ -> false)
let _ = Callback.register "prop_is_disj" prop_is_disj

let prop_is_iff u = of_bool (try Prop.is_iff (to_prop u) with _ -> false)
let _ = Callback.register "prop_is_iff" prop_is_iff

let prop_is_neg u = of_bool (try Prop.is_neg (to_prop u) with _ -> false)
let _ = Callback.register "prop_is_neg" prop_is_neg

let prop_d_atom u = of_atom (Prop.d_atom (to_prop u))
let _ = Callback.register "prop_d_atom" prop_d_atom

let prop_d_var u = of_name (Prop.d_var (to_prop u))
let _ = Callback.register "prop_d_var" prop_d_var

let prop_d_ite u = of_prop (Prop.d_ite (to_prop u))
let _ = Callback.register "prop_d_ite" prop_d_ite

let prop_d_disj u = of_prop (Prop.d_disj (to_prop u))
let _ = Callback.register "prop_d_disj" prop_d_disj

let prop_d_iff u = of_prop (Prop.d_iff (to_prop u))
let _ = Callback.register "prop_d_iff" prop_d_iff

let prop_d_neg u = of_prop (Prop.d_neg (to_prop u))
let _ = Callback.register "prop_d_neg" prop_d_neg

let term_mk_mult u v = 
  of_term (Nl.Nonlin.mk_mult (to_term u) (to_term v))
let _ = Callback.register "term_mk_mult" term_mk_mult

let term_mk_create u  = of_term (Funarr.mk_create (to_term u))
let _ = Callback.register "term_mk_create" term_mk_create

let term_mk_update u v w = of_term (Funarr.mk_update (to_term u) (to_term v) (to_term w)) 
let _ = Callback.register "term_mk_update" term_mk_update

let term_mk_select u v = of_term (Funarr.mk_select (to_term u) (to_term v))
let _ = Callback.register "term_mk_select" term_mk_select

let term_mk_apply u v = of_term (Apply.mk_apply (to_term u) (to_term v))
let _ = Callback.register "term_mk_apply" term_mk_apply

let term_eq u v = of_bool (Term.eq (Value.get Term u) (Value.get Term v))
let _ = Callback.register "term_eq" term_eq

let term_cmp u v = of_int (Term.cmp (Value.get Term u) (Value.get Term v))
let _ = Callback.register "term_cmp" term_cmp

(** {6 Justifications} *)


let is_justification u = 
  of_bool (Kind.compatible Kind.Justification (Value.kind_of u))
let _ = Callback.register "is_justification" is_justification

let to_justification u = Value.get Kind.Justification u
let of_justification u = Value.make Kind.Justification u

let justification_pp u = Jst.pp Format.std_formatter (to_justification u)
let _ = Callback.register "justification_pp" justification_pp


(** {6 Logical contexts} *)

let is_context u = 
  of_bool (Kind.compatible Kind.Context (Value.kind_of u))
let _ = Callback.register "is_context" is_context

let to_context u = Value.get Kind.Context u
let of_context a = Value.make Kind.Context a

let of_term_x_jst = 
  let k = (Kind.mk_pair Kind.Term Kind.Justification) in
    Value.make k

let context_eq u v = of_bool (Context.eq (to_context u) (to_context v))
let _ = Callback.register "context_eq" context_eq  

let context_empty () =
  license_check(); 
  of_context Context.empty
let _ = Callback.register "context_empty" context_empty

let context_ctxt_of u = 
  let c = to_context u in
    of_list Kind.Atom (Context.ctxt c)
let _ = Callback.register "context_ctxt_of" context_ctxt_of

let context_use u v w = 
  let th = to_theory u and c = to_context v and x = to_term w in
  let ys = failwith "to do" (* Combine.Config.Component.dep th (Context.config c) x  *)in
  let yl = Term.Vset.to_list ys in
    of_list Kind.Term (Term.Vset.to_list ys)
let _ = Callback.register "context_use" context_use

let context_inv u v =
  let c = to_context u and a = to_term v in
  let cfg = Context.config c in
    try
      of_term_x_jst (Combine.Config.inv cfg a)
    with
	Not_found -> nil()
let _ = Callback.register "context_inv" context_inv

let context_find u v w = 
  let th = to_theory u and c = to_context v and a = to_term w in
  let cfg = Context.config c in
    of_term_x_jst (Combine.Config.find th cfg a)
let _ = Callback.register "context_find" context_find

let context_apply u v w =
  let th = to_theory u and c = to_context v and a = to_term w in
  let cfg = Context.config c in
    try
      of_term_x_jst (Combine.Config.find th cfg a)
    with
	Not_found -> nil()
let _ = Callback.register "context_apply" context_apply

let context_mem u v w = failwith "to do"
  (* of_bool (Context.mem (to_context v) (to_theory u) (to_term w)) *)
let _ = Callback.register "context_mem" context_mem

let context_pp u = 
  Context.pp Format.std_formatter (to_context u); 
  Format.print_flush()
let _ = Callback.register "context_pp" context_pp

let context_ctxt_pp u =  failwith "to do"
(*
  let s = to_context u in
  let al = s.Context.ctxt in
  let fmt = Format.std_formatter in
    List.iter
      (fun a ->
	 Pretty.string fmt "\nassert ";
	 Atom.pp fmt a;
	 Pretty.string fmt " .")
      al;
    Format.print_flush()
*)
let _ = Callback.register "context_ctxt_pp" context_ctxt_pp  

(** Processing of new equalities. *)

type status =
  | Valid of Jst.t
  | Unsat of Jst.t
  | Ok of Context.t

let is_status u = 
  of_bool (Kind.compatible Kind.Context (Value.kind_of u))
let _ = Callback.register "is_status" is_status

let of_status = Value.make Kind.Status
let to_status = Value.get Kind.Status

let is_consistent u = 
  of_bool (match to_status u with Ok _ -> true | _ -> false)
let _ = Callback.register "is_consistent" is_consistent

let is_redundant u = 
  of_bool (match to_status u with Valid _ -> true | _ -> false)
let _ = Callback.register "is_redundant" is_redundant

let is_inconsistent u = 
  of_bool (match to_status u with Unsat _ -> true | _ -> false)
let _ = Callback.register "is_inconsistent" is_inconsistent  

let d_consistent u =
  match to_status u with
    | Ok s -> of_context s
    | _ -> nil()
let _ = Callback.register "d_consistent" d_consistent 

let d_valid u =
  match to_status u with
    | Valid(j) -> of_justification j
    | _ -> nil()
let _ = Callback.register "d_valid" d_valid

let d_inconsistent u =
  match to_status u with
    | Unsat(j) -> of_justification j
    | _ -> nil()
let _ = Callback.register "d_inconsistent" d_inconsistent

let process u v =
  let s = to_context u and atm = to_atom v in
    let result = 
      try
	let s' = Context.add s atm in
	  Ok(s')
      with
	| Jst.Inconsistent(rho) -> Unsat(rho)
	| Jst.Valid(rho) -> Valid(rho)
    in
      of_status result
    
let _ = Callback.register "process" process   


let prop_sat u v =
  let s = to_context u and p = to_prop v in
  let res = match Prop.sat s p with
    | None -> None
    | Some(rho, _) -> Some(rho)
  in
    Value.make (Kind.Option Kind.Justification) res
let _ = Callback.register "prop_sat" prop_sat


(** Eval-Print Loops. *)

let cmd_rep = Cmd.rep
let _ = Callback.register "cmd_rep" cmd_rep

let cmd_batch u = 
  let ch = Value.get InChannel u in
    Cmd.batch ch
let _ = Callback.register "cmd_batch" cmd_batch


let can u v= 
  let s = to_context u and a = to_term v in
  let cfg = Context.config s in
  let rho = ref Jst.dep0 in
  let b = Combine.Config.can cfg rho a in
    of_term_x_jst (b, !rho)
let _ = Callback.register "can" can

let set_outchannel u = 
  let ch = Value.get OutChannel u in
    Istate.Outchannel.set ch
let _ = Callback.register "set_outchannel" set_outchannel

let set_inchannel u = 
  let ch = Value.get InChannel u in
    Istate.Inchannel.set ch
let _ = Callback.register "set_inchannel" set_inchannel

let set_prompt u = Cmd.Prompt.set (to_string u)
let _ = Callback.register "set_prompt" set_prompt

let set_eot u = Istate.Eot.set (to_string u)
let _ = Callback.register "set_eot" set_eot


let of_cnstrnt_x_jst = 
  let k = (Kind.mk_pair Kind.Cnstrnt Kind.Justification) in
    Value.make k

let cnstrnt u v = 
  let s = to_context u and a = to_term v in
  let cfg = Context.config s in
  let (c, rho) = failwith "to do" (* Context.cnstrnt cfg a  *) in
    of_cnstrnt_x_jst (c, rho)
let _ = Callback.register "cnstrnt" cnstrnt

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

(** A {i term} is either 
  - a term variable or 
  - an application of a function symbol to a finite sequence of arguments.  
  All terms are hash-consed, so equality reduces to identity. Hashconsing uses 
  weak hash table to avoid memory leaks. *)

type t =
  | Var of Name.t
  | App of app

and app = {
  mutable funsym : Funsym.t;
  mutable args : args;
  mutable hash : int
}

and args = t array

type trm = t  (* Synonym for [t] to avoid name clashes *)

let name_of = function 
  | Var(x) -> x
  | _ -> raise Not_found

let destruct = function
  | App(a) -> a.funsym, a.args
  | _ -> raise Not_found

let sym_of = function
  | App(a) -> a.funsym 
  | _ -> raise Not_found

let args_of = function
  | App(a) -> a.args
  | _ -> raise Not_found

let theory_of = function
  | App(a) -> Funsym.theory_of a.funsym
  | _ -> raise Not_found


(** Initially, the hash slot of applications is set to a negative number. Only
  nonnegative numbers represent meaningful hashs. *)
let hash t = 
  let args_hash a = Hashtbl.hash_param 3 3 a in
    match t with
      | Var(x) -> 
	  Name.idx x
      | App(a) -> 
	  if a.hash >= 0 then a.hash else
	    let hsh = (14007 + Funsym.hash a.funsym + args_hash a.args) land 0x3FFFFFFF in
	      a.hash <- hsh;
	      hsh

(** {i Syntactic term ordering}. [compare] 
  is faster than [cmp] and is used for building sets and maplets. *)
let fast_compare s t = 
  let hs = hash s and ht = hash t in
    if hs < ht then 
      -1
    else if hs == ht then 
      if s == t then 0 else Pervasives.compare s t
    else 
      1

(* will be set later on. Needed for simulating mutual dependency. *)
let pptmp: (Format.formatter -> t -> unit) ref = ref (Obj.magic 0)

(** Finite maps. *)
module Map = Maps.Make(
  struct
    type t = trm
    let compare = fast_compare
    let pp fmt = !pptmp fmt
  end)
  
(** Finite set of terms. *)
module Set = Sets.Make(
  struct
    type t = trm
    let compare = fast_compare
    let pp fmt = !pptmp fmt
  end)

(** {i Term arguments} are maintained as hashconsed term of arrays. *)
module Args = struct

  type t = args

  let length = Array.length

  exception Invalid
  exception Valid

  let for_all p a =
    let l = length a in
      l == 0 ||
      (try
	 for i = 1 to l - 1 do
	   if not (p (Array.unsafe_get a i)) then
	     raise Invalid
	 done;
	 true
       with
	   Invalid -> false)

  let exists p a =
    let l = length a in
      l > 0 &&
      (try
	 for i = 1 to l - 1 do
	   if p (Array.unsafe_get a i) then
	     raise Valid
	 done;
	 false
       with
	   Valid -> true)

  let for_all2 p a1 a2 =
    let l1 = length a1 and l2 = length a2 in
      l1 == l2 &&
      (l1 == 0 ||
       try
	 for i = 1 to l1 - 1 do
	   if not (p (Array.unsafe_get a1 i) (Array.unsafe_get a2 i)) then
	     raise Invalid
	 done;
	 true
       with
	   Invalid -> false)

  (** Cache of dummy arrays for avoiding creation of intermittent arrays. *)
  let dummy =
    let module Table = Hashtbl.Make(
      struct
	type t = int
	let equal = (==)
	let hash n = n
      end)
    in
    let table = Table.create 5 in
      fun n -> 
	try
	  Table.find table n
	with
	    Not_found -> 
	      let a = Array.create n (Obj.magic 0) in
		Table.add table n a; a

  let hash a = 
    Hashtbl.hash_param 3 3 a
		 
  module Cache = Weak.Make(
    struct
      type t = args
      let equal = for_all2 (==)     (* terms are hashconsed. *)
      let hash = Hashtbl.hash_param 3 3
    end)

  let cache = Cache.create 1007

  let _ = 
    Tools.add_at_exit 
      (fun () -> 
	 let (length, entries, _, smallest, median, biggest) = Cache.stats cache in
	   Format.eprintf "\nArgument cache: len = %d; # = %d; min = %d; median = %d; max = %d"
	     length entries smallest median biggest)

  let eq = (==)

  let compare a b =
    if a == b then 0 else
      let l = length a and m = length b in
	if l > m then 1 else 
	  if l < m then -1 else 
	    Pervasives.compare a b

  let make0 = [||]  (* no hashconsing needed here. *)

  let hashcons d = 
    try Cache.find cache d with Not_found -> 
      let a = Array.copy d in
	Cache.add cache a; a

  let make1 = 
    let d = dummy 1 in
      fun t -> 
	Array.unsafe_set d 0 t;
	hashcons d
	
  let make2 = 
    let d = dummy 2 in
      fun t1 t2 -> 
	Array.unsafe_set d 0 t1;
	Array.unsafe_set d 1 t2;
	hashcons d

  let make3 = 
    let d = dummy 3 in
      fun t1 t2 t3 -> 
	Array.unsafe_set d 0 t1;
	Array.unsafe_set d 1 t2;
	Array.unsafe_set d 2 t3;
	hashcons d

  let get = Array.get 
	      
  let of_list tl = 
    let l = List.length tl in
      if l = 0 then make0 else
	let d = dummy l in
	  for i = 0 to l - 1 do
	    Array.unsafe_set d i (List.nth tl i)
	  done;
	  hashcons d

  let to_list = Array.to_list

  let of_stack n st = 
    assert(Stacks.length st = n);
    if n = 0 then make0 else
      let d = dummy n in
	for i = 0 to n - 1 do
	  Array.unsafe_set d i (Stacks.pop st)
	done;
	assert(Stacks.is_empty st);
	hashcons d



  let iter = Array.iter

  let fold_right = Array.fold_right
  let fold_left = Array.fold_left

  let map f a = 
    let l = length a in
    let changed = ref false in
      if l = 0 then [||] else
	let d = dummy l in
	  for i = 1 to l - 1 do
	    let v = Array.unsafe_get a i in
	    let w = f v in
	      if v == w then () else 
		(Array.unsafe_set d i w; changed := true)
	  done;
	  if !changed then hashcons d else a

  let protect f a =
    let b = Array.copy a in
    let c = f b in
      hashcons c

  let set = Array.unsafe_set

  let eqbut a b =
    let l = length a and m = length b in
      if l <> m then None else
	let rec scaneq i =
	  if i >= l then None else
	    let s = get a i and t = get b i in
	      if s == t then scaneq (i + 1) else
		if eqtail i then Some(s, t) else None
	and eqtail i =
	  i >= l ||
	  (get a i == get b i && eqtail (i + 1))
	in
	  scaneq 0
end

let arg1 = function
  | App(a) -> Args.get a.args 0
  | _ -> raise Not_found

let arg2 = function
  | App(a) -> Args.get a.args 1
  | _ -> raise Not_found


(** Hashconsing term universe. *)
module Universe = Weak.Make(
  struct
    type t = trm
    let equal t1 t2 =
      match t1, t2 with
	| Var(x), Var(y) ->  
	    Name.eq x y
	| App(a1), App(a2) ->
            Funsym.eq a1.funsym a2.funsym &&
	    Args.eq a1.args a2.args
	| _ -> 
	    false
    let hash = hash
  end)

let universe = Universe.create 1007

let _ =
  Tools.add_at_exit 
    (fun () -> 
       let (length, entries, _, smallest, median, biggest) = Universe.stats universe in
	 Format.eprintf "\nTerm cache: len = %d; # = %d; min = %d; median = %d; max = %d"
	   length entries smallest median biggest)

let mk_var x = 
  Format.eprintf "\nmk_var <- %s" (Name.to_string x);
  let y = Universe.merge universe (Var(x)) in
    Format.eprintf "\nmk_var <- %s@." (Name.to_string (name_of y));
    y
(*
let mk_var n =
  Trace.call 3 "Term.mk_var" n Name.pp;
  let x = mk_var n in
    Trace.exit 3 "Term.mk_var" (name_of x) Name.pp;
    x
*)

    
let mk_app = 
  let dummy = 
    let app0 = App{funsym = Obj.magic 0; args = Obj.magic 0; hash = -1} in
    let arg0 = match app0 with App(arg0) -> arg0 | _ -> assert false in
      fun f al -> 
	arg0.funsym <- f;
	arg0.args <- al;
	app0
  in
    fun f al -> 
      let t0 = dummy f al in
	try                   (* Use of dummy avoids creating application.  *)
	  let t = Universe.find universe t0 in
	    Trace.msg 3 "Term.in.universe" () Pretty.unit;
	    t
	with
	    Not_found -> 
	      let t = App({funsym = f; args = al; hash = hash t0}) in
		Universe.add universe t; t

let mk_unary f a = mk_app f (Args.make1 a)

let mk_binary f a b = mk_app f (Args.make2 a b)

let mk_ternary f a b c = mk_app f (Args.make3 a b c)

(** Creating a constant. *)
let mk_const c = mk_app c Args.make0

(** Hashconsed equality. *)
let eq = (==)
    
let is_var = function Var _ -> true | _ -> false

let is_app = function App _ -> true | _ -> false

let is_const = function
  | App a when Args.length a.args == 0 -> true 
  | _ -> false

let is_unary = function
  | App a when Args.length a.args == 1 -> true 
  | _ -> false

let is_binary = function
  | App a when Args.length a.args == 2 -> true 
  | _ -> false

let is_ternary = function
  | App a when Args.length a.args == 3 -> true 
  | _ -> false



(** Some term functions are extendible and the theory-specific methods
  are handled through a registration mechanism. *)
module Methods = struct

  type m = {
    mutable printer : (Format.formatter -> Funsym.t -> Args.t -> unit) option;
    mutable can:  (Funsym.t -> Args.t -> t) option;
    mutable solve : (t -> t -> t Map.t) option;
    mutable is_diseq : (t -> t -> bool) option;
    mutable is_nonneg : (t -> Three.t) option;
    mutable is_pos : (t -> Three.t) option;
    mutable has_cnstrnt : (t -> Cnstrnt.t -> Three.t) option;
  }
    
  let empty () = {
    printer = None;
    can = None;
    solve = None;
    is_diseq = None;
    is_nonneg = None;
    is_pos = None;
    has_cnstrnt = None;
  }

  (** Registered methods. *)
  module Maps = struct
    let printer = ref Theory.Map.empty
    let can = ref Theory.Map.empty
    let solve = ref Theory.Map.empty
    let is_diseq = ref Theory.Map.empty
    let is_nonneg = ref Theory.Map.empty
    let is_pos = ref Theory.Map.empty
    let has_cnstrnt = ref Theory.Map.empty 
  end

  let add i fo m = 
    match fo with
      | Some(f) -> (m := Theory.Map.add i f !m)
      | None -> ()

  let register i r =
    add i r.printer Maps.printer;
    add i r.can Maps.can;
    add i r.solve Maps.solve;
    add i r.is_diseq Maps.is_diseq;
    add i r.is_nonneg Maps.is_nonneg; 
    add i r.is_pos Maps.is_pos;
    add i r.has_cnstrnt Maps.has_cnstrnt
   
  let printer i = Theory.Map.find i !Maps.printer
  let can i = Theory.Map.find i !Maps.can
  let solve i = Theory.Map.find i !Maps.solve
  let is_diseq i = Theory.Map.find i !Maps.is_diseq
  let is_nonneg i = Theory.Map.find i !Maps.is_nonneg
  let is_pos i = Theory.Map.find i !Maps.is_pos
  let has_cnstrnt i = Theory.Map.find i !Maps.has_cnstrnt
  
end



(** An external variable name is a nonempty string that
  does not contain a digit in the leftmost position. *)
let rec is_external_varname n =
  try
    let fst = String.get (Name.to_string n) 0 in
      not(is_digit fst)
  with
      Invalid_argument _ -> false

and is_digit ch = 
  ch >= '0' && ch <= '9'

(** An internal variable name starts with 
  digits followed by characters [!]. *)
let is_internal_varname n = 
  let str = Name.to_string n in
  let rec scan i = 
    try
      let ch = String.get str i in
	if is_digit ch then scan (i + 1) else ch = '!' 
    with
	Invalid_argument _ -> false
  in
    scan 0 

let d_internal_varname n =
  assert(is_internal_varname n);
  let str = Name.to_string n in
    try
      let i = String.index str '!' in
      let number = String.sub str 0 i in
      let name = String.sub str (i + 1) (String.length str - i - 1) in
	(int_of_string number, name)
    with
	Not_found -> invalid_arg "Term.d_internal_varname: invalid name"

let is_external_var = function 
  | Var(x) -> is_external_varname x 
  | _ -> false

let is_internal_var = function 
  | Var(x) -> not (is_external_varname x)  (* uses faster test. *)
  | _ -> false


(** Pretty-Printing terms. *)
let rec pp fmt = function
  | Var(x) -> pp_var fmt x
  | App(a) -> pp_app fmt (a.funsym, a.args)

and pp_var fmt x = 
  if is_external_varname x then
    Name.pp fmt x
  else 
    let (i, str) = d_internal_varname x in  (* print as [x!i]. *)
      Format.fprintf fmt "%s!%d" str i	  
 
and pp_app fmt (f, a) =
  let al = Array.to_list a in
  let pp_prefix_app f al = 
    Funsym.pp fmt f;
    Format.fprintf fmt "@[("; 
    Pretty.list pp fmt al; 
    Format.fprintf fmt ")@]"
  in
  let pp_sexpr_app f al = 
    Format.fprintf fmt "@[(";
    Funsym.pp fmt f;
    List.iter (fun a -> Format.fprintf fmt " "; pp fmt a) al;
    Format.fprintf fmt ")@]"
  in
    match !Pretty.flag with
      | Pretty.Mode.Mixfix -> 
	  let i = Funsym.theory_of f in
	    (try  (* try special printer. *)
	       Methods.printer i fmt f a
	     with
		 Not_found -> pp_prefix_app f al)
      | Pretty.Mode.Prefix -> 
	  pp_prefix_app f al
      | Pretty.Mode.Sexpr -> 
	  pp_sexpr_app f al 

let _ = (pptmp := pp)
	  
let to_string = Pretty.to_string pp
	  
let mk_external_var str =
  let n = Name.of_string str in
    assert(is_external_varname n);
    mk_var n

(** Global variable for creating fresh variables. *)
let k = ref (-1)
let _ = Tools.add_at_reset (fun () -> k := (-1))

let rec mk_internal_var str = function
  | Some(i) -> 
      mk_internal str i
  | None -> 
      mk_fresh_var str

and mk_fresh_var str = 
  assert(!k < max_int);
  incr(k); 
  mk_internal str !k

and mk_internal str i =
  let n = Name.of_string (string_of_int i ^ "!" ^ str) in
    assert(is_internal_varname n);
    mk_var n

(** Term ordering. Independent of low-level representations. *)
exception Less
exception Greater
let rec compare s t = 
  if s == t then 0 else
    match s, t with  
      | Var(x), Var(y) -> Name.compare x y
      | Var _, App _ -> 1
      | App _, Var _ -> -1
      | App(a), App(b) ->
	  let c = Funsym.cmp a.funsym b.funsym in
	    if c <> 0 then c else args_compare a.args b.args
	
(** Lexicographic ordering on term arguments. *)      
and args_compare a1 a2 =
  if a1 == a2 then 0 else 
    let l1 = Array.length a1 and l2 = Array.length a2 in
      if l1 < l2 then -1
      else if l1 > l2 then 1 else
	try
	  assert(l1 > 0);
	  for i = 0 to l1 - 1 do
	    let ci = compare (Array.unsafe_get a1 i) (Array.unsafe_get a2 i) in
	      if ci < 0 then 
		raise Less 
	      else if ci > 0 then 
		raise Greater
	  done;
	  invalid_arg "Term.args_compare: unreachable"
	with
	  | Less -> -1
	  | Greater -> 1

let (<<<) a b = (compare a b <= 0)

let orient ((a, b) as e) =
  if compare a b >= 0 then e else (b, a)

(** Iteration over terms. Apply [f] at variable positions. *)
let rec fold f t acc =
  match t with
    | Var _ -> f t acc
    | App(a) -> Args.fold_right (fold f) a.args acc

let rec iter f t  =
  match t with
    | Var _ -> f t
    | App(a) -> Args.iter (iter f) a.args

exception Found of t

let choose p t =
  let select x = if p x then raise(Found(x)) in
    try
      iter select t;
      raise Not_found
    with
	Found(x) -> x
	
let rec for_all p = function
  | (Var _ as x) -> p x
  | App(a) -> Args.for_all (for_all p) a.args

let rec exists p = function
  | (Var _ as x) -> p x
  | App(a) -> Args.exists (exists p) a.args

let rec subterm s =
  let rec subs t = 
    s == t ||
    match t with
      | Var _ -> false
      | App(a) -> Args.exists subs a.args
  in
    subs   

let rec is_ground = function
  | Var _ -> false
  | App(a) -> Args.for_all is_ground a.args

let occurs x b = subterm x b

(*
(** Application of substitution [a[x1:= a1]...[xn:=an]]. *)
let replace a xl al = 
  failwith "to do"
(*
  assert(List.length xl = List.length al);
  let lookup y =
    let rec find xl al =
      match xl, al with
	| [], [] -> 
	    raise Not_found
	| x' :: xl', a' :: al' ->
	    if Name.eq x' y then a' else find xl' al'
	| _ ->
	    invalid_arg "Term.replace: non well-formed substitution"
    in
      find xl al
  in
  let rec repl b =
    failwith "term: to do"
  in
    repl a
(*
    match b with
      | Var(y, _) -> 
	  (try lookup (var_name_of y) with Not_found -> b)
      | App(f, bl, _) -> 
	  let bl' = mapl repl bl in
	    if bl == bl' then a else
	      norm f bl'
  in
    repl a  
*)
*)
*)

let is_pure i =
  let rec loop = function
    | Var _ -> true
    | App(a) -> Funsym.theory_of a.funsym = i && Args.for_all loop a.args
  in
    loop

type status = 
  | Variable
  | Pure of Theory.t
  | Mixed of Theory.t * t

let mk_pure i = Pure(i)

let mk_mixed i a = Mixed(i, a)

let rec status = function
  | Var _ -> Variable
  | App(a) -> 
      let f = a.funsym and al = Args.to_list a.args in
      let i = Funsym.theory_of f in   (* needs improvement. *)
      let rec loop = function
	| [] ->
	    mk_pure i
	| a :: al -> 
	    (match status a with
	       | Variable -> loop al
	       | Pure(j) -> if Theory.eq i j then loop al else mk_mixed j a
	       | (Mixed _ as m) -> m)
      in
	loop al

let status2 a b =
  let s = status a in
    match s with
      | Mixed _ -> s
      | _ ->
	  let t = status b in
	    match s, t with
	      | _, Mixed _ -> t
	      | Variable, Variable -> Variable
	      | Variable, Pure _ -> t
	      | Pure _, Variable -> s
	      | Pure(i), Pure(j) ->
		  if Theory.eq i j then s else Mixed(i, a)
	      | _ -> invalid_arg "status: unreachable."

module Hash = Hashtbl.Make(
  struct
    type t = trm
    let equal = eq
    let hash = hash
    end)

(** Set of variables. *)
let rec vars_of t = 
  let xs = Set.empty () in
  let rec loop = function
    | ((Var _) as x) -> Set.add x xs
    | App(a) -> Args.iter loop a.args
  in
    loop t;
    xs

let rec is_var_of x t =
  match t with
    | Var _ -> eq x t
    | App(a) -> Args.exists (is_var_of x) a.args

type map = (t -> t) -> (t -> t)
type interp = Funsym.t -> Args.t -> t
type solve = t -> t -> t Map.t

(** Combined normalizer. *)
let sigma f args =
  let i = Funsym.theory_of f in
    try Methods.can i f args with Not_found -> mk_app f args

(** Apply [f] at variable positions. *)
let map f = 
  let rec mapf t =
    match t with
      | Var _ -> f t
      | App(a) -> 
	  let args' = Args.map mapf a.args in
	    if a.args == args' then t else sigma a.funsym args'
  in
    mapf

(** Replace [s] by [t] and build canonical terms. *)
let replace s t = 
  let rec repl c = 
    if eq c s then t else 
      try
	let a = args_of c in
	let a' = Args.map repl a in
	  if a == a' then c else sigma (sym_of c) a'
      with
	  Not_found -> c
  in
    repl

let solve s t =
  try
    let i = theory_of s in
      Methods.solve i s t
  with
      Not_found -> 
	let j = theory_of t in
	  Methods.solve j s t

(** Term substitutions [x |-> b] with [x] not necessarily 
  a variable and [b] does not contain any domain term. *)
module Subst = struct

  type t = trm Map.t

  let lookup rho x = Map.find x rho

  let empty = Map.empty 

  let singleton x a = 
    let m = Map.empty () in
      Map.set x a m;
      m

  let fuse rho y b =
    if eq y b then () else 
      let repl = replace y b in
	Map.iter
	  (fun x a ->
	     let a' = repl a in
	       if not(a == a') then
		 Map.set x a' rho)
	  rho
	
  let compose rho y b =
    assert(not(subterm y b));
    if eq y b then () else
      begin
	fuse rho y b;
	Map.set y b rho
      end 

  let pp = Map.pp pp

  let apply rho = 
    let rec app t =
      try Map.find t rho with Not_found -> 
	if is_var t then t else
	  let a = args_of t in
	  let b = Args.map app a in
	    if a == b then t else sigma (sym_of t) b
    in
      app

  let fold = Map.fold
  let iter = Map.iter

  let of_list l = 
    let m = Map.empty() in
    List.iter (fun (x, a) -> Map.set x a m) l;
      m

end

(** Application of substitution [s[x1:= t1]...[xn:=tn]] for
  variables [x1] through [xn]. *)
let vreplace s xl tl =
  assert(List.length xl = List.length tl);
  let lookup y =
    let rec find xl tl =
      match xl, tl with
	| [], [] -> raise Not_found
	| x' :: xl', a' :: al' -> if Name.eq x' y then a' else find xl' al'
	| _ ->
	    invalid_arg "Term.replace: non well-formed substitution"
    in
      find xl tl
  in
  let rec repl s =
    match s with
      | Var(y) -> 
	  (try lookup y with Not_found -> s)
      | App(a) -> 
	  let args' = Args.map repl a.args in
	    if a.args == args' then s else sigma a.funsym args'
  in
    repl s 
      
(** Some recognizers... *)

(** Test for equality or disequality. *)
let is_diseq_preds = ref Theory.Map.empty
let register_is_diseq i is_diseq =
  is_diseq_preds := Theory.Map.add i is_diseq !is_diseq_preds

let is_equal a b =
  if eq a b then Three.Yes else 
    try
      let i = theory_of a and j = theory_of b in
	if Theory.eq i j then 
	  if Methods.is_diseq i a b then Three.No else Three.X
	else 
	  Three.X
    with
	Not_found -> Three.X

let is_cnstrnt a c =
  try
    let i = theory_of a in
      Methods.has_cnstrnt i a c
  with
      Not_found -> Three.X

let is_nonneg t =
  try
    let i = theory_of t in
      Methods.is_nonneg i t
  with
      Not_found -> Three.X

let is_pos t =
  try
    let i = theory_of t in
      Methods.is_pos i t
  with
      Not_found -> Three.X


(** {6 Variable Assignments, Interpretations, and Models.} *)

module Assign = struct

  type t = Set.t Map.t
      
  let empty = 
    Map.empty ()

  let is_empty = Map.is_empty

  let apply alpha x = Map.find x alpha
      
  let add x v m =
    assert(is_var x);
    try
      let vs = apply m x in
	if Set.mem v vs then m else
	  let vs' = Set.copy vs in
	    Set.add v vs';
	    let m' = Map.copy m in
	      Map.set x vs' m';
	      m'
    with
	Not_found ->
	  let m' = Map.copy m in
	    Map.set x (Set.singleton v) m';
	    m'
	     
  let pp = 
    let pp_set fmt s = Pretty.set pp fmt (Set.to_list s) in
      Map.pp pp_set
		     
  let choose x alpha =
    let vs = Map.find x alpha in
      Set.choose vs
		       
  let is_ground =
    Map.for_all
      (fun _ -> Set.for_all is_ground)

  let is_deterministic =
    Map.for_all (fun _ vs -> Set.cardinal vs = 1)

  let to_list = Map.to_list

  let value i alpha = 
    let value_of_var x =
      assert(is_var x);
      try Set.choose (apply alpha x) with Not_found -> x
    in
    let rec evl a =
      if is_var a then value_of_var a else 
	let al = args_of a in
	let bl = Args.map evl al in
	  if al == bl then a else i (sym_of a) bl
    in
      evl

  let values i alpha =  
    let rec of_term a =
      if is_var a then 
	of_var a 
      else 
	let f = sym_of a and al = args_of a in
	let ll = of_args [] al in
	let vs = Set.empty () in
	  List.iter (fun l -> Set.add (i f l) vs) ll;
	  vs
    and of_var x =
      assert(is_var x);
      try apply alpha x with Not_found -> Set.singleton x
    and of_args acc =
      failwith "to do"
(*
 function
      | [] -> 
	  acc 
      | a :: al -> 
	  let vs = of_term a in
	  let acc' = of_args acc al in
	    cross vs acc'
*)
    and cross vs ll =
      List.fold_right
	(fun l acc1 -> 
	   Set.fold
	      (fun v acc2 -> (v :: l) :: acc1)
	   vs acc1)
	ll []
    in
      of_term

  let instantiate i alpha vs =
    let us = Set.empty () in
      Set.iter
	(fun v -> Set.union (values i alpha v) us)
	vs;
      us
    	       
  let combine i alpha1 alpha2 =
    if is_empty alpha1 then alpha2 else 
      if is_empty alpha2 then alpha1 else
	let sigma = Map.copy alpha1 in
	  Map.iter 
	    (fun x vs1 ->
	       let vs1' = instantiate i alpha2 vs1 in
		 try
		   let vs2 = Map.find x alpha2 in
		   let vs2' = instantiate i alpha1 vs2 in
		   let vs = Set.copy vs2' in
		     Set.union vs1' vs;
		     Map.set x vs sigma
		 with
		     Not_found -> ())
	    alpha1;
	  sigma
	    
end


module Interp = struct

  module I = Maps.Make(
    struct
      type t = Funsym.t * Args.t
      let rec compare (f, al) (g, bl) =
	let c = Funsym.cmp f g in
	  if c <> 0 then c else args_compare al bl
      let pp = pp_app
    end)
     
  type t = trm I.t

  let empty = I.empty()

  let apply m f al = 
    try
      I.find (f, al) m
    with
	Not_found -> sigma f al

  let update f al v m = 
    let m' = I.copy m in
      I.set (f, al) v m;
      m'

  let to_list = I.to_list
 
  let pp = I.pp pp
    
  let combine i j = 
    let k = I.copy j in
      I.iter
	(fun (f, al) b ->
	   assert(not(I.mem (f, al) j));
	   I.set (f, al) b k)
	i;
      k
      
end

module Model = struct

  type t = Interp.t * Assign.t

  let pp fmt (i, alpha) =
    Format.fprintf fmt "@[";
    Interp.pp fmt i;
    Format.fprintf fmt ",@ ";
    Assign.pp fmt alpha;
    Format.fprintf fmt "@]."

  let empty = (Interp.empty, Assign.empty)

  let is_deterministic (_, alpha) = Assign.is_deterministic alpha

  let is_ground (_, alpha) = Assign.is_ground alpha

  let to_list (i, alpha) = (Interp.to_list i, Assign.to_list alpha)

  let value (i, alpha) = Assign.value (Interp.apply i) alpha

  let values (i, alpha) = Assign.values (Interp.apply i) alpha

  let combine (i, alpha) (j, beta) =
    let k = Interp.combine i j in
      (k,  Assign.combine (Interp.apply k) alpha beta)

end

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


(** Theory definition. *)
let theory = Theory.create "fa"

let is_theory = Theory.eq theory

let _ = 
  Theory.Description.add theory
    "Theory of functional arrays
     Signature: 
       - create(x) : create array with content 'x'
       - a[i]      : lookup content of array 'a' at position 'i'
       - a[i:=x]   : update array 'a' at position 'i' with value 'x'
     Axioms:
       create(a)[j] = a
         a[i:=x][j] = x] when i = j
         a[i:=x][j] = a[j] when i <> j
      a[i:=x][i:=y] = a[i:=y]
      a[j:=y][i:=x] = a[i:=x][j:=y] when i <> j and i<<j"

(** Interpreted operations. *)
module Sig = struct
  let th = theory
  type t = Create | Select | Update

  let name =
    let create = Name.of_string "create"
    and select = Name.of_string "select"
    and update = Name.of_string "update" in
      function
	| Create -> create
	| Select -> select
	| Update -> update
end

module Op = Funsym.Make(Sig)

let op a = Op.out (Term.sym_of a)
let args = Term.args_of

let arg1 a = 
  assert(Term.Args.length a >= 1);
  Term.Args.get a 0

let arg2 a =   
  assert(Term.Args.length a >= 2);
  Term.Args.get a 1

let arg3 a =  
  assert(Term.Args.length a >= 3);
  Term.Args.get a 2

let is_interp a =
  try Op.is_interp (Term.sym_of a) with Not_found -> false

let rec is_pure t =
  try
    let a = args t in
      (match op t with
	 | Sig.Create -> is_pure (arg1 a)
	 | Sig.Select -> is_pure (arg1 a) && is_pure (arg2 a) 
	 | Sig.Update -> is_pure (arg1 a) && is_pure (arg2 a) && is_pure (arg3 a))
  with
      Not_found ->  Term.is_var t

let d_update t =
  match op t, args t with
    | Sig.Update, a -> (arg1 a, arg2 a, arg3 a)
    | _ -> raise Not_found

let d_select t =
  match op t, args t with
    | Sig.Select, a -> (arg1 a, arg2 a)
    | _ -> raise Not_found

let d_create t =
  match op t with
    | Sig.Create -> arg1 (args t)
    | _ -> raise Not_found

(** Creating constant array. *)
let mk_create =
  let create = Op.inj(Sig.Create) in
    fun t -> Term.mk_unary create t

(** Simplifying constructor for selection terms. *)
let mk_select =
  let select = Term.mk_binary (Op.inj(Sig.Select)) in
    fun b j -> 
      try
	(match op b with
	   | Sig.Create -> 
	       arg1 (args b)
	   | Sig.Update ->
	       let a3 = args b in
	       let a = arg1 a3 and i = arg2 a3 and x = arg3 a3 in
		 (match Term.is_equal i j with
		    | Three.Yes -> x
		    | Three.No -> select a j
		    | Three.X -> select b j)
	   | Sig.Select -> 
	       select b j)
      with
	  Not_found -> select b j
	  
(** Simplifying constructor for update terms. *)
let rec mk_update =
  let update = Term.mk_ternary (Op.inj Sig.Update) in
    fun a j y -> 
      try
	(match op a with
	   | Sig.Update ->          (* [b[i:=x][j:=y]]. *)
	       let a3 = args a in
	       let b = arg1 a3 and i = arg2 a3 and x = arg3 a3 in
		 (match Term.is_equal i j with
		    | Three.Yes -> update b i y
		    | Three.No when Term.compare i j > 0 -> mk_update (mk_update b j y) i x
		    | _ -> update a j y)
	   | (Sig.Create | Sig.Select) ->
	       update a j y)
      with
	  Not_found -> update a j y
	  
(** Array canonizer. *)
let sigma f a =
  assert(Op.is_interp f);
  match Op.out f with
    | Sig.Create -> mk_create (arg1 a)
    | Sig.Update -> mk_update (arg1 a) (arg2 a) (arg3 a)
    | Sig.Select -> mk_select (arg1 a) (arg2 a)
	
let rec map f =
  let rec mapf t = 
    try
      (match op t, args t with
	 | Sig.Create, a ->
	     let x = arg1 a in
	     let x' = mapf x in
	       if x == x' then t else mk_create x'
	 | Sig.Update, a ->
	     let a = arg1 a and i = arg2 a and x = arg3 a in
	     let a' = mapf a and i' = mapf i and x' = mapf x in
	       if a == a' && i == i' && x == x' then t else mk_update a' i' x'
	 | Sig.Select, a ->
	     let a = arg1 a and j = arg2 a in
	     let a' = mapf a and j' = mapf j in
	       if a == a' && j == j' then t else mk_select a' j')
    with
	Not_found -> f t
  in
    mapf

let pp fmt f a =
  assert(Op.is_interp f);
  match Op.out f with
    | Sig.Create -> 
	Format.fprintf fmt "create(";
	Term.pp fmt (arg1 a);
	Format.fprintf fmt ")"
    | Sig.Select ->
	Term.pp fmt (arg1 a);
	Format.fprintf fmt "[";  
	Term.pp fmt (arg2 a);  
	Format.fprintf fmt "]"
    | Sig.Update -> 
	Term.pp fmt (arg1 a);
	Format.fprintf fmt "[";
	Term.pp fmt (arg2 a);
	Format.fprintf fmt ":=";
	Term.pp fmt (arg3 a);
	Format.fprintf fmt "]"

let _ = 
  let m = Term.Methods.empty() in
    m.Term.Methods.printer <- Some(pp);
    m.Term.Methods.can <- Some(sigma);
    m.Term.Methods.is_diseq <- None;
    Term.Methods.register theory m


let splits t =
  raise Not_found
  

module T: Can.T = struct
      
  let th = theory
	     
  let map = map

  let can = sigma

  let is_diseq _ _ = false

  open Axioms

  let mk_app_create a = Lterm.mk_app (Op.inj Sig.Create) [a]
  let mk_app_update a i x = Lterm.mk_app (Op.inj Sig.Update)  [a; i ; x]
  let mk_app_select a i = Lterm.mk_app (Op.inj Sig.Select) [a; i]

  let rec chains() =
    [chain1(); chain2(); chain3(); chain4(); chain5(); chain6()]

  (** I. [a[i:=x][i] = x]. *)
  and chain1 () = 
    let a = Lterm.mk_var "a" 
    and i = Lterm.mk_var "i"
    and x = Lterm.mk_var "x" in
      (Axioms.Chain.mk_equal 
	 (Name.of_string "fa1")
	 [] 
	 (mk_app_select (mk_app_update a i x) i) 
	 x)

 (** II. [i <> j ==> a[i:=x][j] = a[j]]. *)
  and chain2 () = 
    let a = Lterm.mk_var "a" 
    and i = Lterm.mk_var "i"
    and j = Lterm.mk_var "j"
    and x = Lterm.mk_var "x" in
      Axioms.Chain.mk_equal 
	(Name.of_string "fa2")
	([Axioms.Atom.mk_diseq i j])
        (mk_app_select (mk_app_update a i x) j)
	(mk_app_select a j)

 (** III. [a[i:=x][i:=y] = a[i:=y]]. *)
  and chain3 () = 
    let a = Lterm.mk_var "a" 
    and i = Lterm.mk_var "i"
    and j = Lterm.mk_var "j"
    and x = Lterm.mk_var "x"
    and y = Lterm.mk_var "y" in
      Axioms.Chain.mk_equal 
	(Name.of_string "fa3")
	[]
	(mk_app_update (mk_app_update a i x) i y)
	(mk_app_update a i y)


  (** IV. [i <> j ==> a[i:=x][j:=y] = a[j:=y][i:=x]]. *)
  and chain4 () = 
    let a = Lterm.mk_var "a" 
    and i = Lterm.mk_var "i"
    and j = Lterm.mk_var "j"
    and x = Lterm.mk_var "x"
    and y = Lterm.mk_var "y" in
      Axioms.Chain.mk_equal 
        (Name.of_string "fa4")
        ([Axioms.Atom.mk_diseq i j])
        (mk_app_update (mk_app_update a i x) j y)
	(mk_app_update (mk_app_update a j y) i x)

  
  (** V. [create(a)[j] = a] *)
  and chain5 () = 
    let a = Lterm.mk_var "a" 
    and j = Lterm.mk_var "j" in
      Axioms.Chain.mk_equal  
	(Name.of_string "fa5")
	[]
	(mk_app_select (mk_app_create a) j)
	a

  and chain6 () =
    let a = Lterm.mk_var "a" 
    and b = Lterm.mk_var "b" 
    and i = Lterm.mk_var "i"
    and x = Lterm.mk_var "x"
    and y = Lterm.mk_var "y" in
      Axioms.Chain.mk_equal
	(Name.of_string "fa6")
	[Axioms.Atom.mk_equal (mk_app_update a i x) (mk_app_update b i y)]
	x y

  let disjunction e =
    raise Not_found

  let chains = chains()
 
end

(*
module Infsys: Can.INFSYS =
  Can.Infsys(T)

module Unit = 
  Can.Register(Infsys)
*)

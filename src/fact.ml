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

(** {6 Global variables} *)

let footprint = ref false

let fmt = ref Format.err_formatter

let print_justification = ref false


(** {6 Facts} *)

type t = Atom.t * Justification.t
    (** A {i fact} is just an atom together with a justification. *)

type fct = t

let compare (a1, _) (a2, _) = Atom.cmp a1 a2

let rec pp fmt (a, j) =
  if !print_justification then
    begin
      Format.fprintf fmt "\n";
      Justification.pp fmt j;
      Format.fprintf fmt "\n|---------\n"
    end;
  Atom.pp fmt a;
  Format.fprintf fmt "@?"
 
let atom_of (a, _) = a

let justification_of (_, j) = j



(** {6 Equality Facts} *)


module Equal = struct

  type t = Term.t * Term.t * Justification.t

  let lhs_of (a, _, _) = a
  let rhs_of (_, b, _) = b

  let destruct e = e

  (** Inject an equality fact into a fact. *)
  let inj (a, b, rho) = (Atom.Equal(a, b), rho)

  let pp fmt e = pp fmt (inj e)

  let make (a, b, rho) =                   (* [rho |- a = b] *)
    let (a, b) = Term.orient (a, b) in
      (a, b, rho)

  let map2 (f, g) ((a, b, rho) as e) =      (* [rho |- a = b] *)
    let (a', alpha) = f a in                (* [alpha |- a = a'] *)
    let (b', beta) = g b in                 (* [beta |- b = b'] *)
      match a == a', b == b' with
	| true, true ->
	    e
	| true, false ->
	    let rho' = Justification.subst_equal (a', b') rho [beta] in
	      make (a', b', rho')
	| false, true ->
	    let rho' = Justification.subst_equal (a', b') rho [alpha] in
	      make (a', b', rho')
	| _ -> 
	    let rho' = Justification.subst_equal (a', b') rho [alpha; beta] in
	      make (a', b', rho')

  let map f = map2 (f, f)

  let map_lhs f = map2 (f, Justification.Eqtrans.id)
  let map_rhs f = map2 (Justification.Eqtrans.id, f)


  let is_var (a, b, _) =
    Term.is_var a && Term.is_var b

  let is_pure i (a, b, _) =
    Term.is_pure i a && Term.is_pure i b

  let is_diophantine (a, b, _) =
    Arith.is_diophantine a && Arith.is_diophantine b

  module Inj = struct

    let solver f (a, b, rho) =         (* [rho |- a = b] *)
      let tau = Justification.solve (a, b) rho in  
	try
	  let sl = f (a, b) in
	  let justify (a, b) = (a, b, tau) in  (* [tau |- e] if [e] in [solve(a, b)] *)
	    List.map justify sl
	with
	    Exc.Inconsistent -> 
	      raise(Justification.Inconsistent(tau))

    let trans f e =
      let (a, b, rho) = destruct e in
      let (a', b') = f (a, b) in
      let rho' = Justification.dependencies [rho] in
	make (a', b', rho')

    let apply1 f (x, b, rho) a =
      let a' = f (x, b) a in
	if a == a' then Justification.Eqtrans.id a else
	  let rho' = Justification.apply1 (a', a) rho in
	    (a', rho')

    let norm apply el a = 
      let hyps = ref [] in
      let rec apply_star acc = function
	| [] -> acc
	| (x, b, rho) :: el ->
	    let acc' = apply (x, b) acc in
	      if not(Term.eq acc acc') then
		hyps := rho :: !hyps;
	      apply_star acc' el
      in
      let b = apply_star a el in
      let rho = Justification.apply (a, b) !hyps in
	(b, rho)

      
    let replace map apply a =
      let hyps = ref [] in
      let lookup y = 
	try
	  let (b, rho) = apply y in
	    hyps := rho :: !hyps;
	    b
	with
	    Not_found -> y
      in
      let b = map lookup a in
      let rho = Justification.apply (a, b) !hyps in
	(b, rho)

    let mapargs mk_app f a =
      let (op, al) = Term.App.destruct a in
	match al with
	  | [] -> 
	      Justification.Eqtrans.id a
	  | [a1] ->
	      let (b1, rho1) = f op a1 in
		if a1 == b1 then Justification.Eqtrans.id a else 
		  let (b, tau) = mk_app op [b1] in
		  let sigma = Justification.subst_equal (a, b) tau [rho1] in
		    (b, sigma)
	  | _ ->
	      let rhol = ref [] in                    (* [rhoi |- bi = ai] *)
	      let f' = Justification.Eqtrans.acc rhol (f op) in
	      let bl = Term.mapl f' al in                                
		if al == bl then Justification.Eqtrans.id a else 
		  let (b, tau) = mk_app op bl in      (* [tau |- b = op(bl)] *)
		  let sigma = Justification.subst_equal (a, b) tau !rhol in 
		    (b, sigma)                        (* [sigma |- a = b] *)

    let mapl f al =
      let rhol = ref [] in      
      let f' = Justification.Eqtrans.acc rhol f in
      let bl = Term.mapl f' al in 
        (bl, !rhol)
		 
  end 

end


(** {6 Disequality Facts} *)

module Diseq = struct

  type t = Term.t * Term.t * Justification.t

  let destruct d = d

  let lhs_of (a, _, _) = a
  let rhs_of (_, b, _) = b

  let inj (a, b, j) = (Atom.Diseq(a, b), j)

  let pp fmt d = pp fmt (inj d)

  let is_diophantine (a, b, _) =
    Arith.is_diophantine a && Arith.is_num b

  let is_var (a, b, _) =
    Term.is_var a && Term.is_var b

  let make ((a, b, rho) as d) =     (* [rho |- a <> b] *)
    if is_var d then
      let (a, b) = Term.orient (a, b) in
	(a, b, rho)
    else 
      if is_diophantine d then
	let lcm =                     (* least common multiple of denominators *)
	  Mpa.Q.abs                   (* of the coefficients in [a] and [b]. *)
	    (Mpa.Q.of_z
	       (Mpa.Z.lcm 
		  (Arith.lcm_of_denominators a)
		  (Arith.lcm_of_denominators b)))
	in
	let (a', b') =                (* all coefficients are integer now. *)
	  if Mpa.Q.is_one lcm then Term.orient (a, b) else 
	    (Arith.mk_multq lcm a, Arith.mk_multq lcm b)
	in
	let q', c' = Arith.destruct (Arith.mk_sub a' b') in  (* [q' + c' <> 0] *)
	  (c', Arith.mk_num (Mpa.Q.minus q'), rho)           (* ==> [x' <> -q'] *)
      else 
	let (a, b) = Term.orient (a, b) in
	  (a, b, rho)

  let map f ((a, b, rho) as d) =
    let (a', alpha') = f a                       (* [alpha' |- a = a'] *)
    and (b', beta') = f b in                     (* [beta' |- b = b'] *)
      match a == a', b == b' with
	| true, true -> d
	| _ ->
	    let rho' = Justification.subst_diseq (a', b') rho [alpha'; beta'] in
	      make (a', b', rho')

  let d_diophantine ((a, b, rho) as d) = 
    assert(is_diophantine d);
    let q = Arith.d_num b in
      (a, q, rho)

  type diseq = t

  module Set = Set.Make(
    struct
      type t = diseq
      let compare (a1, b1, _) (a2, b2, _) =
	let cmp = Term.cmp a1 a2 in
	  if cmp <> 0 then cmp else Term.cmp b1 b2
    end)

end




(** {6 Nonnegative Constraint Facts} *)

module Nonneg = struct

  type t = Term.t * Justification.t

  let inj (a, rho) = (Atom.Nonneg(a), rho)

  let pp fmt c = pp fmt (inj c)

  let make (a, rho) =
    try
      let (q, x) = Arith.d_multq a in
	if Mpa.Q.is_pos q then 
	  (x, rho)
	else 
	  (a, rho)
    with
	Not_found -> (a, rho)

  let make = Trace.func "foo9" "Make.Nonneg" pp pp make
          
  let destruct c = c
	  
  let map f ((a, rho) as c) =
    let (a', alpha') = f a in                    (* [alpha' |- a = a'] *)
      if a == a' then c else
	let rho' = Justification.subst_nonneg a' rho [alpha'] in
	  make (a', rho')

  let holds (a, rho) =
    match Arith.is_nonneg  a with
      | Three.No -> Justification.Three.No(rho)
      | Three.Yes -> Justification.Three.Yes(rho)
      | Three.X -> Justification.Three.X
   
end


(** {6 Nonnegative Constraint Facts} *)

module Pos = struct

  type t = Term.t * Justification.t

  let inj (a, rho) = (Atom.Pos(a), rho)

  let pp fmt c = pp fmt (inj c)

  let destruct c = c

  let is_int a = Three.X

  let make (a, rho) =               (* [rho |- a > 0] *)
    try
      let (q, x) = Arith.d_multq a in
	if Mpa.Q.is_pos q then 
	  (x, rho)
	else 
	  (a, rho)
    with
	Not_found -> (a, rho)
	  
  let map f ((a, rho) as c) =
    let (a', alpha') = f a in                    (* [alpha' |- a = a'] *)
      if a == a' then c else
	let rho' = Justification.subst_pos a' rho [alpha'] in
	  make (a', rho')

end



(** {6 Exceptions} *)

exception Inconsistent of t


(** {6 Recognizers} *)

let is_true (a, _) = Atom.is_true a
let is_false (a, _) = Atom.is_false a


(** {6 Constructors} *)

let make fct = 
  if !footprint then
    begin
      Format.fprintf !fmt "\nFact: ";
      pp !fmt fct;
      Format.fprintf !fmt "@?"
    end;
  fct

let mk_true rho = make (Atom.mk_true, rho)
let mk_false rho = make (Atom.mk_false, rho)

let mk_axiom a = make (a, Justification.axiom a)

open Justification.Three

let mk_equal is_equal (a, b, rho) =  
  match is_equal a b with
    | Yes(tau) -> 
	let sigma = Justification.dependencies [rho; tau] in
	  mk_true sigma
    | No(tau) -> 
	mk_false (Justification.contradiction rho tau)
    | X -> 
	make (Atom.Equal(a, b), rho)

let mk_diseq is_equal (a, b, rho) =  
  match is_equal a b with
    | Yes(tau) -> 
	mk_false (Justification.contradiction rho tau)
    | No(tau) ->
	let sigma = Justification.dependencies [rho; tau] in
	  mk_true sigma
    | X -> 
	make (Atom.Diseq(a, b), rho)

let mk_nonneg is_nonneg (a, rho) =
  match is_nonneg a with
    | Yes(tau) ->
	let sigma = Justification.dependencies [rho; tau] in
	  mk_true sigma
    | No(tau) -> 
	mk_false (Justification.contradiction rho tau)
    | X -> 
	make (Atom.Nonneg(a), rho)

let mk_pos is_pos (a, rho) = 
  match is_pos a with
    | Yes(tau) -> 
	let sigma = Justification.dependencies [rho; tau] in
	  mk_true sigma
    | No(tau) -> 
	mk_false (Justification.contradiction rho tau)
    | X -> 
	make (Atom.Pos(a), rho)


let map (is_equal, is_nonneg, is_pos) f ((atm, rho) as fct) =
  match atm with
    | Atom.True -> 
	fct
    | Atom.False -> 
	fct
    | Atom.Equal(a, b) ->
	let (a', b', rho') = Equal.map f (a, b, rho) in
	  if a == a' && b == b' then fct else
	    (match Term.is_equal a' b' with
	       | Three.Yes -> mk_true rho'
	       | Three.No -> mk_false rho'
	       | Three.X -> mk_equal is_equal (a', b', rho'))
    | Atom.Diseq(a, b) ->
	let (a', b', rho') = Diseq.map f (a, b, rho) in
	  if a == a' && b == b' then fct else 
	    (match Term.is_equal a' b' with
	       | Three.Yes -> mk_false rho'
	       | Three.No -> mk_true rho'
	       | Three.X -> mk_diseq is_equal (a', b', rho'))
    | Atom.Nonneg(a) ->
	let (a', rho') = Nonneg.map f (a, rho) in
	  if a == a' then fct else
	    (match Arith.is_nonneg a' with
	      | Three.Yes ->  mk_true rho'
	      | Three.No -> mk_false rho'
	      | Three.X -> mk_nonneg is_nonneg (a', rho'))
    | Atom.Pos(a) ->
	let (a', rho') = Pos.map f (a, rho) in
	  if a == a' then fct else
	    (match Arith.is_pos a' with
	       | Three.Yes -> mk_true rho'
	       | Three.No -> mk_false rho'
	       | Three.X -> mk_pos is_pos (a', rho'))
	    

(** {6 Set of facts} *)

module Set = Set.Make(
  struct
    type t = fct
    let compare (a, _) (b, _) = Pervasives.compare a b
  end)


(** {6 Stacks of facts} *)

module type STACK = sig
  type t
  val clear : unit -> unit
  val push : Th.t option -> t -> unit
  val pop : unit -> Th.t option * t
  val is_empty : unit -> bool
end

module type T = sig
  type t
  val pp : t Pretty.printer
end

module Stack(Arg: T) = struct 

  type t = Arg.t

  let stack = Stack.create ()

  let th_to_string = function
    | None -> "v"
    | Some(i) -> Th.to_string i

  let enabled = ref true

  let clear () = 
    if !enabled then
      Stack.clear stack
		   
  let push i e =
    if !enabled then
      let pushmsg = Format.sprintf "Push(%s): " (th_to_string i) in
	Trace.msg "stack" pushmsg e Arg.pp;
	Stack.push (i, e) stack
  
  let pop () =
    if !enabled then
      Stack.pop stack
    else 
      failwith "Fatal error: popping a disabled stack"
 
  let is_empty () = 
    Stack.is_empty stack

end

module Eqs = Stack(
  struct
    type t = Equal.t
    let pp = Equal.pp
  end)

module Diseqs = Stack(
  struct
    type t = Diseq.t
    let pp = Diseq.pp
  end)


let with_disabled_stacks f a =
  try
    Eqs.enabled := false;
    Diseqs.enabled := false;
    let b = f a in
      Eqs.enabled := true;
      Diseqs.enabled := true;
      b
  with
      exc ->
	Eqs.enabled := true;
	Diseqs.enabled := true;
	raise exc
    

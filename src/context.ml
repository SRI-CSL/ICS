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

(** A {b logical context} consists of a set of atoms. Such a context is
  represented in terms of a 
    - {b partition} (see {!Partition.t}) and an 
    - {b equality set} (see {!Solution.t}) for each theory in {!Th.t}. 

  A partition represents variable equalities [x = y] and variable 
  disequalities [x <> y], and the solution sets represent equalities [x = a], 
  where [x] is a variable and [a] is a pure term in some theory. An atom is
  added to a logical context by successively
  - Abstracting the atom to one which contains only pure terms using {!Context.abstract}, 
  this may involve the introduction of newly generated variables.
  - Canonization of terms using {!Context.can}, that is, computation of a normal form.
  - Processing of atoms using {!Context.equality} for merging two terms, {!Context.diseq}
  for adding a disequality, and {!Context.add} for adding a constraint.
  - Propagation of newly deduced facts to other components using {!Context.close}.

  For details see also: H. Ruess, N. Shankar, {i Combining Shostak Theories}, 
  published in the proceedings of RTA, 2002. 

  The operations above are all destructive in that they update logical
  contexts. A state is {i protected} against destructive updates by first 
  copying it {!Context.copy} and then updating the copy.

  We use the following conventions: [s] always denotes the logical state,
  [ctxt] denotes the set of atoms in the logical states, [p] denotes a partition, 
  and [eqs] stands for a set of equality sets. Furthermore, [a],[b] etc. are used for 
  terms, and whenever a term variable is intended, we use the names [x],[y],[z]. 
  Theory names are denoted by [i],[j] etc.
*)


(** {6 Logical context} *)

type t = {
  mutable ctxt : Atom.t list;      (* Current context. *)
  mutable p : Partition.t;         (* Variable partitioning. *)
  eqs : Combine.t;                 (* Theory-specific solution sets. *)
  mutable upper : int;             (* Upper bound on fresh variable index. *)
}


(** The empty logical context. *)
let empty = {
  ctxt = [];
  p = Partition.empty;
  eqs = Combine.empty;
  upper = 0
} 


(** Identity test. Do not take upper bounds into account. *)
let eq s1 s2 =              
  Partition.eq s1.p s2.p && 
  Combine.eq s1.eqs s2.eqs


(** Shallow copying. *)
let copy s = {
  ctxt = s.ctxt;
  p = Partition.copy s.p;
  eqs = Combine.copy s.eqs;
  upper = s.upper
}

module Mode = struct

  type t = Context | Internals | None

  let value = ref Internals

  let set m f a =
    let save = !value in
    try
      value := m;
      let b = f a in
	value := save;
	b
    with
	exc -> 
	  value := save;
	  raise exc
    
end 

(** Pretty-printing. *)
let rec pp fmt s =
  match !Mode.value with
    | Mode.Internals ->
	Partition.pp fmt s.p;
        Th.iter (fun i -> Combine.pp i fmt s.eqs)
    | Mode.Context -> 
	Pretty.set Atom.pp fmt (List.rev s.ctxt)
    | Mode.None -> 
	()


(** {6 Accessors} *)

let ctxt_of s = s.ctxt
let eqs_of s = s.eqs
let partition_of s = s.p
let upper_of s = s.upper
let config_of s = (s.p, s.eqs)


(** Processing a fact. *)
let rec process s ((atm, rho) as fct) =
  match Atom.atom_of atm with
    | Atom.TT -> ()
    | Atom.FF -> 
	raise(Jst.Inconsistent(rho))
    | Atom.Equal(a, b) -> 
	let e = Fact.Equal.make (a, b, rho) in
	let th = Fact.Equal.theory_of e in
	  Combine.merge th (s.p, s.eqs) e
    | Atom.Diseq(a, b) -> 
	Combine.dismerge (s.p, s.eqs) (Fact.Diseq.make (a, b, rho))
    | Atom.Nonneg(a) -> 
	Combine.process_nonneg (s.p, s.eqs) (Fact.Nonneg.make (a, rho))
    | Atom.Pos(a) -> 
	Combine.process_pos (s.p, s.eqs) (Fact.Pos.make (a, rho))


(** Propagate newly deduced facts. *)   
let rec close_star s =
  if not(Fact.Eqs.is_empty()) then
    begin
      close_e s (Fact.Eqs.pop());
      close_star s
    end 
  else if not(Fact.Diseqs.is_empty()) then
    begin
      close_d s (Fact.Diseqs.pop());
      close_star s
    end
  else if not(Fact.Nonnegs.is_empty()) then
    begin
      close_nn s (Fact.Nonnegs.pop());
      close_star s
    end


and close_e s (_, e) = 
  Trace.msg "rule" "Close" e Fact.Equal.pp;
  Combine.propagate_equal (s.p, s.eqs) e

and close_d s (i, d) = 
  Trace.msg "rule" "Close" d Fact.Diseq.pp;
  Combine.dismerge (s.p, s.eqs) d

and close_nn s (i, nn) = 
  Trace.msg "rule" "Close" nn Fact.Nonneg.pp;
  Combine.propagate_nonneg (s.p, s.eqs) nn


let normalize s =
  Combine.gc (s.p, s.eqs)


(** {6 Adding new atoms} *)

module Status = struct

  type 'a t = 
    | Valid of Jst.t
    | Inconsistent of Jst.t
    | Ok of 'a

  let pp_justification = ref true

  let pp pp fmt status =
    let ppj fmt rho =
      if !pp_justification then
	begin
	  Pretty.string fmt "\n";
	  Jst.pp fmt rho
	end
    in
      match status with
	| Valid(rho) -> 
	    Pretty.apply ppj fmt (":valid", [rho])
	| Inconsistent(rho) -> 
	    Pretty.apply ppj fmt (":unsat", [rho])
	| Ok(x) -> 
	    Pretty.apply pp fmt (":ok ", [x])

end


let simplify s = 
  Trace.func "rule" "Simplify" Atom.pp Fact.pp
    (Combine.simplify (s.p, s.eqs))

let process s = 
  Trace.proc "rule" "Process" Fact.pp (process s)

let abstract s =
  Trace.func "rule" "Abstract" Atom.pp Fact.pp
    (Combine.abstract (s.p, s.eqs))

let add s atm =
  let ((atm', rho') as fct') = simplify s atm in
    if Atom.is_true atm' then
      Status.Valid(rho')
    else if Atom.is_false atm' then
      Status.Inconsistent(Jst.dep2 rho' (Jst.axiom atm))
    else 
      (try
	 Fact.Eqs.clear();             (* Clearing out stacks. *)
	 Fact.Diseqs.clear();
	 Fact.Nonnegs.clear();
	 Term.Var.k := s.upper;        (* Install fresh variable index *)
	 let s = copy s in             (* Protect state against updates *)
	 let (atm'', rho'') = abstract s atm' in
	 let fct'' = (atm'', Jst.dep3 rho' rho'' (Jst.axiom atm)) in
	   process s fct''; 
           close_star s;
	   normalize s;   
	   s.ctxt <- atm :: s.ctxt;    (* Update context. *)
	   s.upper <- !Term.Var.k;     (* Install variable counter in state. *)
	   Status.Ok(s)
	 with
	   | Jst.Inconsistent(rho) ->         
	       Status.Inconsistent(rho)
	   | exc ->     
	       raise exc)

let add =
  let pp0 fmt s = Mode.set Mode.None (pp fmt) s in
  let ppc fmt s = Mode.set Mode.Context (pp fmt) s in
    Trace.func2 "top" "Process" ppc Atom.pp (Status.pp pp0) 
      add

let addl =
  let rec loop s = function
    | [] -> 
	Status.Ok(s)
    | a :: al -> 
	(match add s a with
	   | Status.Valid _ -> loop s al
	   | Status.Ok(s') -> loop s' al   
	   | Status.Inconsistent(rho) -> Status.Inconsistent(rho))
  in
    loop 


let is_inconsistent =
  let rec loop s = function
    | [] -> false
    | a :: al -> 
	(match add s a with
	   | Status.Valid _ -> loop s al
	   | Status.Inconsistent _ -> true
	   | Status.Ok(s') -> loop s' al)
  in
    loop 

let is_valid =
  let rec loop s = function
    | [] -> true
    | a :: al -> 
	(match add s a with
	   | Status.Valid _ -> loop s al
	   | _ -> false)
  in
    loop

(* Check if [s] is satisfiable after case-splittiing. *)
let check_sat s =
  let error_valid_arg s a =
    invalid_arg (
      Format.sprintf "Fatal Error: %s \n valid in case split for\n%s@."
		   (Atom.to_string a)
		   (Pretty.to_string pp s))
  in
  let rec check splits s =
    try
      (match Combine.split (s.p, s.eqs) with
	 | Combine.Split.Finint(fin) -> 
	     invalid_arg "Splits on integers: to do"
	 | Combine.Split.Equal(i, j) -> 
	     let e = Atom.mk_equal (i, j)
	     and d = Atom.mk_diseq (i, j) in
               (match add s e with
		  | Status.Inconsistent _ ->
		      (match add s d with
			 | Status.Inconsistent _ -> None
			 | Status.Ok(s') -> check (d :: splits) s'
			 | Status.Valid _ -> error_valid_arg s d)
		  | Status.Ok(s') ->
		      (match check (e :: splits) s' with
			 | None -> 
			     (match add s d with
				| Status.Inconsistent _ -> None
				| Status.Ok(s'') -> check (d :: splits) s''  
				| Status.Valid _ -> error_valid_arg s d)
			 | Some(s'', splits'') -> 
			     Some(s'', splits''))
		  | Status.Valid _ -> 
		      error_valid_arg s e))
    with
	Not_found -> Some(splits, s)
  in
    check [] s


		 

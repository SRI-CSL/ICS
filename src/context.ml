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

(** Logical context manipulations. *)

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

exception Found of Atom.Set.t

(** {6 Logical contexts} *)

type t = {
  ctxt : Atom.t list;              (* Current context. *)
  p : Partition.t;                 (* Variable partitioning. *)
  eqs : Combine.E.t;               (* Theory-specific equality sets. *)
  upper : int;                     (* Upper bound on fresh variable index. *)
}


(** The empty logical context. *)
let empty = {
  ctxt = [];
  p = Partition.empty;
  eqs = Combine.E.empty;
  upper = 0
} 


(** Identity test. Do not take upper bounds into account. *)
let eq s1 s2 =              
  Partition.eq s1.p s2.p && 
  Combine.E.eq s1.eqs s2.eqs


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
	(let v = Partition.v_of s.p in
	   if not(V.is_empty v) then
	     begin
	       Format.fprintf fmt "\nV: ";
	       V.pp fmt v
	     end);
	(let d = Partition.d_of s.p in
	   if not(D.is_empty d) then
	     begin
	       Format.fprintf fmt "\nD: ";
	       D.pp fmt d
	    end);
	if not(Combine.E.is_empty s.eqs) then
	  begin
	    Format.fprintf fmt "\n";
            Combine.E.pp fmt s.eqs
	  end 
    | Mode.Context -> 
	Pretty.set Atom.pp fmt (List.rev s.ctxt)
    | Mode.None -> 
	()


(** {6 Accessors} *)

let ctxt_of s = s.ctxt
let eqs_of s = s.eqs
let partition_of s = s.p
let upper_of s = s.upper
let config_of s = (s.eqs, s.p)

let normalize = Combine.gc 


(** {6 Processing atoms} *)

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

(** Enable/Disable cone of influence. *)
let coi_enabled = ref 0        (* disabled *)
let semantic_coi_min = ref 3   (* initial value for semantic cone of influence. May be adjusted. *)
let syntactic_coi_min = ref (-1)


(** Statistics. *)
let statistics = ref false
let verbose = ref false
let num_of_inconsistencies = ref 0
let total_savings = ref 0.

let _ = Tools.add_at_reset (fun () -> num_of_inconsistencies := 0)
let _ = Tools.add_at_reset (fun () -> total_savings := 0.)

let _ = 
  Tools.add_at_exit 
    (fun () -> 
       if !statistics then
	 begin
	   if !coi_enabled = 2 || !semantic_coi_min >= 0 then
	     Format.eprintf "\nSemantic cone of influence (min: %d)"
	       (if !semantic_coi_min >= 0 then !semantic_coi_min else 0)
	   else if !coi_enabled = 1 || !syntactic_coi_min >= 0 then
	     Format.eprintf "\nSyntactic cone of influence (min: %d)"
	       (if !syntactic_coi_min >= 0 then !syntactic_coi_min else 0)
	   else 
	     Format.eprintf "\nNo cone of influence";
	   Format.eprintf "\nNumber of inconsistencies: %d" !num_of_inconsistencies;
	   Format.eprintf "\nCone of influence savings: %d / 100" 
	     (truncate ((1. -. (!total_savings /. (float_of_int !num_of_inconsistencies))) *. 100.));
	   Format.eprintf "\n@."
	 end)


let rec add s atm =
  let atm', rho' = Combine.simplify (s.eqs, s.p) atm in
    if Atom.is_true atm' then
      Status.Valid(rho')
    else if Atom.is_false atm' then
      let rho = Jst.dep2 rho' (Jst.axiom atm) in    
      let tau = cone_of_influence s atm rho in
	Status.Inconsistent(tau)
    else 
      (try
	 Term.Var.k := s.upper;         (* Install fresh variable index. *)
	 let fct' =  (atm', Jst.dep2 rho' (Jst.axiom atm)) in
	 let (eqs', p') = Combine.process fct' (s.eqs, s.p) in
	   if Combine.E.eq s.eqs eqs' && 
	     Partition.eq s.p p' 
	   then
	     Status.Valid(rho')
	   else 
	     let s' = {
	       ctxt = atm :: s.ctxt;
	       upper = !Term.Var.k;
	       p = p';
	       eqs = eqs'
	     } 
	     in
	       Status.Ok(s')
       with
	 | Jst.Inconsistent(rho) -> 
	     let tau = cone_of_influence s atm rho in
	       Status.Inconsistent(tau))

and cone_of_influence s atm rho = 
  let inconsistency = match Jst.Mode.get () with
    | Jst.Mode.Dep -> Jst.axioms_of rho
    | Jst.Mode.No -> Atom.Set.add atm (ctxt2atoms s)
  in
  let inconsistency' = 
    match !coi_enabled with
      | 1 -> 
	  syntactic_cone_of_influence atm inconsistency
      | 2 -> 
	  semantic_cone_of_influence atm inconsistency
      | _ ->
	  if !semantic_coi_min >= 0 &&  
	    (Atom.Set.cardinal inconsistency) >= !semantic_coi_min 
	  then
	    semantic_cone_of_influence atm inconsistency
	  else if !syntactic_coi_min >= 0 && 
	    (Atom.Set.cardinal inconsistency) >= !syntactic_coi_min 
	  then
	    syntactic_cone_of_influence atm inconsistency
	  else 
	    inconsistency
  in
  let savings = 
    if inconsistency == inconsistency' then 1. else
      let n = float_of_int (Atom.Set.cardinal inconsistency')
      and m = float_of_int (Atom.Set.cardinal inconsistency) in
	n /. m
  in
    if !statistics then
      begin
	incr(num_of_inconsistencies);
	total_savings := !total_savings +. savings;
      end;
    adjust_coi savings;
    Jst.of_axioms inconsistency'

(** Dynamically adjust cone of influence *)
and adjust_coi savings = 
 (*  if !statistics then
    Format.eprintf "\nCOI Ratio: %f" savings; *)
  if savings > 0.97 then   
    begin
      if !semantic_coi_min >= 0 && !semantic_coi_min <= 150 then 
	begin
	  semantic_coi_min := !semantic_coi_min + 1;
	  if false && !verbose then
	    Format.eprintf "\n Adjusting min for semantic COI to %d" !semantic_coi_min
	end 
      else if !syntactic_coi_min >= 0 && !semantic_coi_min <= 150 then 
	begin
	  syntactic_coi_min := !syntactic_coi_min + 1;
	  if false && !verbose then
	    Format.eprintf "\n Adjusting min for syntactic COI to %d" !syntactic_coi_min
	end
    end
  else if savings < 0.90 then
    begin
      if !semantic_coi_min > 0 then 
	begin
	  semantic_coi_min := !semantic_coi_min - 1;
	  if false && !verbose then
	    Format.eprintf "\n Adjusting min for semantic COI to %d" !semantic_coi_min
	end 
      else if !syntactic_coi_min > 0 then 
	begin
	  syntactic_coi_min := !syntactic_coi_min - 1;
	  if false && !verbose then
	    Format.eprintf "\n Adjusting min for syntactic COI to %d" !syntactic_coi_min
	end
    end


and ctxt2atoms s =
  List.fold_right Atom.Set.add s.ctxt Atom.Set.empty

and syntactic_cone_of_influence atm inconsistency =
  let visited = ref (Atom.Set.singleton atm) in
  let todo = Stack.create () in
  let rec loop () =
    try
      let current = Stack.pop todo in
	Atom.Set.iter
	  (fun atm ->
	     if not(Atom.Set.mem atm !visited) &&
	       Atom.is_connected atm current 
	     then
	       begin
		 visited := Atom.Set.add atm !visited;
		 Stack.push atm todo
	       end)
	  inconsistency;
	loop ()
    with
	Stack.Empty -> !visited
  in
    Stack.push atm todo;
    let inconsistency' = loop () in  
      trace_coi inconsistency' inconsistency;
      inconsistency'


and semantic_cone_of_influence atm inconsistency =
  let visited = ref (Atom.Set.singleton atm) in
  let s = ref empty in
  let todo = Stack.create () in
  let rec loop () =
    try
      let current = Stack.pop todo in
	Atom.Set.iter
	  (fun atm ->
	     if not(Atom.Set.mem atm !visited) &&
	       Atom.is_connected atm current 
	     then
	       match add !s atm with
		 | Status.Valid _ -> ()
		 | Status.Ok(s') -> 
		     s := s'; 
		     visited := Atom.Set.add atm !visited; 
		     Stack.push atm todo
		 | Status.Inconsistent _ -> 
		     raise(Found(Atom.Set.add atm !visited)))
	  inconsistency;
	loop ()
    with
	Stack.Empty -> !visited
  in
    Stack.push atm todo;
    let proofmode = Jst.Mode.get () in
      try
	Jst.Mode.set Jst.Mode.No;
	let inconsistency' = try loop () with Found(atms) -> atms in
	  Jst.Mode.set proofmode;
	  inconsistency'
      with
	  exc -> 
	    Jst.Mode.set proofmode;
	    raise exc
	  

and trace_coi atms1 atms2 =
  Trace.msg "coi" "COI" 
    (Atom.Set.cardinal atms1, Atom.Set.cardinal atms2) 
    (Pretty.pair Pretty.number Pretty.number)
	

let addl atms =
  let rec loop s = function
    | [] -> 
	Status.Ok(s)
    | a :: al -> 
	(match add s a with
	   | Status.Valid _ -> loop s al
	   | Status.Ok(s') -> loop s' al   
	   | Status.Inconsistent(rho) -> Status.Inconsistent(rho))
  in
    loop atms


(*
(* For debugging:  *)
let add =
  let pp0 fmt s = Mode.set Mode.None (pp fmt) s in
  let ppc fmt s = Mode.set Mode.Context (pp fmt) s in
    Trace.func2 "top" "Process" ppc Atom.pp (Status.pp pp0)
      add
*)


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
  Some(s)  (* to do *)


let diff s1 s2 =
  let p' = Partition.diff s1.p s2.p
  and eqs' = Combine.E.diff s1.eqs s2.eqs in
    {s1 with p = p'; eqs = eqs'}


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
open Term
(*i*)

type t = {                             (* Constraints are compose of explicit constraints. *)
  explicit: Cnstrnt.t;                 (* and a list of pairs [(c,a)] which represents the *)
  symbolic: (Cnstrnt.t * Term.t) list  (* constraint [c] minus the constraint of term [a]. *)
}

let explicit s = s.explicit
let symbolic s = s.symbolic

let destruct s = (s.explicit, s.symbolic)

let make (e,s) = { explicit = e; symbolic = s}

let is_explicit s = s.symbolic = []

let of_cnstrnt c = { explicit = c; symbolic = [] }

let full = of_cnstrnt Cnstrnt.mk_real

let rec pp fmt s = 
  Cnstrnt.pp fmt s.explicit;
  if s.symbolic <> [] then
    begin
      Pretty.string fmt " inter ";
      pp_implicit fmt s.symbolic
    end

and pp_implicit fmt =
  Pretty.infixl
    (fun fmt (c,a) ->
       Cnstrnt.pp fmt c;
       Pretty.string fmt " - Cnstrnt(";
       Term.pp fmt a;
       Pretty.string fmt ")")
    " inter "
    fmt

(*s Status. *)

let status s =
  if Cnstrnt.is_empty s.explicit then
    Status.Empty
  else
    match Cnstrnt.d_singleton s.explicit with
      | Some(q) -> 
	  Status.Singleton(q)
      | None ->
	  if Cnstrnt.is_full s.explicit && s.symbolic = [] then
	    Status.Full
	  else
	    Status.Other

(*s Meaning of a symbolic constraint is obtained by
 instantiating all symbolic constraints with explicit constraints. *)

let rec meaning f s = 
  let rec symbolic1 (c,a) = 
    Cnstrnt.subtract c (cnstrnt f a)

  and symbolic l = 
    match l with
    | [] -> 
	Cnstrnt.mk_real
    | [(c,a)] -> 
	symbolic1 (c,a)
    | (c,a) :: xl ->
	Cnstrnt.inter (symbolic1 (c,a)) (symbolic xl)
  in
  if s.symbolic = [] then
    s.explicit
  else 
    Cnstrnt.inter s.explicit (symbolic s.symbolic)
  
    
(*s Constraint of a term [a] in context [f] *)

and cnstrnt f a =
  let rec cnstrnt_of_term a =
  match Arith.d_interp a with
    | Some(Sym.Num(q), []) -> 
	Cnstrnt.mk_singleton q
    | Some(Sym.Mult, l) -> 
	Cnstrnt.multl (List.map cnstrnt_of_term l)
    | Some(Sym.Add, l) -> 
	Cnstrnt.addl (List.map cnstrnt_of_term l)
    | Some(Sym.Expt(n), [x]) -> 
	Cnstrnt.expt n (cnstrnt_of_term x)
    | _ ->
	meaning f (f a)    (* raise [Not_found] if [a] not in domain. *)
  in 
  try
    cnstrnt_of_term a
  with
      Not_found -> Cnstrnt.mk_real

(*s [occurs x s] holds if [x] occurs uninterpreted in s. *)

let occurs x s =
  List.exists (fun (_,a) -> Term.subterm x a) s.symbolic

(*s Replace [x] by [b] in constraint and take care of symbolic constraints
 which become explicit in the process. *)

let replace x b s =
  if not(occurs x s) then
    s
  else 
    let repl a = Arith.replace x b a in
    let (e', sl') = 
      List.fold_right
	(fun (c,a) ((e, sl) as acc) ->
	   let a' = repl a in
	   match Arith.d_num a' with
	     | Some(q) ->
		 let e' = Cnstrnt.subtract e (Cnstrnt.mk_singleton q) in
		 (Cnstrnt.inter c e, sl)
	     | None ->
		 (e, (c, a') :: sl))
	s.symbolic
	(s.explicit, [])
    in
    make (e', sl')



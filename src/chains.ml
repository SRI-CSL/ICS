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

(** Representation of a forward chaining rule. *)

type t = chain list

and chain = hyps * concl

and rewrite = hyps * Term.t * Term.t

and hyps = atom list

and concl = atom

and atom = 
  | Equal of Term.t * Term.t
  | Diseq of Term.t * Term.t


let rec pp fmt =
  List.iter (pp_chain fmt)

and pp_chain fmt (hl, cl) =
  pp_hyps fmt hl;
  Format.fprintf fmt " ==> ";
  pp_atom fmt cl

and pp_hyps fmt =
  let rec iterate = function
    | [] -> ()
    | [h] -> pp_atom fmt h
    | h :: hl -> pp_atom fmt h; Format.fprintf fmt ", "; iterate hl
  in
    iterate

and pp_atom fmt = function
  | Equal(a, b) -> Term.pp fmt a; Format.fprintf fmt " = "; Term.pp fmt b
  | Diseq(a, b) -> Term.pp fmt a; Format.fprintf fmt " <> "; Term.pp fmt b



(** {6 Normalization} *)

exception Not_applicable

let rec normalize chs =
  let nrm = 
    List.fold_right 
      (fun ch acc -> 
	 try
	   let nrm1 = normalize1 ch in
	     seq nrm1 acc
	 with
	     Not_found -> acc)
      chs id
  in
    repeat nrm

and id f al =
  Term.mk_app f al

and seq nrm1 nrm2 f al =
  try
    nrm1 f al
  with
      Not_applicable -> nrm2 f al

and repeat nrm =
  let rec nrm_star f al = 
    try
      let b = nrm f al in
	if Term.is_var b then b else
	  nrm_star (Term.sym_of b) (Term.args_of b)
    with
	Not_applicable -> id f al
  in
    nrm_star

and normalize1 (hyps, concl) =
  failwith "to do"
(*
  match Atom.atom_of concl with
    | Atom.Equal(a, b) -> 
	failwith "normalize1: to do"
    | _ -> raise Not_found
*)

and pattern_to_pred a =
  let loop symtab pred b =
    if Term.is_var b then
      failwith "to do"
    else
      failwith "to do"
  in
    loop

(** {6 Matcher} *)

exception No_match

(** Matching a pattern with a term:
  - [match ?x b rho = rho] if [rho(?x) = b]
  - [match ?x b rho = bot] if [rho(?x) <> b]
  - [match f(a{1},...,a{n}) f(b{1},...,b{n}) rho = rho{n}]
    if [rho{0} = rho], [rho{i+1} = match a{i} b{i} rho{i}]. *)
let matcher pat a = 
  let loop pat a rho =
    if Term.is_var pat then
      try
	let b = Term.Varmap.find pat rho in
	  if Term.eq a b then rho else raise No_match
      with
	  Not_found -> raise No_match
    else
      failwith "to do"
  in
    loop pat a Term.Varmap.empty
	  
      

(** {6 Disequality test} *)

let is_diseq chs =
  failwith "Chains.is_diseq: to do"



(** {6 Flattening} *)

module Flat = struct

  type t = chain list

  and chain = hyps * concl

  and hyps = hyp list

  and hyp = 
    | Equal of var * term
    | Diseq of var * var

  and conc = 
    | Equal of term * term
    | Diseq of term * term

  and var = Name.t

  and term = 
    | Var of Name.t
    | App of Funsym.t * Name.t list
	
end 


let rec flatten cl = failwith "to do"



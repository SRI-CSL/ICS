(*
 * The contents of this file are subject to the ICS(TM) Community Research
 * License Version 1.0 (the ``License''); you may not use this file except in
 * compliance with the License. You may obtain a copy of the License at
 * http://www.icansolve.com/license.html.  Software distributed under the
 * License is distributed on an ``AS IS'' basis, WITHOUT WARRANTY OF ANY
 * KIND, either express or implied. See the License for the specific language
 * governing rights and limitations under the License.  The Licensed Software
 * is Copyright (c) SRI International 2003, 2004.  All rights reserved.
 * ``ICS'' is a trademark of SRI International, a California nonprofit public
 * benefit corporation.
 *)

type t = {
  mutable theory : Theory.t;
  mutable symbol : Name.t;
}

type funsym = t

let create = 
  let module Memoize = Weak.Make(
    struct
      type t = funsym
      let equal f g = Name.eq f.symbol g.symbol && Theory.eq f.theory g.theory
      let hash f = Name.idx f.symbol
    end)
  in
  let table = Memoize.create 17 in
  let _ = Tools.add_at_reset (fun () -> Memoize.clear table) in
  let dummy = 
    let scaffold = { theory = Obj.magic 0; symbol = Obj.magic 0 } in
      fun i n -> 
	scaffold.theory <- i; 
	scaffold.symbol <- n;
	scaffold
  in
    fun i n -> 
      try      (* Avoid creating symbol by looking for an instance of the symbol. *)
	Memoize.find table (dummy i n)
      with
	  Not_found -> 
	    let f = {theory = i; symbol = n} in
	      Memoize.add table f;
	      f

let eq f g =
  Name.eq f.symbol g.symbol &&
  Theory.eq f.theory g.theory

let hash f = Name.idx f.symbol

let name_of f = f.symbol

let theory_of f = f.theory

let is_interp i f =
  try Theory.eq i f.theory with Not_found -> false

let cmp f g =
  let cmp = Name.compare f.symbol g.symbol in
    if cmp <> 0 then cmp else Pervasives.compare f.theory g.theory
      
let pp fmt f =
  Name.pp fmt f.symbol;
  if (Version.debug() >= 3) then
    begin
      Format.fprintf fmt "{";
      Theory.pp fmt f.theory;
      Format.fprintf fmt "}@;";
    end 

let to_string f =
  pp Format.str_formatter f;
  Format.flush_str_formatter ()
    
   
module Map = Map.Make(
  struct
    type t = funsym
    let compare = cmp
  end)
 
module Set = Set.Make(
  struct
    type t = funsym
    let compare = cmp
  end)

module Hash = Hashtbl.Make(
  struct
    type t = funsym
    let equal = eq
    let hash = hash
  end)


(** Interpreted operations. *)
module type SIG = sig
  val th : Theory.t
  type t
  val name : t -> Name.t
end

module Make(Sig: SIG) = struct

  let equal op1 op2 =
    Name.eq (Sig.name op1) (Sig.name op2)

  let table = Hash.create 5
		
  let out f =
    Hash.find table f
      
  let inj =
    let module Memoize = Hashtbl.Make(
      struct
	type t = Sig.t
	let equal = equal
	let hash op = Name.idx (Sig.name op)
      end)
    in
    let memoize = Memoize.create 5 in
      fun op ->
	try
	  Memoize.find memoize op
	with
	    Not_found -> 
	      let f = create Sig.th (Sig.name op) in
		Hash.add table f op; 
		Memoize.add memoize op f;
		f
	
  let is_interp f =
    try
      Theory.eq Sig.th f.theory
    with
	Not_found -> false
	
end 

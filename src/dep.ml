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

(** Finite map with term variables as keys. *)
module Map = Maps.Make(
  struct
    type t = Term.t
    let compare x y = 
      match x, y with
	| Term.Var(n), Term.Var(m) -> Name.compare n m
	| _ -> invalid_arg "Dep.Set.compare: nonvariable term"
    let pp = Term.pp
  end)

(** Finite set of term variables. *)
module Set = Sets.Make(
  struct
    type t = Term.t
    let compare x y = 
      match x, y with
	| Term.Var(n), Term.Var(m) -> Name.compare n m
	| _ -> invalid_arg "Dep.Map.compare: nonvariable term"
    let pp = Term.pp
  end)

type t = Set.t Map.t

let mem x = Map.mem x

let pp = Map.pp Set.pp
	      
let find u a =
  assert(Term.is_var a);
  try Map.find a u with Not_found -> Set.empty()

let empty = Map.empty
 
let is_empty = Map.is_empty

let copy u = 
  if is_empty u then u else Map.copy u
      
(** [add x y m] adds [x] to the use of [y]. *)
let add x y u =
  assert(Term.is_var x && Term.is_var y);
  Trace.call 8 "Dep.add" (x, y) (Pretty.pair Term.pp Term.pp);
  (try 
     let uy = Map.find y u in
       Set.add x uy
   with
       Not_found -> Map.set y (Set.singleton x) u);
  Trace.exit0 8 "Dep.add"
   
(** [remove x y s] deletes [x] from the use of [y]. *)
let remove x y u = 
  assert(Term.is_var x && Term.is_var y); 
  try 
    let uy = Map.find y u in
      Trace.call 8 "Dep.remove" (x, y) (Pretty.pair Term.pp Term.pp);
      Set.remove x uy;
      if Set.is_empty uy then
	Map.remove y u;
      Trace.exit0 8 "Dep.remove"
  with
      Not_found -> ()

let replace x y z u =
  remove x z u;
  add y z u


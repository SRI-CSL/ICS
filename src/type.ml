
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
open Mpa         
open Binrel
open Tools
open Dom
open Hashcons
(*i*)

type t = cnstrnt hashed

and cnstrnt = 
  | Bot
  | Number of Number.t
  | Enumeration of Name.Set.t
  | Bitvector of int option
  | Top

let destruct c = c.node

let eq = (===)

(*s Hashconsing constraints. *)

module HashCnstrnt = Hashcons.Make(      
  struct 
    type t = cnstrnt

    let equal c d =
      match c, d with
	| Top, Top -> 
	    true
	| Bot, Bot -> 
	    true
	| Number(n), Number(m) -> 
	    Number.eq n m
	| Enumeration(ns), Enumeration(ms) ->
	    Name.Set.equal ns ms
	| Bitvector n, Bitvector m ->
	    (match n, m with
	       | Some(x), Some(y) -> n = m
	       | None, None -> true
	       | _ -> false)
	| _ -> false

    let hash = Hashtbl.hash
  end)

let make : cnstrnt -> t =
  let ht = HashCnstrnt.create 17 in
  Tools.add_at_reset (fun () -> HashCnstrnt.clear ht);
  HashCnstrnt.hashcons ht


(*s Constructors. *)

let mk_bot = make(Bot)

let mk_top = make(Top)

let is_bot c = (c === mk_bot)

let is_top c = (c === mk_top)

let mk_number n =
  if Number.is_bot n then mk_bot else make(Number(n))

let mk_enumerative ns =
  if Name.Set.is_empty ns then
    mk_bot 
  else
    make(Enumeration(ns))

let mk_bitvector =
  let bv = make(Bitvector(None)) in
  function
    | None -> bv
    | Some(n) -> if n < 0 then mk_bot else make(Bitvector(Some(n)))


let d_enumerative c =
  match c.node with
    | Enumeration(ns) -> Some(ns)
    | _ -> None

let d_number c =
  match c.node with
    | Number(n) -> Some(n)
    | _ -> None

let d_bv c =
  match c.node with
    | Bitvector((Some _) as x) -> x
    | _ -> None


(*s Checks wether [c] is a subconstraint of [d]. *)

let rec sub c d =
  match c.node, d.node with
    | _, Top -> true
    | Bot, _ -> true 
    | Number(n), Number(m) -> 
	Number.sub n m
    | Enumeration(ns), Enumeration(ms) ->
	Name.Set.equal ns ms
    | Bitvector n, Bitvector m ->
	(match n, m with
	   | Some(x), Some(y) -> x = y
	   | None, None -> true
	   | _ -> false)
    | _ -> false


let rec inter c d =
  match c.node, d.node with
    | Top, _ -> d
    | _, Top -> c
    | Number(n), Number(m) -> 
	mk_number(Number.inter n m)
    | Enumeration(ns), Enumeration(ms) ->
	if Name.Set.equal ns ms then c else mk_bot 
    | Bitvector n, Bitvector m ->
	(match n, m with
	   | Some(n), Some(m) -> if n = m then c else mk_bot
	   | Some _, None -> c
	   | None, Some _ -> d
	   | None, None -> c)
    | _ ->  
	mk_bot

and union c d =
  match c.node, d.node with
    | Bot, _ -> d
    | _, Bot -> c
    | Number(n), Number(m) -> 
	mk_number(Number.union n m)
    | Enumeration(ns), Enumeration(ms) ->
	if Name.Set.equal ns ms then c else mk_top
    | Bitvector n, Bitvector m ->
	(match n, m with
	   | Some(x), Some(y) when x = y -> c 
	   | _ -> mk_bitvector None)
    | _ ->
	mk_top

let rec cmp c d =
  match c.node, d.node with
    | Top, Top -> Same
    | _, Top -> Sub
    | Top, _ -> Super
    | Bot, Bot -> Same
    | Bot, _ -> Sub
    | _, Bot -> Super
    | Number(n), Number(m) ->
	Number.cmp n m
    | Enumeration(ns), Enumeration(ms) -> 
	if Name.Set.equal ns ms then Same else Disjoint
    | Bitvector n, Bitvector m ->
	(match n, m with
	   | Some(x), Some(y) -> 
	       if x = y then Same else Disjoint
	   | Some _, None -> Sub
	   | None, Some _ -> Super
	   | None, None -> Same)
    | _ -> Disjoint

let analyze c =
  match c.node with
    | Top -> Status.Full
    | Bot -> Status.Empty
    | Number(n) -> Number.analyze n
    | _ -> Status.Other

let d_singleton c = 
  match c.node with
    | Number(n) -> Number.d_singleton n
    | _ -> None

(*s Checks wether two given lists are disjoint*)

let rec is_disjoint c d =
  match c.node, d.node with
    | Top, _ -> 
	false
    | _, Top -> 
	false
    | Number(n), Number(m) -> 
	Number.is_disjoint n m
    | Enumeration(ns), Enumeration(ms) ->
	not(Name.Set.equal ns ms)
    | Bitvector n, Bitvector m ->
	(match n, m with
	   | Some(x), Some(y) -> x <> y
	   | _ -> false)
    | _ -> 
	true

(*s Printing constraints. *)

let rec pp fmt c =
  Format.fprintf fmt "@[";
  (match c.node with
     | Bot ->
	 Format.fprintf fmt "bot"
     | Number(n) -> 
	 Number.pp fmt n
     | Enumeration(ns) -> 
	 Tools.ppl ("{", ",", "}") Name.pp fmt (Name.Set.elements ns)
     | Bitvector n ->
	 (match n with
	    | Some(x) -> Format.fprintf fmt "bv[%d]" x
	    | None ->  Format.fprintf fmt "bv")
     | Top -> 
	 Format.fprintf fmt "top");
  Format.printf "@]"


type typ = t  (* avoid type-check error below *)

module Set = Set.Make(
  struct
    type t = typ
    let compare = Pervasives.compare
  end)

module Map = Map.Make(
  struct
    type t = typ
    let compare = Pervasives.compare
  end)

(*s Derived constructorsl. *)

let mk_real = mk_number (Number.mk_real)
let mk_int = mk_number (Number.mk_int)
let mk_nat = mk_number (Number.mk_nat)

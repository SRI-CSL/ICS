
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
open Format
(*i*)

(*s Variables. *)

type t = 
  | External of Name.t
  | Internal of Name.t * int

let name_of = function
  | External(n) -> n
  | Internal(n, i) ->  
      let str = Format.sprintf "%s!%d" (Name.to_string n) i in
      Name.of_string str

let eq x y =
  match x, y with
    | External(n), External(m) -> 
	Name.eq n m
    | Internal(n,i), Internal(m,j) -> 
	Name.eq n m && i = j
    | _ -> false

let cmp x y =
  match x, y with
    | External _, Internal _ -> -1
    | Internal _, External _ -> 1
    | External(n), External(m) -> 
	Name.cmp n m
    | Internal(n, i), Internal(m, j) -> 
	let c1 = Name.cmp n m in
	if c1 != 0 then c1 else Pervasives.compare i j

let (<<<) x y = (cmp x y <= 0)


(*s Sets and maps of terms. *)

type var = t

module Set = Set.Make(
  struct
    type t = var
    let compare = cmp
  end)

module Map = Map.Make(
  struct
    type t = var
    let compare = cmp
  end)

(*s Constructors. *)

let mk_var x = External(x)

let k = ref 0
let _ = Tools.add_at_reset (fun () -> k := 0)

let mk_fresh x = function
  | Some(k) -> 
      Internal(x, k)
  | None ->
      incr(k);
      Internal(x, !k)

let mk_slack =
  let slackname = Name.of_string "k" in
    mk_fresh slackname

(*s Recognizers. *)

let is_var = function External _ -> true | _ -> false
let is_fresh = function Internal _ -> true | _ -> false

let is_slack =
  let slackname = Name.of_string "k" in
    function
      | Internal(k, _) -> 
	  Name.eq k slackname
      | _ ->
	  false

(*s Printer. *)

let pp fmt = function
  | External(n) -> Name.pp fmt n
  | Internal(n,i) -> Format.fprintf fmt "%s!%d" (Name.to_string n) i

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

open Mpa
open Format

type t = 
  | External of Name.t
  | Fresh of Name.t * int
  | Bound of int

let name_of = function
  | External(n) -> n
  | Fresh(n, i) ->  
      let str = Format.sprintf "%s!%d" (Name.to_string n) i in
	Name.of_string str
  | Bound(n) ->
      let str = Format.sprintf "!%d" n in
	Name.of_string str

let eq x y =
  match x, y with
    | External(n), External(m) -> 
	Name.eq n m
    | Fresh(n,i), Fresh(m,j) -> 
	Name.eq n m && i = j
    | Bound(n), Bound(m) ->
	n = m
    | _ -> 
	false

let cmp x y =
  match x, y with
    | External _, Fresh _ -> -1
    | Fresh _, External _ -> 1
    | External(n), External(m) -> 
	Name.cmp n m
    | Fresh(n, i), Fresh(m, j) -> 
	let c1 = Name.cmp n m in
	if c1 != 0 then c1 else Pervasives.compare i j
    | Bound(n), Bound(m) ->
	Pervasives.compare n m
    | _ ->
	Pervasives.compare x y

let (<<<) x y = (cmp x y <= 0)


(** {6 Sets and maps of terms} *)

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


(** {6 Constructors} *)

let mk_var x = External(x)

let k = ref 0
let _ = Tools.add_at_reset (fun () -> k := 0)

let mk_fresh x = function
  | Some(k) -> 
      Fresh(x, k)
  | None ->
      incr(k);
      Fresh(x, !k)

let mk_free i = Bound(i)

(** {6 Recognizers} *)

let is_var = function External _ -> true | _ -> false
let is_fresh = function Fresh _ -> true | _ -> false
let is_free = function Bound _ -> true | _ -> false

let d_free = function
  | Bound(i) -> i
  | _ -> assert false


(** {6 Printer} *)

let pp fmt x =
  Name.pp fmt (name_of x)

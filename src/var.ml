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
 
type t =  
  | Slack of int * slack
  | External of Name.t * Dom.t option
  | Rename of Name.t * int * Dom.t option
  | Fresh of Th.t * int * Dom.t option
  | Bound of int

and slack = Zero | Nonneg of Dom.t
 

(** {6 Constructors} *)

let mk_external n d = External(n, d)
let mk_slack k sl = Slack(k, sl)
let mk_rename n k d = Rename(n, k, d)
let mk_fresh th k d = Fresh(th, k, d)
let mk_free k = Bound(k)


(** {6 Accessors} *)
    
let name_of = function
  | External(n, _) -> n
  | Rename(n, i, _) ->  
      Name.of_string (Format.sprintf "%s!%d" (Name.to_string n) i)
  | Slack(i, Zero) ->  
      Name.of_string (Format.sprintf "%s!%d" "k0" i)
  | Slack(i, Nonneg(d)) ->  
      Name.of_string (Format.sprintf "%s!%d" "k" i)
  | Fresh(th, i, _) -> 
      Name.of_string (Format.sprintf "%s!%d" (Th.to_string th) i)
  | Bound(n) ->
      Name.of_string (Format.sprintf "!%d" n)

let dom_of = function 
  | Slack(_, Zero) -> Dom.Int
  | Slack(_, Nonneg(d)) -> d
  | External(_, Some(d)) -> d
  | Fresh(_, _, Some(d)) -> d
  | Rename(_, _, Some(d)) -> d
  | _ -> raise Not_found


let is_int x = 
  try dom_of x = Dom.Int with Not_found -> false

let is_real x =
  try dom_of x = Dom.Real with Not_found -> false



(** {6 Variable ordering} *)

let domcmp d e =
  match d, e with
    | None, None -> 0
    | Some _, None -> -1
    | None, Some _ -> 1
    | Some d, Some e -> Dom.cmp d e

(** slack < external < fresh < renaming < bound *)
let rec cmp x y =
  match x, y with
    | Slack(i, sl1), Slack(j, sl2) ->
	(match sl1, sl2 with
	   | Zero, Zero -> 
	       Pervasives.compare i j
	   | Zero, Nonneg _ -> -1
	   | Nonneg _, Zero -> 1
	   | Nonneg(d), Nonneg(e) ->
	       let c1 = Dom.cmp d e in
		 if c1 != 0 then c1 else
		   Pervasives.compare i j)
    | Slack _, _ ->  -1
    | _, Slack _ -> 1
    | External(n, d), External(m, e) -> 
	let c1 = domcmp d e in
	  if c1 != 0 then c1 else Name.compare n m
    | External _, _ -> -1
    | _, External _ -> 1
    | Fresh(n, i, d), Fresh(m, j, e) ->
	let c1 = domcmp d e in
	  if c1 != 0 then c1 else 
	    let c2 = Pervasives.compare i j in
	      if c2 != 0 then c2 else Pervasives.compare n m
    | Fresh _, _ -> -1
    | _, Fresh _ -> 1
    | Rename(n, i, d), Rename(m, j, e) -> 
	let c1 = domcmp d e in
	  if c1 != 0 then c1 else 
	    let c2 = Pervasives.compare i j in
	      if c2 != 0 then c2 else Name.compare n m
    | Rename _, _ -> -1
    | Bound(i), Bound(j) -> 
	Pervasives.compare i j
    | Bound _ , _ -> 1


let (<<<) x y = (cmp x y <= 0)

     
(** {6 Recognizers} *)

let is_var = function External _ -> true | _ -> false
let is_rename = function Rename _ -> true | _ -> false
let is_free = function Bound _ -> true | _ -> false
let is_slack sl = function Slack(_, sl') when sl = sl' -> true | _ -> false
let is_nonneg_slack = function Slack(_, Nonneg _) -> true | _ -> false
let is_zero_slack = function Slack(_, Zero) -> true | _ -> false
let is_fresh th = function Fresh(th', _, _) when th = th' -> true | _ -> false

let is_internal = function
  | Slack _ -> true
  | Rename _ -> true
  | Fresh _ -> true
  | _ -> false

let d_free = function
  | Bound(i) -> i
  | _ -> assert false


(** {6 Pretty Printer} *)

let pretty = ref true

let pp fmt x =
  Name.pp fmt (name_of x);
  if not(!pretty) then
    (try 
       let d = dom_of x in
	 Pretty.string fmt "{";
	 Dom.pp fmt d;
	 Pretty.string fmt "}"
     with
	 Not_found -> ())

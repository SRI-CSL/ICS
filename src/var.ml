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
  | External of Name.t * dom 
  | Rename of Name.t * int * dom
  | Fresh of Name.t * int * dom
  | Slack of int * bool * dom
  | Bound of int

and dom = Dom.t option 

let name_of = function
  | External(n, _) -> n
  | Rename(n, i, _) ->  
      let str = Format.sprintf "%s!%d" (Name.to_string n) i in
	Name.of_string str
  | Slack(i, alpha, _) ->
      let str = Format.sprintf "%s!%d" (if alpha then "k" else "l") i in
	Name.of_string str
  | Fresh(n, i, _) ->
      let str = Format.sprintf "%s!%d" (Name.to_string n) i in
	Name.of_string str
  | Bound(n) ->
      let str = Format.sprintf "!%d" n in
	Name.of_string str

let dom_of = function
  | External(_,d) -> d
  | Rename(_,_,d) -> d
  | Slack(_,_,d) -> d
  | Fresh(_,_,d) -> d
  | Bound _ -> None

let eq x y =
  match x, y with
    | External(n, d), External(m, e) -> 
	Name.eq n m && d = e
    | Rename(n, i, d), Rename(m, j, e) -> 
	Name.eq n m && i = j && d = e
    | Fresh(n, i, d), Fresh(m, j, e) -> 
	Name.eq n m && i = j && d = e
    | Bound(n), Bound(m) ->
	n = m
    | Slack(i, alpha, d), Slack(j, beta, e) ->
	i = j && alpha = beta && d = e
    | _ -> 
	false

let cmp x y =
  let domcmp d e =
    match d, e with
      | None, None -> 0
      | Some _, None -> -1
      | None, Some _ -> 1
      | Some d, Some e ->
	  (match d, e with   
	     | Dom.Real, Dom.Real -> 0
	     | Dom.Int, Dom.Int -> 0
	     | Dom.Real, Dom.Int -> 1    (* real-valued variables are larger *)
	     | Dom.Int, Dom.Real -> -1)
  in 
  let strictcmp alpha beta =    
    match alpha, beta with
      | true, false -> -1           (* nonstrict slacks are smaller *)
      | false, true -> 1
      | _ -> 0
  in
    match x, y with
      | External _, Rename _ -> -1   (* external variables are smaller than renaming vars. *)
      | Rename _, External _ -> 1
      | External(n, d), External(m, e) -> 
	  let c1 = domcmp d e in
	    if c1 != 0 then c1 else Name.cmp n m
      | Rename(n, i, d), Rename(m, j, e) -> 
	  let c1 = domcmp d e in
	    if c1 != 0 then c1 else 
	      let c2 = Name.cmp n m  in
		if c2 != 0 then c2 else Pervasives.compare i j
      | Fresh(n, i, d), Fresh(m, j, e) -> 
	  let c1 = domcmp d e in
	    if c1 != 0 then c1 else 
	      let c2 = Name.cmp n m  in
		if c2 != 0 then c2 else Pervasives.compare i j
      | Slack(i, alpha, d), Slack(j, beta, e) ->  
	  let c1 = strictcmp alpha beta in
	    if c1 != 0 then c1 else 
	      let c2 = domcmp d e in   (* newer slack vars are smaller *)
		if c2 != 0 then c2 else -(Pervasives.compare i j)
      | Slack _, _ -> -1           (* slacks are smaller than other variables. *)
      | _, Slack _ -> 1
      | External _, Fresh _ -> 1   (* external variables are larger than fresh vars *)
      | Fresh _, External _ -> -1
      | Bound(n), Bound(m) -> 
	  Pervasives.compare n m
      | _ -> 
	  Pervasives.compare x y

let (<<<) x y = (cmp x y <= 0)

let hash = function
  | External(n, _) ->
      (3 + Hashtbl.hash n) land 0x3FFFFFFF
  | Rename(n, i, _) ->
      (5 + Hashtbl.hash n + i) land 0x3FFFFFFF
  | Bound(i) ->
      (7 + i) land 0x3FFFFFFF
  | Slack(i,_, _) ->
      (11 + i) land 0x3FFFFFFF 
  | Fresh(n, i, _) ->
      (17 + Hashtbl.hash n + i) land 0x3FFFFFFF
     

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

let mk_var x d = External(x, d)

let k = ref 0
let _ = Tools.add_at_reset (fun () -> k := 0)

let mk_rename x i d =
  match i with
    | Some(k) -> 
	Rename(x, k, d)
    | None ->
	incr(k);
	Rename(x, !k, d)

let mk_fresh x i d =
  match i with
    | Some(k) -> 
	Fresh(x, k, d)
    | None ->
	incr(k);
	Fresh(x, !k, d)

let mk_free i = Bound(i)

let mk_slack i alpha d =
  match i with
    | Some(k) -> 
	Slack(k, alpha, d)
    | None ->
	incr(k);
	Slack(!k, alpha, d)

(** {6 Recognizers} *)

let is_var = function External _ -> true | _ -> false
let is_rename = function Rename _ -> true | _ -> false
let is_fresh = function Rename _ -> true | _ -> false
let is_free = function Bound _ -> true | _ -> false
let is_slack = function Slack _ -> true | _ -> false


let d_free = function
  | Bound(i) -> i
  | _ -> assert false

(** {6 Printer} *)

let pretty = ref false

let pp fmt x =
  Name.pp fmt (name_of x);
  (if not !pretty then
    (match dom_of x with
       | Some(d) -> 
	   Pretty.string fmt "{";
	   Dom.pp fmt d;
	   Pretty.string fmt "}"
       | None -> ()))



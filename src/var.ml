
open Mpa
open Format

   
type t =  
  | Slack of int * slack
  | External of Name.t * Dom.t option
  | Rename of Name.t * int * Dom.t option
  | Fresh of Th.t * int * Dom.t option
  | Bound of int                       (* Bound variables for quantification *)

and slack = Zero | Nonneg of Dom.t
     
and var = t

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

(* slack < external < fresh < renaming < bound *)
let rec cmp x y =
  match x, y with
    | Slack(i, Zero), Slack(j, Zero) ->
	Pervasives.compare j i
    | Slack(_, Zero), Slack(j, Nonneg _) -> -1
    | Slack(_, Nonneg _), Slack(j, Zero) -> 1
    | Slack(i, Nonneg(d)), Slack(j, Nonneg(e)) ->
	let c1 = Dom.cmp d e in
	  if c1 != 0 then c1 else
	    Pervasives.compare j i
    | Slack _, _ ->  -1
    | _, Slack _ -> 1
    | External(n, d), External(m, e) -> 
	let c1 = domcmp d e in
	  if c1 != 0 then c1 else Name.cmp n m
    | External _, _ -> -1
    | Fresh(n, i, d), Fresh(m, j, e) ->
	let c1 = domcmp d e in
	  if c1 != 0 then c1 else 
	    let c2 = Pervasives.compare i j in
	      if c2 != 0 then c2 else Pervasives.compare n m
    | Fresh _, _ -> -1
    | Rename(n, i, d), Rename(m, j, e) -> 
	let c1 = domcmp d e in
	  if c1 != 0 then c1 else 
	    let c2 = Pervasives.compare i j in
	      if c2 != 0 then c2 else Name.cmp n m
    | Rename _, _ -> -1
    | Bound(i), Bound(j) -> 
	Pervasives.compare i j
    | Bound _ , _ -> 1

and domcmp d e =
  match d, e with
    | None, None -> 0
    | Some _, None -> -1
    | None, Some _ -> 1
    | Some d, Some e -> Dom.cmp d e


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

(** {6 Sets and maps of terms} *)


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


(** {6 Hashing} *)

let hash = function
  | External(n, _) -> (3 + Hashtbl.hash n) land 0x3FFFFFFF
  | Rename(n, i, _) -> (5 + i + Hashtbl.hash n) land 0x3FFFFFFF
  | Bound(i) -> (7 + i) land 0x3FFFFFFF
  | Slack(n, Zero) -> (11 + Hashtbl.hash n) land 0x3FFFFFFF 
  | Slack(n, Nonneg(d)) -> (17 + Hashtbl.hash d) land 0x3FFFFFFF 
  | Fresh(n, i, _) -> (21 + i + Hashtbl.hash n) land 0x3FFFFFFF 

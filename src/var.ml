
open Mpa
open Format

   
type t =  
  | Slack of int * Dom.t * slack
  | External of Name.t * Dom.t option
  | Rename of Name.t * int * Dom.t option
  | Fresh of Th.t * int * Dom.t option
  | Bound of int                       (* Bound variables for quantification *)

and slack = Zero | Nonneg
     
and var = t

(** {6 Constructors} *)

let mk_external n d = External(n, d)
let mk_slack k d m = Slack(k, d, m)
let mk_rename n k d = Rename(n, k, d)
let mk_fresh th k d = Fresh(th, k, d)
let mk_free k = Bound(k)


(** {6 Accessors} *)
    
let name_of = function
  | External(n, _) -> n
  | Rename(n, i, _) ->  
      Name.of_string (Format.sprintf "%s!%d" (Name.to_string n) i)
  | Slack(i, d, Zero) ->  
      Name.of_string (Format.sprintf "%s!%d" "k0" i)
  | Slack(i, d, Nonneg) ->  
      Name.of_string (Format.sprintf "%s!%d" "k" i)
  | Fresh(th, i, _) -> 
      Name.of_string (Format.sprintf "%s!%d" (Th.to_string th) i)
  | Bound(n) ->
      Name.of_string (Format.sprintf "!%d" n)

let dom_of = function 
  | Slack(_, d, _) -> d
  | External(_, Some(d)) -> d
  | Fresh(_, _, Some(d)) -> d
  | Rename(_, _, Some(d)) -> d
  | _ -> raise Not_found


let is_int x = 
  try dom_of x = Dom.Int with Not_found -> false

let is_real x =
  try dom_of x = Dom.Real with Not_found -> false



(** {6 Variable ordering} *)

(* slack < external < fresh < renaming *)
let rec cmp x y =
  let c1 = domcmp x y in
    if c1 <> 0 then c1 else
      (match x, y with       
	 | Fresh _, Rename _ -> -1
	 | Rename _, Fresh _ -> 1
	 | External _, (Rename _ | Fresh _) -> -1 
	 | (Rename _ | Fresh _), External _ -> 1
	 | Slack _, (Rename _ | External _ | Fresh _) -> -1
	 | (Rename _ | External _ | Fresh _), Slack _ -> 1  
	 | Rename(n, i, _), Rename(m, j, _) -> 
	     let c2 = Pervasives.compare i j in
	       if c2 != 0 then c2 else Name.cmp n m
	 | External(n, _), External(m, _) -> 
	     Name.cmp n m
	 | Slack(i, _, m), Slack(j, _, n) ->
	     (match m, n with
		| Zero, Nonneg -> -1
		| Nonneg, Zero -> 1
		| _ -> Pervasives.compare j i)
	 | Fresh(n, i, _), Fresh(m, j, _) ->
	     let c1 = -(Pervasives.compare i j) in
	       if c1 != 0 then c1 else Pervasives.compare n m
	 | _ -> 
	     Pervasives.compare x y)

and domcmp x y =
  let dom = function 
    | Slack(_, d, _) -> Some(d)
    | External(_, d) -> d
    | Fresh(_, _, d) -> d
    | Rename(_, _, d) -> d
    | Bound _ -> None
  in
    match dom x, dom y with
      | None, None -> 0
      | Some _, None -> -1
      | None, Some _ -> 1
      | Some d, Some e -> Dom.cmp d e


let (<<<) x y = (cmp x y <= 0)

     
(** {6 Recognizers} *)

let is_var = function External _ -> true | _ -> false
let is_rename = function Rename _ -> true | _ -> false
let is_free = function Bound _ -> true | _ -> false
let is_slack m = function Slack(_, _, m') when m = m' -> true | _ -> false
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
  | Slack(n, d, Zero) -> (11 + Hashtbl.hash d) land 0x3FFFFFFF 
  | Slack(n, d, Nonneg) -> (17 + Hashtbl.hash d) land 0x3FFFFFFF 
  | Fresh(n, i, _) -> (21 + i + Hashtbl.hash n) land 0x3FFFFFFF 

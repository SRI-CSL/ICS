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
 * 
 * Author: Harald Ruess
*)

open Mpa

type uninterp = Name.t

type arith = 
  | Num of Mpa.Q.t  
  | Add
  | Multq of Mpa.Q.t

type pair = Cons | Car | Cdr
      (** Function symbols for the theory of S-expressions. *)

type coproduct = InL | InR | OutL | OutR

type bv =
  | Const of Bitv.t
  | Conc of int * int
  | Sub of int * int * int

type pprod = 
  | Mult
  | Expt of int

type apply = 
  | Apply of Dom.t option
  | Abs

type arrays = 
  | Create
  | Select 
  | Update


type sym = 
  | Uninterp of uninterp
  | Arith of arith
  | Pair of pair
  | Coproduct of coproduct
  | Bv of bv
  | Pp of pprod
  | Fun of apply
  | Arrays of arrays 

type t = sym * int

let hash (_, hsh) = hsh


module Uninterp = struct

  let get = function 
    | Uninterp(op), _ -> op
    | _ -> raise Not_found

  let uninterp =
    let table = Name.Hash.create 17 in
    let _ =  Tools.add_at_reset (fun () -> Name.Hash.clear table) in
      fun n ->
	try
	  Name.Hash.find table n
	with
	    Not_found ->
	      let hsh = (2 + Name.hash n) land 0x3FFFFFFF in
	      let f = (Uninterp(n), hsh) in
		Name.Hash.add table n f; f

end

module Arith = struct

  type t = arith

  let get = function 
    | Arith(op), _ -> op
    | _ -> raise Not_found

  let num = 
    let table = Mpa.Q.Hash.create 17 in
    let _ =  Tools.add_at_reset (fun () -> Mpa.Q.Hash.clear table) in
      fun q ->
	try
	  Mpa.Q.Hash.find table q 
	with
	    Not_found ->
	      let hsh = (3 + Mpa.Q.hash q) land 0x3FFFFFFF in
	      let c = (Arith(Num(q)), hsh) in
		Mpa.Q.Hash.add table q c; c

  let multq =
    let table = Mpa.Q.Hash.create 17 in
    let _ =  Tools.add_at_reset (fun () -> Mpa.Q.Hash.clear table) in
      fun q ->
	try
	  Mpa.Q.Hash.find table q 
	with
	    Not_found ->
	      let hsh = (5 + Mpa.Q.hash q) land 0x3FFFFFFF in
	      let c = (Arith(Multq(q)), hsh) in
		Mpa.Q.Hash.add table q c; c

  let add = (Arith(Add), 77)

  let d_num = function
    | Arith(Num(q)), _ -> q
    | _ -> raise Not_found

  let d_multq = function
    | Arith(Multq(q)), _ -> q
    | _ -> raise Not_found


  let pp fmt = function
    | Num(q) -> Mpa.Q.pp fmt q 
    | Add -> Format.fprintf fmt "+"
    | Multq(q) ->  Mpa.Q.pp fmt q; Format.fprintf fmt "*" 

  let args pp fmt = function
    | Num q, [] -> Mpa.Q.pp fmt q
    | Add, [] -> Format.fprintf fmt "0"
    | Add, al -> Pretty.infixl pp " + " fmt al
    | Multq(q) , [x] -> Pretty.infix Mpa.Q.pp "*" pp fmt (q, x)
    | _ -> raise (Invalid_argument "not interpreted in arithmetic")
	  
end

module Pair = struct

  let get = function 
    | Pair(op), _ -> op
    | _ -> raise Not_found

  let cons = (Pair(Cons), 371)
  let car = (Pair(Car), 379)
  let cdr = (Pair(Cdr), 381)

  let pp fmt = function
    | Cons -> Format.fprintf fmt "cons"
    | Car -> Format.fprintf fmt "car"
    | Cdr -> Format.fprintf fmt "cdr"
end

module Coproduct = struct

  let get = function 
    | Coproduct(op), _ -> op
    | _ -> raise Not_found

  let inl = (Coproduct(InL), 177)
  let inr = (Coproduct(InR), 183)
  let outl = (Coproduct(OutL), 191)
  let outr = (Coproduct(OutR), 193)

  let pp fmt = function
    | InL -> Format.fprintf fmt "inl"
    | InR ->  Format.fprintf fmt "inr"
    | OutL -> Format.fprintf fmt "outl"
    | OutR -> Format.fprintf fmt "outr"

end

module Pprod = struct

  let get = function
    | Pp(op), _ -> op
    | _ -> raise Not_found
  
  let mult = (Pp(Mult), 731)

  let expt = 
    let table = Hashtbl.create 17 in
    let _ =  Tools.add_at_reset (fun () -> Hashtbl.clear table) in
      fun n ->
	try
	  Hashtbl.find table n
	with
	    Not_found ->
	      let hsh = (17 + n) land 0x3FFFFFFF in
	      let op = (Pp(Expt(n)), hsh) in
		Hashtbl.add table n op; op

  let is_expt (sym, _) =
    match sym with
      | Pp(Expt _) -> true
      | _ -> false

  let d_expt (sym, _) = 
    match sym with
      | Pp(Expt(n)) -> n
      | _ -> raise Not_found

  let pp fmt = function
    | Mult -> Format.fprintf fmt "."
    | Expt(n) -> Format.fprintf fmt "^%d" n

end 

module Bv = struct

  let get = function 
    | Bv(op), _ -> op
    | _ -> raise Not_found

  module BitvHash = Hashtbl.Make(
    struct
      type t = Bitv.t
      let equal = Bitv.equal
      let hash = Hashtbl.hash
    end )

  let const =
    let table = BitvHash.create 17 in
    let _ =  Tools.add_at_reset (fun () -> BitvHash.clear table) in
      fun b ->
	try
	  BitvHash.find table b
	with
	    Not_found ->
	      let hsh = Hashtbl.hash b in
	      let op = (Bv(Const(b)), hsh) in
		BitvHash.add table b op; op

  let conc = 
    let table = Hashtbl.create 17 in
    let _ =  Tools.add_at_reset (fun () -> Hashtbl.clear table) in
      fun n m ->
	try
	  Hashtbl.find table (n, m)
	with
	    Not_found ->
	      let hsh = (542 + n + m) land 0x3FFFFFFF in
	      let op = (Bv(Conc(n, m)), hsh) in
		Hashtbl.add table (n, m) op; op

  let sub = 
    let table = Hashtbl.create 17 in
    let _ =  Tools.add_at_reset (fun () -> Hashtbl.clear table) in
      fun n i j ->
	try
	  Hashtbl.find table (n, i, j)
	with
	    Not_found ->
	      let hsh = (97 + n + i + j) land 0x3FFFFFFF in
	      let op = (Bv(Sub(n, i, j)), hsh) in
		Hashtbl.add table (n, i, j) op; op

  let pp fmt = function
    | Const(b) -> Format.fprintf fmt "0b%s" (Bitv.to_string b)
    | Conc(n,m) -> Format.fprintf fmt "conc[%d,%d]" n m
    | Sub(n,i,j) -> Format.fprintf fmt "sub[%d,%d,%d]" n i j

  let width b =
    match b with
      | Const(c) ->
	  Bitv.length c
      | Sub(n,i,j) ->
	  assert(0 <= i && i <= j && j < n);
	  j-i+1
      | Conc(n,m) -> 
	  assert(0 <= n && 0 <= m);
	  n + m

end

module Array = struct

  let get = function 
    | Arrays(op), _ -> op
    | _ -> raise Not_found

  let create = (Arrays(Create), 27)
  let update = (Arrays(Update), 47)
  let select = (Arrays(Select), 65)

  let pp fmt = function
    | Create -> Format.fprintf fmt "create"
    | Select -> Format.fprintf fmt "select"  
    | Update -> Format.fprintf fmt "update"

end

module Fun = struct

  let get = function 
    | Fun(op), _ -> op
    | _ -> raise Not_found

  let abs = (Fun(Abs), 111)

  let apply = 
    let apply0 = (Fun(Apply(None)), 115) 
    and apply_int = (Fun(Apply(Some(Dom.Int))), 117)
    and apply_nonint = (Fun(Apply(Some(Dom.Nonint))), 119)
    and apply_real = (Fun(Apply(Some(Dom.Real))), 123) in
      function
	| None -> apply0
	| Some(d) ->
	    (match d with
	       | Dom.Int -> apply_int
	       | Dom.Nonint -> apply_nonint
	       | Dom.Real -> apply_real)

  let pp fmt = function
    | Apply(Some(c)) -> 
	Pretty.string fmt ("apply[" ^ Pretty.to_string Dom.pp c ^ "]")
    | Apply(None) ->
	Pretty.string fmt "apply"
    | Abs -> 
	Pretty.string fmt "lambda"

  let is_abs = function
    | Fun(Abs), _ -> true
    | _ -> false

  let is_apply = function
    | Fun(Apply _), _ -> true
    | _ -> false

end

let eq = (==)

let cmp f g =
  if f == g then 0 else if hash f < hash g then -1 else 1

let pp fmt (sym, _) =
  match sym with
    | Uninterp(f) -> Name.pp fmt f
    | Arith(op) -> Arith.pp fmt op
    | Pair(op) -> Pair.pp fmt op
    | Bv(op) -> Bv.pp fmt op
    | Coproduct(op) -> Coproduct.pp fmt op
    | Arrays(op) -> Array.pp fmt op
    | Pp(op) -> Pprod.pp fmt op
    | Fun(op) -> Fun.pp fmt op


let to_string = Pretty.to_string pp



(** {6 Pretty printing} *)

let pretty = ref true  (* Infix/Mixfix output when [pretty] is true. *)

let pp p fmt (f, l) =
  let str = Pretty.string fmt in
  let arg = p fmt in
  let sym f = str (to_string f) in
  let args =  Pretty.tuple p fmt in
  let app f l = sym f; Pretty.tuple p fmt l in
  let infixl x = Pretty.infixl p x fmt in
    if not(!pretty) then
      app f l 
    else 
      match fst(f), l with
	| Arith(Num q), [] -> 
	    Mpa.Q.pp fmt q
	| Arith(Add), _ -> 
	    infixl " + " l
	| Arith(Multq(q)) , [x] -> 
	    Pretty.infix Mpa.Q.pp "*" p fmt (q, x)  
	| Pp(Mult), [] ->
	    str "1"
	| Pp(Mult), xl ->
	    infixl "*" xl
	| Pp(Expt _), [x] ->
	    arg x; sym f
	| Bv(Const(b)), [] -> 
	    str ("0b" ^ Bitv.to_string b)
	| Bv(Conc _), l -> 
	    infixl " ++ " l
	| Bv(Sub(_,i,j)), [x] ->
	    arg x; Format.fprintf fmt "[%d:%d]" i j
	| Arrays(Update), [x;y;z] ->
	    arg x; str "["; arg y; str " := "; arg z; str "]"
	| Arrays(Select), [x; y] ->
	    arg x; str "["; arg y; str "]"
	| _ -> 
	    app f l


let theory_of (sym, _) =
  match sym with
    | Uninterp _ -> Th.u
    | Arith _ -> Th.a
    | Pair _ -> Th.p
    | Bv _ -> Th.bv
    | Coproduct _ -> Th.cop
    | Arrays _ -> Th.arr
    | Pp _ -> Th.nl
    | Fun _ -> Th.app

let get (sym, _) = sym

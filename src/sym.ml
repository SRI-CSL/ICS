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

type t = sym * int
    (** A symbol consists of the the symbol itself together with
      a hash value, which may not be unique to the symbol. *)
   
and sym = 
  | Uninterp of uninterp
  | Arith of arith
  | Product of product
  | Coproduct of coproduct
  | Bv of bv
  | Pp of pprod
  | Fun of apply
  | Arrays of arrays 

and uninterp = Name.t

and arith = 
  | Num of Mpa.Q.t  
  | Add
  | Multq of Mpa.Q.t

and product = Cons | Car | Cdr

and coproduct = InL | InR | OutL | OutR

and bv =
  | Const of Bitv.t
  | Conc of int * int
  | Sub of int * int * int

and pprod = 
  | Mult
  | Expt of int

and apply = 
  | Apply of Dom.t option
  | Abs

and arrays = Create | Select | Update


let hash (_, hsh) = hsh

let get (sym, _) = sym

let theory_of (sym, _) =
  match sym with
    | Uninterp _ -> Th.u
    | Arith _ -> Th.a
    | Product _ -> Th.p
    | Bv _ -> Th.bv
    | Coproduct _ -> Th.cop
    | Arrays _ -> Th.arr
    | Pp _ -> Th.nl
    | Fun _ -> Th.app


module Uninterp = struct

  let get = function 
    | Uninterp(op), _ -> op
    | _ -> raise Not_found

  let make =
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

  let is = function Uninterp _ -> true | _ -> false

  let pp p fmt (f, al) = 
    Pretty.apply p fmt (Name.to_string f, al)

end

module Arith = struct

  let get = function 
    | Arith(op), _ -> op
    | _ -> raise Not_found

  let mk_num = 
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

  let mk_multq =
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

  let mk_add = (Arith(Add), 77)

  let is_num = function Arith(Num _), _ -> true | _ -> false
  let is_multq = function Arith(Multq _), _ -> true | _ -> false
  let is_add = function Arith(Add), _ -> true | _ -> false

  let d_num = function
    | Arith(Num(q)), _ -> q
    | _ -> raise Not_found

  let d_multq = function
    | Arith(Multq(q)), _ -> q
    | _ -> raise Not_found

  let pp p fmt = function
    | Num q, [] -> 
	Mpa.Q.pp fmt q
    | Add, [] -> 
	Pretty.string fmt "0"
    | Add, al -> 
	Pretty.infixl p " + " fmt al
    | Multq(q) , [x] -> 
	Pretty.infix Mpa.Q.pp "*" p fmt (q, x)
    | _ -> 
	invalid_arg "Ill-formed application in linear arithmetic"
	  
end

module Product = struct

  let get = function 
    | Product(op), _ -> op
    | _ -> raise Not_found

  let mk_cons = (Product(Cons), 371)
  let mk_car = (Product(Car), 379)
  let mk_cdr = (Product(Cdr), 381)

  let is_cons = function Product(Cons), _ -> true | _ -> false
  let is_car = function Product(Car), _ -> true | _ -> false
  let is_cdr = function Product(Cdr), _ -> true | _ -> false
 
  let pp p fmt = function
    | Cons, [a; b] -> 
	Pretty.apply p fmt ("cons", [a; b])
    | Car, [a] -> 
	Pretty.apply p fmt ("car", [a])
    | Cdr, [a] -> 
	Pretty.apply p fmt ("cdr", [a])
    | _ ->
	invalid_arg "Ill-formed application in product theory"
end

module Coproduct = struct

  let get = function 
    | Coproduct(op), _ -> op
    | _ -> raise Not_found

  let mk_inl = (Coproduct(InL), 177)
  let mk_inr = (Coproduct(InR), 183)
  let mk_outl = (Coproduct(OutL), 191)
  let mk_outr = (Coproduct(OutR), 193)

  let is = function Coproduct _, _ -> true | _ -> false
  let is_inl = function Coproduct(InL), _ -> true | _ -> false
  let is_inr = function Coproduct(InR), _ -> true | _ -> false
  let is_outl = function Coproduct(OutL), _ -> true | _ -> false
  let is_outr = function Coproduct(OutR), _ -> true | _ -> false

  let pp p fmt = function
    | InL, [a] -> 
	Pretty.apply p fmt ("inl", [a])
    | InR, [a] -> 
	Pretty.apply p fmt ("inr", [a])
    | OutL, [a] -> 
	Pretty.apply p fmt ("outl", [a])
    | OutR, [a] -> 
	Pretty.apply p fmt ("outr", [a])
    | _ -> 
	invalid_arg "Ill-formed application in coproduct theory"

end

module Pprod = struct

  let get = function
    | Pp(op), _ -> op
    | _ -> raise Not_found
  
  let mk_mult = (Pp(Mult), 731)

  let mk_expt = 
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

  let is = function Pp _, _ -> true | _ -> false
  let is_expt = function Pp(Expt _), _ -> true | _ -> false
  let is_mult = function Pp(Mult), _ -> true | _ -> false

  let d_expt (sym, _) = 
    match sym with
      | Pp(Expt(n)) -> n
      | _ -> raise Not_found

  let pp p fmt = 
    function
      | Mult, [] ->
	  Pretty.string fmt "1"
      | Mult, al ->
	  Pretty.infixl p "." fmt al
      | Expt(n), [a] ->
	  let op = Format.sprintf "^%d" n in
	    p fmt a; Pretty.string fmt op
      | _ ->
	  invalid_arg "Ill-formed application in the theory of products"

end 

module Bv = struct

  let get = function 
    | Bv(op), _ -> op
    | _ -> raise Not_found

  let mk_const =
    let module BitvHash = Hashtbl.Make(
      struct
	type t = Bitv.t
	let equal = Bitv.equal
	let hash = Hashtbl.hash
      end) 
    in
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

  let mk_conc = 
    let table = Hashtbl.create 17 in
    let _ =  Tools.add_at_reset (fun () -> Hashtbl.clear table) in
      fun n m ->
	assert(0 <= n && 0 <= m);
	try
	  Hashtbl.find table (n, m)
	with
	    Not_found ->
	      let hsh = (542 + n + m) land 0x3FFFFFFF in
	      let op = (Bv(Conc(n, m)), hsh) in
		Hashtbl.add table (n, m) op; op

  let mk_sub = 
    let table = Hashtbl.create 17 in
    let _ =  Tools.add_at_reset (fun () -> Hashtbl.clear table) in
      fun n i j ->
	assert(0 <= i && i <= j && j < n);
	try
	  Hashtbl.find table (n, i, j)
	with
	    Not_found ->
	      let hsh = (97 + n + i + j) land 0x3FFFFFFF in
	      let op = (Bv(Sub(n, i, j)), hsh) in
		Hashtbl.add table (n, i, j) op; op

  let is = function Bv _, _ -> true | _ -> false
  let is_const = function Bv(Const _), _ -> true | _ -> false
  let is_conc = function Bv(Conc _), _ -> true | _ -> false
  let is_sub = function Bv(Sub _), _ -> true | _ -> false

  let d_const = function Bv(Const(b)), _ -> b | _ -> raise Not_found
  let d_conc = function Bv(Conc(n, m)), _ -> (n, m) | _ -> raise Not_found
  let d_sub = function Bv(Sub(n, i, j)), _ -> (n, i, j) | _ -> raise Not_found

  let pp p fmt (op, al) =
    match op, al with
      | Const(b), [] -> 
	  Format.fprintf fmt "0b%s" (Bitv.to_string b)
      | Conc(n, m), [a; b] -> 
	  (match !Pretty.flag with
	     | Pretty.Mixfix ->  
		 Pretty.infix p "++" p fmt (a, b)
	     | _ ->
		 let op = Format.sprintf "conc[%d,%d]" n m in
		   Pretty.apply p fmt (op, [a; b]))
      | Sub(n, i, j), [a] -> 
	  (match !Pretty.flag with
	     | Pretty.Mixfix ->  
		 let op = Format.sprintf "[%d:%d]" i j in
		   p fmt a; Pretty.string fmt op
	     | _ ->
	  	 let op = Format.sprintf "sub[%d,%d,%d]" n i j in
		   Pretty.apply p fmt (op, [a]))
      | _ ->
	  invalid_arg "Ill-formed application in bitvector theory"

  let width b =
    match b with
      | Const(c) -> Bitv.length c
      | Sub(n, i, j) -> j - i + 1
      | Conc(n, m) -> n + m

end

module Array = struct

  let get = function 
    | Arrays(op), _ -> op
    | _ -> raise Not_found

  let mk_create = (Arrays(Create), 27)
  let mk_update = (Arrays(Update), 47)
  let mk_select = (Arrays(Select), 65)

  let is = function Arrays _, _ -> true | _ -> false
  let is_create = function Arrays(Create), _ -> true | _ -> false
  let is_update = function Arrays(Update), _ -> true | _ -> false
  let is_select = function Arrays(Update), _ -> true | _ ->false

  let pp p fmt (op, al) =
    let arg = p fmt and str = Pretty.string fmt in
      match (op, al) with
	| Create, [a] ->
	    Pretty.apply p fmt ("create", [a])
	| Select, [a; j] -> 
	    (match !Pretty.flag with
	       | Pretty.Mixfix -> 
		   arg a; str "["; arg j; str "]"
	       | _ -> 
		   Pretty.apply p fmt ("select", [a; j]))
	| Update, [a; i; x] -> 
	    (match !Pretty.flag with
	       | Pretty.Mixfix -> 
		   arg a; str "["; arg i; str " := "; arg x; str "]"
	       | _ -> 
		   Pretty.apply p fmt ("update", [a; i; x]))
	| _ ->
	    invalid_arg "Ill-formed application in theory of arrays"
	    
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

  let pp p fmt = 
    function
      | Apply(Some(c)), [a; b] -> 
	  let op = Format.sprintf "apply[" ^ Pretty.to_string Dom.pp c ^ "]" in
	    Pretty.apply p fmt (op, [a; b])
      | Apply(None), [a; b] ->
	  (match !Pretty.flag with
	     | Pretty.Mixfix -> 
		 Pretty.infix p " $ " p fmt (a, b)
	     | _ ->
		 Pretty.apply p fmt ("apply",  [a; b]))
      | Abs, [a] -> 
	  Pretty.apply p fmt ("lambda",  [a])
      | _ ->
	  invalid_arg "Ill-formed application in theory of functions"

  let is = function Fun _, _ -> true | _ -> false
  let is_abs = function Fun(Abs), _ -> true | _ -> false
  let is_apply = function Fun(Apply _), _ -> true | _ -> false

end

(** Function symbols are hash-consed, therefore equality coincides
  with identity. *)
let eq = (==)

(** Use hashed hash value for comparison. *)
let cmp f g =
  if f == g then 0 else if hash f < hash g then -1 else 1

(** Combined pretty-printer. *)
let pp p fmt ((sym, _),  al) = 
  match sym with
    | Uninterp(op) -> Uninterp.pp p fmt (op, al)
    | Arith(op) -> Arith.pp p fmt (op, al)
    | Product(op) -> Product.pp p fmt (op, al)
    | Bv(op) -> Bv.pp p fmt (op, al)
    | Coproduct(op) -> Coproduct.pp p fmt (op, al)
    | Arrays(op) -> Array.pp p fmt (op, al)
    | Pp(op) -> Pprod.pp p fmt (op, al)
    | Fun(op) -> Fun.pp p fmt (op, al)

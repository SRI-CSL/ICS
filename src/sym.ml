(*
 * The contents of this file are subject to the ICS(TM) Community Research
 * License Version 1.0 (the ``License''); you may not use this file except in
 * compliance with the License. You may obtain a copy of the License at
 * http://www.icansolve.com/license.html.  Software distributed under the
 * License is distributed on an ``AS IS'' basis, WITHOUT WARRANTY OF ANY
 * KIND, either express or implied. See the License for the specific language
 * governing rights and limitations under the License.  The Licensed Software
 * is Copyright (c) SRI International 2003, 2004.  All rights reserved.
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
  | Cl of cl
  | Arrays of arrays 
  | Propset of propset

and uninterp = Name.t

and arith = 
  | Num of Mpa.Q.t  
  | Add
  | Multq of Mpa.Q.t

and product = Cons | Car | Cdr

and coproduct = 
  | In of direction 
  | Out of direction

and direction = Left | Right

and bv =
  | Const of Bitv.t
  | Conc of int * int
  | Sub of int * int * int

and pprod = Mult

and cl = 
  | Apply
  | S
  | K 
  | I
  | C
  | Reify of t * int

and arrays = Create | Select | Update

and propset = Empty | Full | Ite

let hash (_, hsh) = hsh

let get (sym, _) = sym

let theory_of (sym, _) =
  match sym with
    | Uninterp _ -> Th.u
    | Arith _ -> Th.la
    | Product _ -> Th.p
    | Bv _ -> Th.bv
    | Coproduct _ -> Th.cop
    | Arrays _ -> Th.arr
    | Pp _ -> Th.nl
    | Cl _ -> Th.app
    | Propset _ -> Th.set

let genidx =
  let idx = ref 100 in  (* indices below [100] for nonparametric symbols. *)
  let _ = Tools.add_at_reset (fun () -> idx := 100) in
    fun () -> incr(idx); !idx


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
	      let hsh = genidx() in
	      let f = (Uninterp(n), hsh) in
		Name.Hash.add table n f; f

  let equal = Name.eq

  let is (sym, _) = 
    match sym with
      | Uninterp _ -> true 
      | _ -> false

  let pp p fmt (f, al) = 
    Pretty.apply p fmt (Name.to_string f, al)

  let to_string f =
    Name.to_string f

end

module Arith = struct

  let get = function 
    | Arith(op), _ -> op
    | _ -> raise Not_found

  let equal f g =
    match f, g with
      | Num(q), Num(p) -> Mpa.Q.equal q p
      | Multq(q), Multq(p) -> Mpa.Q.equal q p
      | Add, Add -> true
      | _ -> false

  let mk_num = 
    let table = Mpa.Q.Hash.create 17 in
    let _ =  Tools.add_at_reset (fun () -> Mpa.Q.Hash.clear table) in
      fun q ->
	try
	  Mpa.Q.Hash.find table q 
	with
	    Not_found ->
	      let c = (Arith(Num(q)), genidx()) in
		Mpa.Q.Hash.add table q c; c
		  
  let mk_multq =
    let table = Mpa.Q.Hash.create 17 in
    let _ =  Tools.add_at_reset (fun () -> Mpa.Q.Hash.clear table) in
      fun q ->
	try
	  Mpa.Q.Hash.find table q 
	with
	    Not_found ->
	      let hsh = genidx() in
	      let c = (Arith(Multq(q)), hsh) in
		Mpa.Q.Hash.add table q c; c

  let mk_add = (Arith(Add), 0)

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

  let to_string = function
    | Num q ->  Pretty.to_string Mpa.Q.pp q
    | Add -> "+"
    | Multq(q) -> Format.sprintf "%s*" (Pretty.to_string Mpa.Q.pp q)

end

module Product = struct

  let get = function 
    | Product(op), _ -> op
    | _ -> raise Not_found

  let equal f g =
    match f, g with
      | Cons, Cons -> true
      | Car, Car -> true
      | Cdr, Cdr -> true
      | _ -> false

  let mk_cons = (Product(Cons), 1)
  let mk_car = (Product(Car), 2)
  let mk_cdr = (Product(Cdr), 3)

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

  let to_string = function
    | Cons -> "cons"
    | Car -> "car"
    | Cdr -> "cdr"
end

module Coproduct = struct

  let get = function 
    | Coproduct(op), _ -> op
    | _ -> raise Not_found

  let equal f g =
    match f, g with
      | In(x), In(y) -> x = y
      | Out(x), Out(y) -> x = y
      | _ -> true

  let mk_inl = (Coproduct(In(Left)), 4)
  let mk_inr = (Coproduct(In(Right)), 5)
  let mk_outl = (Coproduct(Out(Left)), 6)
  let mk_outr = (Coproduct(Out(Right)), 7)

  let is = function Coproduct _, _ -> true | _ -> false
  let is_inl = function Coproduct(In(Left)), _ -> true | _ -> false
  let is_inr = function Coproduct(In(Right)), _ -> true | _ -> false
  let is_outl = function Coproduct(Out(Left)), _ -> true | _ -> false
  let is_outr = function Coproduct(Out(Right)), _ -> true | _ -> false


  let to_string = function
    | In(Left) -> "inl"
    | In(Right) -> "inr"
    | Out(Left) -> "outl"
    | Out(Right) -> "outr"

  let pp p fmt (f, al) = 
    Pretty.apply p fmt (to_string f, al)

end

module Pprod = struct

  let get = function
    | Pp(op), _ -> op
    | _ -> raise Not_found

  let equal f g =
    match f, g with
      | Mult, Mult -> true
  
  let mk_mult = (Pp(Mult), 8)

  let is = function Pp _, _ -> true | _ -> false
  let is_mult = function Pp(Mult), _ -> true | _ -> false

  let pp p fmt = 
    function
      | Mult, [] ->
	  Pretty.string fmt "1"
      | Mult, al ->
	  Pretty.infixl p "*" fmt al

  let to_string = function
    | Mult -> "*"

end 

module Bv = struct

  let get = function 
    | Bv(op), _ -> op
    | _ -> raise Not_found

  let equal f g =
    match f, g with
      | Const(b1), Const(b2) -> Bitv.equal b1 b2
      | Conc(n1, m1), Conc(n2, m2) -> n1 = n2 && m1 = m2
      | Sub(n1, i1, j1), Sub(n2, i2, j2) -> n1 = n2 && i1 = i2 && j1 = j2
      | _ -> false

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
	      let hsh = genidx() in
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
	      let hsh = genidx() in
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
	     | Pretty.Mode.Mixfix ->  
		 Pretty.infix p "++" p fmt (a, b)
	     | _ ->
		 let op = Format.sprintf "conc[%d,%d]" n m in
		   Pretty.apply p fmt (op, [a; b]))
      | Sub(n, i, j), [a] -> 
	  (match !Pretty.flag with
	     | Pretty.Mode.Mixfix ->  
		 let op = Format.sprintf "[%d:%d]" i j in
		   p fmt a; Pretty.string fmt op
	     | _ ->
	  	 let op = Format.sprintf "sub[%d,%d,%d]" n i j in
		   Pretty.apply p fmt (op, [a]))
      | _ ->
	  invalid_arg "Ill-formed application in bitvector theory"

  let to_string = function 
    | Const(b) ->  Format.sprintf "0b%s" (Bitv.to_string b)
    | Conc(n, m) -> Format.sprintf "conc[%d,%d]" n m 
    | Sub(n, i, j) ->  Format.sprintf "sub[%d,%d,%d]" n i j

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

  let equal f g =
    match f, g with
      | Create, Create -> true
      | Update, Update -> true
      | Select, Select -> true
      | _ -> false

  let mk_create = (Arrays(Create), 9)
  let mk_update = (Arrays(Update), 10)
  let mk_select = (Arrays(Select), 11)

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
	       | Pretty.Mode.Mixfix -> 
		   arg a; str "["; arg j; str "]"
	       | _ -> 
		   Pretty.apply p fmt ("select", [a; j]))
	| Update, [a; i; x] -> 
	    (match !Pretty.flag with
	       | Pretty.Mode.Mixfix -> 
		   arg a; str "["; arg i; str " := "; arg x; str "]"
	       | _ -> 
		   Pretty.apply p fmt ("update", [a; i; x]))
	| _ ->
	    invalid_arg "Ill-formed application in theory of arrays"

  let to_string = function
    | Create -> "create"
    | Select -> "select"
    | Update -> "update"
	    
end

type tsym = t   (* nickname *)


module Propset = struct

  let get = function 
    | Propset(op), _ -> op
    | _ -> raise Not_found

  let equal f g =
    match f, g with
      | Empty, Empty -> true
      | Full, Full -> true
      | Ite, Ite -> true
      | _ -> false

  let mk_empty = (Propset(Empty), 16)
  let mk_full = (Propset(Full), 17)
  let mk_ite = (Propset(Ite), 18)

  let is = function Propset _, _ -> true | _ -> false
  let is_empty = function Propset(Empty), _ -> true | _ -> false
  let is_full = function Propset(Full), _ -> true | _ -> false
  let is_ite = function Propset(Ite), _ -> true | _ ->false

  let to_string = function
    | Empty -> "empty"
    | Full -> "full"
    | Ite -> "ite"

  let pp p fmt (op, al) =
    let arg = p fmt and str = Pretty.string fmt in
      match (op, al) with
	| Empty, [] ->
	    Pretty.string fmt "empty"
	| Full, [] ->
	    Pretty.string fmt "full"
	| Ite, [a; b; c] -> 
	    Pretty.apply p fmt ("ite", [a; b; c])
	| _ ->
	    invalid_arg "Ill-formed application in theory of propositional sets"    
end


module Cl = struct

  let get = function 
    | Cl(op), _ -> op
    | _ -> raise Not_found

  let is = function Cl _, _ -> true | _ -> false
    
  let equal f g =
    match f, g with
      | Apply, Apply -> true
      | S, S -> true
      | K, K -> true
      | I, I -> true
      | C, C -> true
      | Reify(f, n), Reify(g, m) -> (f == g) && n = m  (* symbols are hash-consed. *)
      | _ -> false
	  
  let s = (Cl(S), 111)
  let k = (Cl(K), 110)
  let i = (Cl(I), 109)
  let c = (Cl(C), 108)

  let reify =
    let module Hash = Hashtbl.Make(
      struct
	type t = tsym * int
	let equal (f, n) (g, m) =  f == g && n = m
	let hash (f, _) = hash f
      end) 
    in
    let ht = Hash.create 17 in
    let _ = Tools.add_at_reset (fun () -> Hash.clear ht) in
      fun (f, n) ->
	assert(not(is f));
	try
	  Hash.find ht (f, n)
	with
	    Not_found ->
	      let op = (Cl(Reify(f, n)), genidx()) in
		Hash.add ht (f, n) op; op

  let apply = (Cl(Apply), 112)

  let rec pp p fmt = 
    function
      | Apply, al -> 
	  let op = Format.sprintf "apply" in 
	    Pretty.string fmt "(";
	    Pretty.infixl p " $ " fmt al;
	    Pretty.string fmt ")"
      | S, [] -> 
	  Pretty.string fmt "S"
      | K, [] ->
	  Pretty.string fmt "K"
      | I, [] ->
	  Pretty.string fmt "I"
      | C, [] ->
	  Pretty.string fmt "C"
      | Reify(f, _), [] ->
	  let str = Format.sprintf "'%s'" (to_string f) in
	    Pretty.string fmt str
      | _ ->
	  invalid_arg "Ill-formed application in theory of functions"

  and to_string (sym, _) = 
    match sym with
      | Uninterp(op) -> Uninterp.to_string op
      | Arith(op) -> Arith.to_string op
      | Product(op) -> Product.to_string op
      | Bv(op) -> Bv.to_string op
      | Coproduct(op) -> Coproduct.to_string op
      | Arrays(op) -> Array.to_string op
      | Pp(op) -> Pprod.to_string op
      | Propset(op) -> Propset.to_string op
      | Cl(op) -> invalid_arg "Reified combinatory logic symbol"
      

  let is_s = function Cl(S), _ -> true | _ -> false
  let is_k = function Cl(K), _ -> true | _ -> false
  let is_i = function Cl(I), _ -> true | _ -> false
  let is_c = function Cl(C), _ -> true | _ -> false
  let is_reify = function Cl(Reify _), _ -> true | _ -> false
  let is_apply = function Cl(Apply), _ -> true | _ -> false

  let d_reify = function Cl(Reify(f, n)), _ -> (f, n) | _ -> raise Not_found

end


let equal (f, _) (g, _) =
 match f, g with
   | Uninterp(op1), Uninterp(op2) -> Uninterp.equal op1 op2
   | Arith(op1), Arith(op2) -> Arith.equal op1 op2
   | Product(op1), Product(op2) -> Product.equal op1 op2
   | Bv(op1), Bv(op2) -> Bv.equal op1 op2
   | Coproduct(op1), Coproduct(op2) -> Coproduct.equal op1 op2
   | Arrays(op1), Arrays(op2) -> Array.equal op1 op2
   | Pp(op1), Pp(op2) -> Pprod.equal op1 op2
   | Cl(op1), Cl(op2) -> Cl.equal op1 op2
   | Propset(op1), Propset(op2) -> Propset.equal op1 op2
   | _ -> false
  

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
    | Cl(op) -> Cl.pp p fmt (op, al)
    | Propset(op) -> Propset.pp p fmt (op, al)


(** Function symbols are hash-consed, therefore equality coincides
  with identity. *)
let eq f g =
  assert(if f == g then equal f g else not(equal f g));
  f == g

(** Use hashed hash value for comparison. *)
let cmp f g =
  if f == g then 0 else 
    begin
      assert(not(equal f g) && hash f <> hash g);
      if hash f < hash g then -1 else 1
    end 

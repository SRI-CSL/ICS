(*
 * The contents of this file are subject to the ICS(TM) Community Research
 * License Version 2.0 (the ``License''); you may not use this file except in
 * compliance with the License. You may obtain a copy of the License at
 * http://www.icansolve.com/license.html.  Software distributed under the
 * License is distributed on an ``AS IS'' basis, WITHOUT WARRANTY OF ANY
 * KIND, either express or implied. See the License for the specific language
 * governing rights and limitations under the License.  The Licensed Software
 * is Copyright (c) SRI International 2003, 2004.  All rights reserved.
 * ``ICS'' is a trademark of SRI International, a California nonprofit public
 * benefit corporation.
 *)

module type VAR = sig
  type t
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val hash : t -> int
  val pp : Format.formatter -> t -> unit
end

module type PREDSYM = sig
  type t
  val equal : t -> t -> bool  
  val compare : t -> t -> int 
  val hash : t -> int
  val pp : Format.formatter -> t -> unit
  val sub : t -> t -> bool
  val disjoint : t -> t -> bool
end

module type PARTITION = sig
  type var
  val find : var -> var
  val canonical : var -> bool
  val equal : var -> var -> bool
  val diseq : var -> var -> bool
  val union : var -> var -> unit
  val separate : var -> var -> unit
end

module type INFSYS = sig
  type predsym
  type var
  type t
  val empty : t
  val initialize : t -> unit
  val reset : unit -> unit
  val unchanged : unit -> bool
  val current : unit -> t
  module Set : (Sets.S with type elt = predsym)
  module Pos : (Maps.S with type key = var and type value = Set.t)
  module Neg : (Maps.S with type key = var and type value = Set.t)
  val pos : unit -> Pos.t
  val neg : unit -> Neg.t 
  val synchronized : unit -> bool
  val valid : predsym -> var -> bool
  val unsat : predsym -> var -> bool 
  val diseq : var -> var -> bool
  exception Unsat
  val processPos : predsym -> var -> unit
  val processNeg : predsym -> var -> unit
  val propagateEq : var -> var -> unit
end
 
module Make
  (V: VAR) 
  (P: PREDSYM)
  (Partition: PARTITION with type var = V.t) = 
struct
  type var = V.t
  type predsym = P.t

  module Set = Sets.Make(P)
  module Pos = Maps.Make(V)(Set)   (* Bindings [x |-> {p{1},...,p{n}}]. *)
  module Neg = Maps.Make(V)(Set)


  type t = {
    mutable pos : Pos.t;
    mutable neg : Neg.t;
  }

  let empty = { 
    pos = Pos.empty();
    neg = Neg.empty()
  }
  
  let init = ref empty
   
  module Config = struct
    module Pos = Config.Map(Pos)
    module Neg = Config.Map(Neg)
  end

  let pos = Config.Pos.current
  let neg = Config.Neg.current

  let initialize s =
    init := s;
    Config.Pos.initialize s.pos;
    Config.Neg.initialize s.neg

  let rec synchronized() = 
    synchronizedPos() &&
    synchronizedNeg()

  and synchronizedPos() = 
    Pos.for_all (fun x _ -> Partition.canonical x) (pos())

  and synchronizedNeg() = 
    Neg.for_all (fun x _ -> Partition.canonical x) (neg())

  let reset () = 
    initialize empty
      
  let unchanged () = 
    Config.Pos.unchanged() &&
    Config.Neg.unchanged()
      
  let current () = 
    if unchanged () then !init else {
      pos = Config.Pos.current();
      neg = Config.Neg.current()
    }

  let lookupPos = 
    let empty = Set.empty() in
      fun x -> 
	let x' = Partition.find x in
	  try Config.Pos.find x' with Not_found -> 
	    assert(Set.is_empty empty);
	    empty

  let lookupNeg = 
    let empty = Set.empty() in
      fun x -> 
	let x' = Partition.find x in
	  try Config.Neg.find x' with Not_found -> 
	    assert(Set.is_empty empty);
	    empty

  (** [p(x)] if there is some [q(x)] with [q(x) => p(x)]. *)
  let valid p x = 
    assert(synchronized());
    let super q = P.sub q p in
      Set.exists super (lookupPos x)

  (** [~p(x)] if there is some
    - [~q(x)] with [p(x) => q(x)], or
    - [q(x)] with [~(p(x) & q(x))]. *)
  let unsat p x = 
    assert(synchronized());
    let sup q = P.sub p q in
      Set.exists sup (lookupNeg x) ||
      Set.exists (P.disjoint p) (lookupPos x)

  let diseq x y = 
    let px = lookupPos x and qy = lookupPos y in
      Set.exists (fun p -> Set.exists (fun q -> P.disjoint p q) px) qy

  exception Unsat

  let processPos p x = 
    assert(synchronized());
    let x = Partition.find x in
      if valid p x then () else
	if unsat p x then raise Unsat else
	  let ps' = Set.copy (lookupPos x) in
	    Set.add p ps';
	    Config.Pos.set x ps'
	  
  let processNeg p x =
    assert(synchronized());
    let x = Partition.find x in
      if unsat p x then () else
	if valid p x then raise Unsat else
	  let ps = Set.copy (lookupNeg x) in
	    Set.add p ps;
	    Config.Neg.set x ps
	    
  let rec propagateEq x y =
    assert(Partition.equal x y);
    assert(not(Partition.canonical x));
    assert(Partition.canonical y);
    propagateEqPos x y;
    propagateEqNeg x y

  and propagateEqPos x y = 
    try
      let ps = Config.Pos.find x in
	Config.Pos.remove x;
	(try
	   let qs = Config.Pos.find y in
	     Config.Pos.set y (mergePos qs ps)
	 with
	     Not_found -> Config.Pos.set y ps)
    with
	Not_found -> ()

  and mergePos qs ps = 
    let rs = Set.copy ps in
    let add q = 
      if Set.exists (P.disjoint q) ps then raise Unsat else
	if Set.exists (fun p -> P.sub q p) ps then () else
	  Set.add q rs
    in
      Set.iter add qs;
      rs

  and propagateEqNeg x y = 
    try
      let ps = Config.Neg.find x in
	Config.Neg.remove x;
	(try
	   let qs = Config.Neg.find y in
	     Config.Neg.set y (mergeNeg qs ps)
	 with
	     Not_found -> Config.Neg.set y ps)
    with
	Not_found -> ()

  and mergeNeg qs ps = 
    let rs = Set.copy ps in
    let add q = 
      if Set.exists (P.disjoint q) ps then raise Unsat else
	if Set.exists (P.sub q) ps then () else
	  Set.add q rs
    in
      Set.iter add qs;
      rs
end

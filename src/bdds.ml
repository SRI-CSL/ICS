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

module type T = sig
  type t 
  val eq : t -> t -> bool
  val compare : t -> t -> int
  val mk_zero : t
  val mk_one : t
  module Args : sig
    type triple
    val eq : triple -> triple -> bool
    val make : t -> t -> t -> triple
    val hash : triple -> int
    val get : triple -> int -> t
    val set : triple -> int -> t -> unit
  end 
  val mk_ite : Args.triple -> t
  val is_ite : t -> bool
  val d_ite : t -> Args.triple
  val mk_fresh : unit -> t
  module Subst : sig
    type map
    val empty : unit -> map
    val compose : map -> t -> t -> unit
  end
end

module Make(T: T) = struct

  let mk_one = T.mk_one
  let mk_zero = T.mk_zero
		  
  let is_zero = T.eq T.mk_zero
  let is_one = T.eq T.mk_one
  let is_const b = is_zero b || is_one b
  let is_ite = T.is_ite
  let is_var b  = not(is_const b)

  let arg1 a =  T.Args.get a 0
  let arg2 a =  T.Args.get a 1
  let arg3 a =  T.Args.get a 2

  let d_cond b = 
    assert(is_ite b);
    arg1 (T.d_ite b)

 let d_pos b = 
   assert(is_ite b);
   arg2 (T.d_ite b)

 let d_neg b = 
   assert(is_ite b);
   arg3 (T.d_ite b)
    
 let rec is_bdd b = 
   is_zero b || 
   is_one b || 
   (is_ite b && 
    is_var (d_cond b) && is_bdd (d_pos b) && is_bdd (d_neg b))
     
 let lift b =
   if is_bdd b then b else 
     T.mk_ite (T.Args.make b T.mk_one T.mk_zero)
       
 let drop b =
   assert(is_bdd b);
   if is_ite b && is_one (d_pos b) && is_zero (d_neg b) then 
     d_cond b
   else 
     b
	
 (** For hashconsing conditionals. *)
 type memo = {
   mutable args: T.Args.triple; (* arguments *)
   bdd: T.t                     (* and corresponding conditional. *)
 }
    
 (** Hashconsing uses weak pointers for avoiding memory leaks. *)
 module Memo = Weak.Make(
   struct
     type t = memo
     let equal m n = T.Args.eq m.args n.args
     let hash m = T.Args.hash m.args
   end)
   
 let table = Memo.create 107
	       
 let _ = 
   Tools.add_at_exit 
     (fun () -> 
	let (length, entries, _, smallest, median, biggest) = Memo.stats table in
	  if entries > 0 then 
	    Format.eprintf "\nBDD cache: len = %d; # = %d; min = %d; median = %d; max = %d"
	      length entries smallest median biggest)

 let dummy = { args = T.Args.make mk_zero mk_zero mk_zero; bdd = mk_zero }
     
 let rec build a =
   dummy.args <- a;  
   try (Memo.find table dummy).bdd with Not_found ->
     let b = build1 a in
       assert(is_bdd b);
       Memo.add table {args = a; bdd = b}; 
       b 

  and build1 a =
   let b1 = arg1 a and b2 = arg2 a and b3 = arg3 a in
     assert(is_bdd b1 && is_bdd b2 && is_bdd b3);
     if T.eq b2 b3 then b2 else 
       if is_one b2 && is_zero b3 then b1 else 
	 if is_one b1 then b2 else 
	   if is_zero b1 then b3 else 
	     let x = topvar (d_cond b1) b2 b3 in
	     let pos = build (T.Args.make (cofactor_pos x b1) (cofactor_pos x b2) (cofactor_pos x b3))
	     and neg = build (T.Args.make (cofactor_neg x b1) (cofactor_neg x b2) (cofactor_neg x b3)) in
	       assert(is_var x);
	       if T.eq pos neg then pos else 
		 T.mk_ite (T.Args.make x pos neg)
      
  and cofactor_pos x b = 
    assert(is_var x && is_bdd b);
    if is_ite b && T.eq x (d_cond b) then d_pos b else b
      
  and cofactor_neg x b = 
    assert(is_var x && is_bdd b);
    if is_ite b && T.eq x (d_cond b) then d_neg b else b
	  
  and max x y = 
    assert(is_var x && is_var y);
    if T.compare x y <= 0 then x else y 
      
  and topvar x s2 s3 =
    assert(is_var x);
    try 
      let y = d_cond s2 in
	(try
	   let z = d_cond s3 in
	     max x (max y z)
	 with
	     Not_found -> max x y)
    with
	Not_found -> 
	  (try
	     let z = d_cond s3 in
	       max x z
	   with
	       Not_found -> x)
	  
  let mk_ite b1 b2 b3 = build (T.Args.make b1 b2 b3)
  let mk_conj b1 b2 = mk_ite b1 b2 T.mk_zero
  let mk_disj b1 b2 = mk_ite b1 T.mk_one b2
  let mk_neg b = mk_ite b T.mk_zero T.mk_one
  let mk_imp b1 b2 = mk_ite b1 b2 T.mk_one
  let mk_equiv b1 b2 = mk_ite b1 b2 (mk_neg b2)
  let mk_xor b1 b2 =  mk_ite b1 (mk_neg b2) b2

  let is_diseq b1 b2 = 
    assert(is_bdd b1 && is_bdd b2);
    is_one (mk_xor b1 b2)

  exception Inconsistent

  (** Solving equations over bdds based on the equality of [ite(x, p, n)] and 
    [(p union n) & exists delta. x = (p inter (n implies delta))] *)
  let solve =
    let el = Stacks.create () in
      fun b -> 
	assert(is_bdd b);
	Stacks.clear el;
	Stacks.push b el;
	let fresh = ref [] in
	let sl = T.Subst.empty () in
	  while not(Stacks.is_empty el) do
	    let a = Stacks.pop el in
	      if is_zero a then raise Exc.Inconsistent else
		if is_one a then () else
		  let x = d_cond a and pos = d_pos a and neg = d_neg a in
		  let k = T.mk_fresh () in   
		    fresh := k :: !fresh;
		    T.Subst.compose sl x (mk_conj pos (mk_imp neg k));
		    Stacks.push (mk_disj pos neg) el
	  done;
	  (!fresh, sl)
		     
end

(*
 * The contents of this file are subject to the ICS(TM) Community Research
 * License Version 2.1 (the ``License''); you may not use this file except in
 * compliance with the License. You may obtain a copy of the License at
 * http://www.icansolve.com/license.html.  Software distributed under the
 * License is distributed on an ``AS IS'' basis, WITHOUT WARRANTY OF ANY
 * KIND, either express or implied. See the License for the specific language
 * governing rights and limitations under the License.  The Licensed Software
 * is Copyright (c) SRI International 2003, 2004.  All rights reserved.
 * ``ICS'' is a trademark of SRI International, a California nonprofit public
 * benefit corporation.
 *)

(** Weak hash tables

  @author Harald Ruess

  Hash tables for associating keys [k] with a values [v]. The association
  is {i weak} in that [k] is still garbage collectable.  In this case,
  the association [k |-> v] is deleted.
*)

module type HASH =
  sig
    type t
    val equal: t -> t -> bool
    val hash: t -> int
  end

module type S =
  sig
    type key
    type 'a t
    val create: int -> 'a t
    val count : 'a t -> int
    val add: 'a t -> key -> 'a -> unit
    val find: 'a t -> key -> 'a
    val mem : 'a t -> key -> bool
    val iter: (key -> 'a -> unit) -> 'a t -> unit
    val fold: (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
    val to_list : 'a t -> (key * 'a) option list
    type stats = { length : int; count : int; del: int }
    val stats : 'a t -> stats
  end

module Make(H: HASH): (S with type key = H.t) = struct

  type key = H.t

  module D = Set.Make(
    struct
      type t = int
      let compare i j = if i == j then 0 else if i < j then -1 else 1
    end)

  let bot () = Obj.magic 0
      
  type 'a t = {
    mutable keys : key Weak.t;
    mutable vals : 'a array;
    mutable entries : int;
    mutable deleted : D.t
  }

  let create m =
    let m = max 7 (min m Sys.max_array_length) in
      { keys = Weak.create m;
	vals = Array.create m (bot());
	entries = 0;
	deleted = D.empty }

  let length t = 
    assert(Array.length t.vals = Weak.length t.keys);
    Array.length t.vals

 let count t =
    let m = length t in
    let n = ref 0 in
      for j = 0 to m - 1 do
	if Weak.check t.keys j then incr n;
      done;
      !n

  let is_deleted t j = D.mem j t.deleted

  let protect = ref false

  (** [with_protected f a] disables garbage collection of values 
    while executing [f a].  As a consuence,  [Array.get t.vals j]
    always a meaningful value, whenever [Weak.check t.keys j] holds. *)
  let with_protected f a =
    try
      protect := true;
      let b = f a in
	protect := false;
	b
    with
	exc -> protect := false; raise exc
     
  let to_list t =  
    let m = length t in
    let l = ref [] in
    let body j = 
      let e =         
	match Weak.get t.keys j with 
	  | Some(k) ->    (* atomic. *)  
	      assert(Weak.check t.keys j);
	      Some(k, Array.get t.vals j)
	  | None -> 
	      None
      in
	l := e :: !l
    in
      for j = 0 to m - 1 do with_protected body j done;
      !l

  (** Linear probing. *)
  let hash m k i = 
    assert(i <= m);
    let h' = H.hash k mod (max_int - m) in
      assert(0 <= h' + i);
      (h' + i) mod m 

  let mem t k = 
    let m = length t in
    let h = hash m k in
    let rec repeat i = 
      i < m &&
      let j = h i in
	assert(j < m);
	match Weak.get t.keys j with
	  | Some(k') -> if H.equal k k' then true else repeat (i + 1)
	  | None -> if is_deleted t j then repeat (i + 1) else false
    in
      repeat 0

  let find t k = 
    let m = length t in
    let h = hash m k in
    let rec repeat i = 
      if i = m then raise Not_found else
	let j = h i in
	  assert(j < m);
	  match Weak.get t.keys j with
	    | Some(k') ->     (* atomic. *)
		if H.equal k k' then 
		  (assert(Weak.check t.keys j);
		   let v = Array.get t.vals j in
		     v)
		else
		  repeat (i + 1)
	    | None ->
		if is_deleted t j then 
		  repeat (i + 1) 
		else
		  raise Not_found
    in
      with_protected repeat 0

  exception Overflow
      
  let add_aux t k u =        
    let m = length t in
    let h = hash m k in
    let rec repeat i = 
      if i >= m then raise Overflow else
	let j = h i in
	  assert(j < length t);
	  match Weak.get t.keys j with
	    | None ->             
		let remove _ = 
		  t.deleted <- D.add j t.deleted; 
		  t.entries <- t.entries + 1;
		  if not (!protect) then        (* do not free value while *)
		    Array.set t.vals j (bot())  (* executing protected code. *)
		in
		  Weak.set t.keys j (Some(k));  (* shadow values for [k] should be removed. *)
		  Array.set t.vals j u; 
		  if is_deleted t j then 
		    t.deleted <- D.remove j t.deleted;
		  t.entries <- t.entries + 1;
		  Gc.finalise remove k
	    | Some _ -> 
		repeat (i + 1)
    in                            (* resize when load factor [>= 0.7]. *)
      if float_of_int t.entries /. float_of_int m >= 0.7 then
	raise Overflow
      else 
	repeat 0

  let resize t = 
    let m = length t in
    let keys = t.keys and vals = t.vals in
    let m' = min (3*m/2 + 3) (Sys.max_array_length - 1) in
      if m' <= m then failwith "Weakhash.add : table cannot grow more" else
	begin
	  t.keys <- Weak.create m';
	  t.vals <- Array.create m' (bot());
	  t.deleted <- D.empty;
	  let body j  = 
	    match Weak.get keys j with 
	      | Some(k) -> (* atomic. *)
		  assert(Weak.check keys j);
		  let u = Array.get vals j in
		    add_aux t k u
	      | None -> ()
	  in
	  for j = 0 to m - 1 do with_protected body j done
	end
	  
  let add t k u =
    assert(not (mem t k));    (* avoid creating shadows. *)
    try add_aux t k u with Overflow -> 
      resize t; add_aux t k u
  
  let iter f t =
    let m = length t in
    let body j = 
      match Weak.get t.keys j with 
	| Some(k) ->         (* atomic. *)
	    assert(not(is_deleted t j));
	    let u = Array.get t.vals j in
	      f k u
	| None -> ()
    in
      for j = 0 to m - 1 do with_protected body j done
      
  let fold f t e =
    let acc = ref e in
    let f' k u = acc := f k u !acc in
      iter f' t;
      !acc

  type stats = { length : int; count : int; del: int }

  let stats t = {
    length = length t;
    count = count t;
    del = D.cardinal t.deleted
  }

end

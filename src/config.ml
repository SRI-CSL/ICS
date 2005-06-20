(*
 * The contents of this file are subject to the ICS(TM) Community Research
 * License Version 2.1 (the ``License''); you may not use this file except in
 * compliance with the License. You may obtain a copy of the License at
 * http://www.icansolve.com/license.html.  Software distributed under the
 * License is distributed on an ``AS IS'' basis, WITHOUT WARRANTY OF ANY
 * KIND, either express or implied. See the License for the specific language
 * governing rights and limitations under the License.  The Licensed Software
 * is Copyright (c) SRI International 2004.  All rights reserved.
 * ``ICS'' is a trademark of SRI International, a California nonprofit public
 * benefit corporation.
 *)

module Set(S: Sets.S) = struct
  let init = ref (S.empty())    (* initial configuration. *)
  let curr = ref (S.empty())    (* current configuration. *)
  let initialize s = init := s; curr := s
  let current () = !curr
  let unchanged () = (!init == !curr)
  let mem x = S.mem x !curr
  let sub s = S.subset s !curr
  let protect () = if !init == !curr then curr := S.copy !curr
  let add x = if not(mem x) then begin protect (); S.add x !curr end
  let remove x = if mem x then begin protect (); S.remove x !curr end
  let union s = if not(sub s) then begin protect (); S.union s !curr end 
end

module Map(M: Maps.S) = struct
  let empty = M.empty()
  let init = ref empty    (* initial configuration. *)
  let curr = ref empty    (* current configuration. *)
  let initialize s = init := s; curr := s
  let reset () = 
    assert(M.is_empty empty);
    initialize empty
  let current () = !curr
  let pp fmt = M.pp fmt !curr
  let unchanged () = (!init == !curr)
  let find x = M.find x !curr
  let mem x = M.mem x !curr
  let protect () = if !init == !curr then curr := M.copy !curr
  let set x v = protect (); M.set x v !curr
  let remove x = if mem x then begin protect (); M.remove x !curr end
  let replace x y = protect (); curr := M.replace !curr x y
end

module Subst(S: Subst.S) = struct
  type t = S.t
  let empty = S.empty()
  let init = ref empty
  let curr = ref empty
  let initialize s = init := s; curr := s
  let reset () = initialize empty
  let current () = !curr
  let pp fmt = S.pp fmt !curr
  let unchanged () = (!init == !curr)
  let lookup x = S.lookup !curr x
  let apply t = S.apply !curr t
  let mem x = S.dom x !curr
  let protect () = if !init == !curr then curr := S.copy !curr
  let add x t = protect(); S.add !curr x t
  let compose rho = protect(); S.compose !curr rho
 let update x t = protect(); S.update !curr x t
  let remove x = if mem x then (protect(); S.remove !curr x)
  let fold f e = S.fold f !curr e
  let iter f = S.iter f !curr
  let exists p = S.exists p !curr
  let for_all p = S.for_all p !curr
  let choose p = S.choose p !curr
end

module Powermap(M: Powermaps.S) = struct
  let empty = M.empty()
  let init = ref empty    (* initial configuration. *)
  let curr = ref empty    (* current configuration. *)
  let initialize s = init := s; curr := s
  let reset () = initialize empty
  let current () = !curr
  let unchanged () = (!init == !curr)
  let find x = M.find x !curr
  let mem x = M.mem x !curr
  let is_empty = mem
  let is_singleton x = M.Values.cardinal (find x) = 1

  let protect () =  if !init == !curr then curr := M.copy !curr

  let remove x = if mem x then begin protect (); M.remove x !curr end
    
  let update x ys = 
    if M.Values.is_empty ys then remove x else
       (protect (); M.set x ys !curr)

  let merge x y = 
    let xs = find x in
      if M.Values.is_empty xs then () else
	let ys = find y in
	  if M.Values.is_empty ys then
	    begin
	      protect();
	      M.remove x !curr;
	      M.set y xs !curr
	    end
	  else 
	    let ys' = M.Values.copy ys in
	      M.Values.union xs ys';
	      protect();
	      M.remove x !curr;
	      M.set y ys' !curr
	     
  let add x y =
    let dy = find y in
      if M.Values.is_empty dy then
	begin
	  protect(); 
	  M.set y (M.Values.singleton x) !curr
	end
      else if not(M.Values.mem x dy) then
	let dy' = M.Values.copy dy in
	  M.Values.add x dy';
	  protect(); 
	  M.set y dy' !curr

  let rem x y = 
   let dy = find y in
     if M.Values.is_empty dy then () else
       if M.Values.mem x dy then
	 if M.Values.cardinal dy = 1 then
	   (protect(); M.remove y !curr)
	 else 
	   let dy' = M.Values.copy dy in
	     M.Values.remove x dy';
	     protect(); M.set y dy' !curr
end

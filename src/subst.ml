
(*i
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
i*)

(*s Module [Subst]: Data structure for substitutions with domain [Term.t] 
 with [use] lists. *)


module type S = sig
  val iter : (Term.t -> unit) -> Term.t -> unit     
end


module type T = sig
  type t
  val eq : t -> t -> bool
  val subst_of : t -> Term.t Term.map
  val use_of : t -> Term.set Term.map
  val empty: t
  val copy : t -> t
  val apply: t -> Term.t -> Term.t  
  val find: t -> Term.t -> Term.t
  val inv : t -> Term.t -> Term.t 
  val use : t -> Term.t -> Term.set 
  val mem : t -> Term.t -> bool
  val update : Term.t -> Term.t -> t -> unit 
  val restrict : Term.t -> t -> unit
end
  
module Make(Th: S) = struct
  
  type t = {
    mutable find: Term.t Term.map;
    mutable inv : Term.t Term.map;
    mutable use : Term.set Term.map
  }

  let empty = {
    find = Ptmap.empty;
    inv = Ptmap.empty;
    use = Ptmap.empty
  }

  let copy s = {
    find = s.find;
    inv = s.inv;
    use = s.use
  }

  let eq s t = (s.find == t.find)

  let subst_of s = s.find
  let use_of s = s.use
  let inv_of s = s.inv

  let apply s a = Ptmap.find a s.find
  let find s a = try Ptmap.find a s.find with Not_found -> a
  let inv s a = Ptmap.find a s.inv
  let mem s a =  Ptmap.mem a s.find
  let invmem s a = Ptmap.mem a s.inv
  let use s a = try Ptmap.find a s.use with Not_found -> Ptset.empty


(*s Rewiring of [use] indices when changing the [find] structure. *)

  let add_use s a =
    let upd x =
      let usex = use s x in
      let usex' = Ptset.add a usex in
      if not(usex == usex') then
	s.use <- Ptmap.add x usex' s.use
    in
    Th.iter upd a

  let del_use s a =
    let upd x =
      let usex = use s x in
      let usex' = Ptset.remove a usex in
      if Ptset.is_empty usex' then
	s.use <- Ptmap.remove x s.use
      else if not(usex == usex') then
	s.use <- Ptmap.add x usex' s.use
    in
    Th.iter upd a


(*s [update a b s] sets the find of [a] to [b]. *)

  let update a b s =
    (try                                 (* if the find of [a] gets overwritten, then *)          
       del_use s (Ptmap.find a s.find )  (* remove it from use lists. *)
     with
	 Not_found -> ());
    s.find <- Ptmap.add a b s.find;   
    s.inv <- Ptmap.add b a s.inv;
    add_use s b


(*s Restrict. *)

 let restrict a s = 
    assert(mem s a);
    let b = Ptmap.find a s.find in
    s.find <- Ptmap.remove a s.find;
    s.inv <- Ptmap.remove b s.inv;
    del_use s b

end

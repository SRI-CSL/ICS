
(*i*)
open Term
open Hashcons
(*i*)

module type INTERP = sig
  val name : string 
  val is_th : Term.t -> bool
  val iter : (Term.t -> unit) -> Term.t -> unit     
end


module Make(Th: INTERP) = struct

  open Th
  
  type t = {
    mutable find: Subst.t;
    mutable inv : Subst.t;
    mutable use : Term.ts Term.Map.t
  }

  let empty () = {
    find = Subst.empty;
    inv = Subst.empty;
    use = Term.Map.empty
  }

  let copy s = {
    find = s.find;
    inv = s.inv;
    use = s.use
  }

  let subst_of s = s.find
  let use_of s = s.use
  let inv_of s = s.inv

  let apply s = 
    Subst.apply s.find

  let find s = 
    Subst.find s.find

  let inv s =
    Subst.apply s.inv

  let mem s = 
    Subst.mem s.find

  let invmem s =
    Subst.mem s.inv

  let use s a =
    try
      Term.Map.find a s.use
    with
	Not_found ->
	  Term.Set.empty

  let add_use s a =
    let upd x =
      let usex = use s x in
      let usex' = Term.Set.add a usex in
      if not(usex == usex') then
	s.use <- Term.Map.add x usex' s.use
    in
    iter upd a

  let del_use s a =
    let upd x =
      let usex = use s x in
      let usex' = Term.Set.remove a usex in
      if Term.Set.is_empty usex' then
	s.use <- Term.Map.remove x s.use
      else if not(usex == usex') then
	s.use <- Term.Map.add x usex' s.use
    in
    iter upd a

  (*s Union. *)

  let union s ((a,b) as e) =
    Trace.call 5 ("Union("^name^")") e Pretty.eqn;
    assert(not(is_th a));
    (try                                 (* if the find of [a] gets overwritten, then *)          
       del_use s (Subst.apply s.find a)  (* remove it from use lists. *)
     with
	 Not_found -> ());
    s.find <- Subst.add (a,b) s.find;   
    s.inv <- Subst.add (b,a) s.inv;
    add_use s b

  (*s Extending the domain. *)
  
  let extend s (a,b) = 
    Trace.call 5 ("Ext("^name^")") (a,b) Pretty.eqn;               
    assert(not(mem s a));
    s.find <- Subst.add (a,b) s.find;
    s.inv <- Subst.add (b,a) s.inv;
    add_use s b

  (*s Restricting the domain. *)

  let restrict s a =
    Trace.call 5 ("Res("^name^")") a Pretty.term;
    assert(mem s a);
    let b = apply s a in
    s.find <- Subst.remove a s.find;
    s.inv <- Subst.remove b s.find;
    del_use s b

end







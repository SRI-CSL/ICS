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


(** A {i configuration} consists of a triple [(g, e, p)] with
  - [g] the input facts,
  - [e] the equality sets for the individual theories
  - [v] the shared variable equalities and disequalities. *)
module Config = struct

  type t = { e: E.Config.t; v: V.Config.t }
		 
  let empty () = { e = E.Config.empty(); v = V.Config.empty() }

  let shared s = s.v
  let components s = s.e

  module Print = struct

    let all fmt s =
      Format.fprintf fmt "@[";
      if not(V.Config.is_empty s.v) then 
	V.Config.pp fmt s.v;
      if not(E.Config.is_empty s.e) then 
	E.Config.pp fmt s.e;
      Format.fprintf fmt "@]@."

    let component i fmt s = 
      E.Config.Component.pp i fmt s.e

    let shared fmt s = 
      V.Config.pp fmt s.v

  end

  let pp = Print.all

  let model s =
    let mdl = E.Config.model s.e in
      mdl

  let dep i s x = E.Config.Component.dep i s.e x

  (** Theory-specific lookups in solution sets. *)
  module Find = struct

    let lookup i s t = 
      try E.Config.Component.Apply.get i s.e t with Not_found -> t

    let justify i s t = 
      try E.Config.Component.Apply.justify i s.e t with Not_found -> 
	Judgement.mk_refl t

  end


  (** Theory-specific inverse lookups as a partial function. *)
  module Inv = struct

    let lookup s t =
      let i = Term.theory_of t in
	E.Config.Component.Inv.get i s.e t

    let justify s t = 
      let i = Term.theory_of t in
	E.Config.Component.Inv.justify i s.e t

  end

  let occ i s x =
    E.Config.Component.occ i x s.e

  (** Canonization of mixed terms. *)
  module Can = struct

    let term s =
      let rec canterm t =
	if Term.is_var t then 
	  V.Config.find s.v t 
	else
	  let f = Term.sym_of t and a = Term.args_of t in
	  let a' = canargs (Funsym.theory_of f) a in
	  let t' = if Term.Args.eq a a' then t else Term.sigma f a' in
	    try V.Config.find s.v (Inv.lookup s t') with Not_found -> t'
      and canargs i a = 
	let canarg t = Find.lookup i s (canterm t) in
	  Term.Args.map canarg a
      in
	canterm

    (** Apply [f] on all hypothesis of [t = Can.term s t]. *)
    let term' f s t =
      let rec canterm t =
	if Term.is_var t then canvar t else 
	  let f = Term.sym_of t and a = Term.args_of t in
	  let a' = canargs (Funsym.theory_of f) a in
	  let t' = if Term.Args.eq a a' then t else Term.sigma f a' in
	    try canvar (inv t') with Not_found -> t'
      and inv t = 
	let e = Inv.justify s t in
	  f e; e#lhs
      and canvar x =
	let e = V.Config.justify s.v x in
	  f e; e#rhs
      and canargs i a = 
	let canarg t = 
	  let e = Find.justify i s (canterm t) in
	    f e; e#rhs
	in
	  Term.Args.map canarg a
      in
	canterm t

    let iter f s t = 
      let _ = term' f s t in ()

    let fold f s t e =
      let acc = ref e in
      let f' e = (acc := f e !acc) in
	iter f' s t;
	!acc

    class can (s: t) (t: Term.t) = (object
      inherit Judgement.Top.equal
      method lhs = t
      method rhs = term s t
      method name = "can"
      method hyps = 
	fold Judgement.mk_add_equal s t Judgement.mk_empty
      method assumptions acc = 
	let add e = e#assumptions acc in
	  iter add s t
    end : Judgement.equal)

    let justify s t = new can s t

    let explain s t1 t2 =
      let e1 = justify s t1 and e2 = justify s t2 in
	assert(Term.eq e1#rhs e2#rhs);
	Judgement.mk_join e1 e2

  end

  (** Canonization of mixed terms. *)
  let can = Can.term
	  
  let is_canonical s t = Term.eq t (can s t)

  (** Equality test. *)
  module Equal = struct

    let test s t1 t2 = 
      Term.eq (Can.term s t1) (Can.term s t2)

    let justify s t1 t2 =
      let e1 = Can.justify s t1 and e2 = Can.justify s t2 in
	assert(Term.eq e1#rhs e2#rhs);
	Judgement.mk_join e1 e2

  end 

  (** Disequality test. *)
  module Diseq = struct

    let test s t1 t2 =
      assert(is_canonical s t1 && is_canonical s t2);
      if Term.is_var t1 && Term.is_var t2 then
	V.Config.is_diseq s.v t1 t2
      else
	false

    let justify s t1 t2 = 
      assert(is_canonical s t1);
      assert(is_canonical s t2);
      V.Config.Explain.diseq s.v t1 t2

  end 

  (** A term [t] is constrained by [c] if 
    - [t] is a variable with constrained [d] and [c <= d] or
    - [t] has theory-specific constraint [c] associated with it. *)
  module Cnstrnt = struct

    let test s t c = 
      assert(is_canonical s t);
      try 
	Cnstrnt.sub c (V.Config.cnstrnt s.v t) 
      with 
	  Not_found -> (Term.is_cnstrnt t c = Three.Yes)

    class cnstrnt (t: Term.t) (c: Cnstrnt.t) = (object
      inherit Judgement.Top.cnstrnt
      method arg = t
      method cnstrnt = c
      method name = "cnstrnt0"
      method hyps = Judgement.mk_empty
      method validate = (Term.is_cnstrnt t c = Three.Yes)
    end : Judgement.cnstrnt)
	    
    let justify s t c = 
      assert(test s t c);
      if Term.is_var t then V.Config.Explain.cnstrnt s.v t c else 
	new cnstrnt t c
	
  end 

  module Nonneg = struct

    let test s t = 
      assert(is_canonical s t);
      false

    let justify s t =
      assert(is_canonical s t);
      failwith "not yet applicable"    


  end 

  module Pos = struct
    
    let test s t = 
      assert(is_canonical s t);
      false

    let justify s t =
      assert(is_canonical s t);
      failwith "not yet applicable"

  end 

  module Valid = struct

    let test s = function
      | Atom.TT -> true
      | Atom.FF -> false
      | Atom.Equal(t1, t2) -> Term.eq t1 t2
      | Atom.Diseq(t1, t2) -> Diseq.test s t1 t2
      | Atom.Nonneg(t) -> Nonneg.test s t
      | Atom.Pos(t) -> Pos.test s t
      | Atom.Cnstrnt(t, c) -> Cnstrnt.test s t c

    let justify s a =
      assert(test s a);
      match a with
	| Atom.TT -> (Judgement.mk_triv:>Judgement.atom)
	| Atom.Equal(t1, _) -> ((Judgement.mk_refl t1):>Judgement.atom)
	| Atom.Diseq(t1, t2) -> ((Diseq.justify s t1 t2):>Judgement.atom)
	| Atom.Nonneg(t) -> ((Nonneg.justify s t):>Judgement.atom)
	| Atom.Pos(t) -> ((Pos.justify s t):>Judgement.atom)
	| Atom.Cnstrnt(t, c) -> ((Cnstrnt.justify s t c):>Judgement.atom)
	| Atom.FF -> invalid_arg "Trivially unsat"
  end 

end
    
module Infsys = struct
   
  let initialize s =  
    G.Infsys.initialize [];
    V.Infsys.initialize s.Config.v;
    E.Infsys.initialize s.Config.e
    
  let current () = { 
    Config.v = V.Infsys.current(); 
    Config.e = E.Infsys.current() 
  }

  let reset () =
    G.Infsys.reset ();
    V.Infsys.reset ();
    E.Infsys.reset ()
    
  let is_unchanged () = 
    V.Infsys.is_unchanged() && 
    E.Infsys.is_unchanged()

  (** Apply applicable inference rules until input [G] is empty. *)   
  let rec process a =
    match a#concl with
      | Atom.TT -> ()
      | Atom.FF -> raise(Judgement.Unsat(Judgement.to_unsat a))
      | Atom.Equal _  -> process_equal (Judgement.to_equal a)
      | Atom.Diseq _ -> process_diseq (Judgement.to_diseq a)
      | Atom.Nonneg _ -> process_nonneg (Judgement.to_nonneg a)
      | Atom.Pos _ -> process_pos (Judgement.to_pos a)
      | Atom.Cnstrnt _ -> process_cnstrnt (Judgement.to_cnstrnt a)

  and processl al =
    List.iter process al

  and process_equal e =
    match Term.status2 e#lhs e#rhs with
      | Term.Variable -> V.Infsys.process_equal e
      | Term.Pure(i) -> E.Infsys.process_equal i e
      | Term.Mixed(i, t) -> E.Infsys.abstract i t (e:>Judgement.atom)

  and process_diseq d = 
    match Term.status2 d#lhs d#rhs with
      | Term.Variable -> V.Infsys.process_diseq d
      | Term.Pure(i) -> E.Infsys.process_diseq i d
      | Term.Mixed(i, t) -> E.Infsys.abstract i t (d:>Judgement.atom)
   
  and process_nonneg n =
    match Term.status n#arg with
      | Term.Variable -> E.Infsys.process_nonneg Linarith.theory n
      | Term.Pure(i) -> E.Infsys.process_nonneg i n
      | Term.Mixed(i, t) -> E.Infsys.abstract i t (n:>Judgement.atom)

  and process_pos p =
    match Term.status p#arg with
      | Term.Variable -> E.Infsys.process_pos Linarith.theory p
      | Term.Pure(i) -> E.Infsys.process_pos i p
      | Term.Mixed(i, t) -> E.Infsys.abstract i t (p:>Judgement.atom)

  and process_cnstrnt c =
    match Term.status c#arg with
      | Term.Variable -> V.Infsys.process_cnstrnt c
      | Term.Pure(i) -> E.Infsys.abstract i c#arg (c:>Judgement.atom)
      | Term.Mixed(i, t) -> E.Infsys.abstract i t (c:>Judgement.atom)

 let rec finalize () = 
    propagate ();
    close();
    assert(G.Infsys.is_empty ());
    { Config.v = V.Infsys.finalize(); 
      Config.e = E.Infsys.finalize() 
    }
        
  and close () =
    try process (G.Infsys.get ()) with G.Empty -> 
      assert(G.Infsys.is_empty())

  and propagate () =
    try propagate1 (); propagate () with Not_found -> 
      assert(Propagate.is_empty ()); ()

  (** Propagate variable equalities, disequalities, constraints,
    and inequalities (in this order). *)
  and propagate1 () = 
   try E.Infsys.propagate_equal (Propagate.Equal.get ()) with Not_found -> 
 (*     (try E.Infsys.propagate_diseq (Propagate.Diseq.get ()) with Not_found -> *)
	 (try E.Infsys.propagate_cnstrnt (Propagate.Cnstrnt.get ()) with Not_found -> 
	    E.Infsys.propagate_nonneg (Propagate.Nonneg.get ()))

  and normalize () = 
    V.Infsys.normalize ();
    E.Infsys.normalize ()

  (** Call [f a] in configuration [s] by protecting global variables against 
    destructive updates. *)
  let call_with s f a =
    assert(Propagate.is_empty ());
    assert(G.Infsys.is_empty ());
    let save = finalize () in
      try
	initialize s;
	let b = f a in
	  initialize save;
	  b
      with
	  exc -> 
	    initialize save;
	    raise exc

  (** Search for satisfiable branch. *)
  let rec branch () =
    match E.Infsys.branch () with
      | None -> ()
      | Some(cl) -> failwith "branch: to do"

  let model () = failwith "model: to do"

end

  

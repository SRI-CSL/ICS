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

(** Flat terms are of the form [x*y] with [x], [y] variables. *)
module Flat = struct

  let is t = 
    Nonlin.is_mult t &&
    Term.is_var (Nonlin.lhs t) &&
    Term.is_var (Nonlin.rhs t)
    
  let lhs t =
    assert(is t);
    Nonlin.lhs t

  let rhs t =
    assert(is t);
    Nonlin.rhs t

  let can f a =
    assert(Term.Args.length a = 2);
    Nonlin.Pprod.mk_mult (Term.Args.get a 0) (Term.Args.get a 1)

  let map f t =
    assert(is t);
    let x = lhs t and y = rhs t in
    let x' = f x and y' = f y in
      if x == x' && y == y' then t else Nonlin.Pprod.mk_mult x' y'

  let dom e =
    assert(Term.is_var e#lhs);
    e#lhs

  let cod e =
    assert(is e#rhs);
    e#rhs

end


(** Inference system for AC terms with cancellation laws. *)
module Ac = Can.Make(
  struct

    let th = Nonlin.theory
    let can = Flat.can
    let map = Flat.map

    let is_diseq _ _ = false

    open Axioms  

    let mk_lapp = 
      let mult = (Nonlin.Op.inj Nonlin.Mult) in
	fun a b -> Lterm.mk_app mult [a; b]

    (** [x * (y * z) = (x * y) * z]. *)
    let assoc = 
      let x = Lterm.mk_var "x" in
      let y = Lterm.mk_var "y" in
      let z = Lterm.mk_var "z" in
	Chain.mk_equal 
	  (Name.of_string "assoc")
	  [] 
	  (mk_lapp x (mk_lapp y z)) 
	  (mk_lapp (mk_lapp x y) z)

    (** [x * y1 = x * y2 ==> y1 = y2]. *)
    let lcancel =
      let x = Lterm.mk_var "x" in
      let y1 = Lterm.mk_var "y1" in
      let y2 = Lterm.mk_var "y2" in
	Chain.mk_equal  
	  (Name.of_string "lcancel")
	  [Atom.mk_equal (mk_lapp x y1) (mk_lapp x y2)]
	  y1 y2

    (** [y1 * x = y2 * x ==> y1 = y2]. *)
    let rcancel =
      let x = Lterm.mk_var "x" in
      let y1 = Lterm.mk_var "y1" in
      let y2 = Lterm.mk_var "y2" in
	Chain.mk_equal 
          (Name.of_string "rcancel")
	  [Atom.mk_equal (mk_lapp y1 x) (mk_lapp y2 x)]
	  y1 y2
	
    let chains = 
      [assoc; lcancel; rcancel]
      
    let disjunction _ = raise Not_found
      
  end)

module J = Judgement


(** Various deductions from equalities [x = y*z]. *)
module Deduce = struct


  let linearize t = failwith "to do"

  let all e = ()

end 

(*
module Deduce = struct

  open Cnstrnt

  (** Deducing new constraints from an equality [x = y * z] using
    abstract constraint interpretation; for example:
    - [int = int * X] ==> [X = int]
    - [real = real * X] ==> [X = real]. *)
  let cnstrnts e =
    let x = dom e and a = cod e in
    let cnstrnt z =
      assert(Term.is_var z);
      match V.Infsys.cnstrnt z with
	| Some(c) -> 
	    if Cnstrnt.is_real c#cnstrnt then c else
	      raise(Jst.Inconsistent(failwith "to do"))
	| None ->
	    V.Infsys.process_cnstrnt (x, Real, rho);
	    Real, rho
    in  
    let y = Flat.lhs a  and z = Flat.rhs a in   (* [rho |- x = y*z] *)
    let c, gamma = cnstrnt x          (* [gamma |- x in c]. *)               
    and d, delta = cnstrnt y          (* [delta |- y in d]. *)
    and e, eps = cnstrnt z in         (* [eps |- z in e]. *)
      match c, d, e with
	| Int, Int, Nonint -> 
	    raise(Jst.Inconsistent(Jst.dep4 rho gamma delta eps))
	| Int, Nonint, Int -> 
	    raise(Jst.Inconsistent(Jst.dep4 rho gamma delta eps))
	| Nonint, Int, Int -> 
	    raise(Jst.Inconsistent(Jst.dep4 rho gamma delta eps))
	| Int, Int, Real -> 
	    V.Infsys.process_cnstrnt (z, Int, Jst.dep3 rho gamma delta)
	| Int, Real, Int -> 
	    V.Infsys.process_cnstrnt (y, Int, Jst.dep3 rho gamma eps)
	| Real, Int, Int -> 
	    V.Infsys.process_cnstrnt (x, Int, Jst.dep3 rho delta eps)
	| _  ->
	    ()
	    
  (** Deducing new nonnegativity constraints from an equality [x = y * z]
    - [nonneg = nonneg * Z] implies [Z = nonneg]
    -  [nonneg = Y * nonneg] implies [Y = nonneg]
    -  [X = nonneg * nonneg] implies [X = nonneg]. *)
  let nonneg x (a, rho) =
    assert(Term.is_var x && Flat.is a);     
    let is_nonneg z = 
      failwith "to do"
      (* try Some(Simplex.jst_of_nonneg_var z) with Not_found -> None  *)
    in  
    let y = Flat.lhs a and z = Flat.rhs a in       (* [rho |- x = y*z]. *)
      match is_nonneg x, is_nonneg y, is_nonneg z with
	| None, Some(tau2), Some(tau3) ->
	    Simplex.Infsys.process_nonneg (x, Jst.dep3 rho tau2 tau3)
	| Some(tau1), None, Some(tau3) ->
	    Simplex.Infsys.process_nonneg (y, Jst.dep3 rho tau1 tau3)
	| Some(tau1), Some(tau2), None ->
	    Simplex.Infsys.process_nonneg (z, Jst.dep3 rho tau1 tau2)
	| _ ->
	    ()

  let rec linarith x (a, rho) = 
    assert(Term.is_var x && Flat.is a);
    Trace.call 5 "Nl.deduce(la)" (x, a, rho) Jst.Equal.pp;
    (let y = Flat.lhs a and z = Flat.rhs a in  (* [rho |- x = y*z]. *)
     let theta = ref rho in
     let x' = replace theta x 
     and y' = replace theta y
     and z' = replace theta z in
       if x == x' && y == y' && z == z' then () else
	 let a' = Nonlin.mk_mult y' z' in
	   if not(Term.eq x' a') then 
	     Ac.remove (x, a, rho);
	   let a'' = linearize theta a' in
	     assert(A.is_pure a'');
	     Ac.remove (x, a, rho);
	     Simplex.Infsys.process_equal (x', a'', !theta));
    Trace.exit0 5 "Nl.deduce(la)"

  and replace theta x =
    try
      let (a, rho) = Simplex.Eqs.apply (Simplex.Infsys.current()) x in
	theta := Jst.dep2 rho !theta; a
    with
	Not_found -> x
    
		
  (** Returns a linear arithmetic term obtained by renaming nonlinear subterms. *)
  and linearize rho a =
    assert(Nonlin.is_pure a);
    let varify a =
      assert(Monomial.is_canonical a);
      assert(not(Monomial.is_one a));
      let m = Nonlin.monomial_of a in
	if Term.is_var m then a else
	  let q = Nonlin.coeff_of a in
	  let x = Ac.var_of_pure rho m in
	    assert(Term.is_var x);
	    A.mk_multq q x 
    in
    let a' = Nonlin.map varify a in
      assert(A.is_pure a');
      a'

  let linearize rho =
    Trace.func 7 "Linearize" Term.pp Term.pp (linearize rho)
      
  (** Propagation of disjunctions [x = 0 || y = 1] as 
    obtained from [x = y * z] if [y == x] or [z == x]. *)
  let rec disj x (a, rho) =    
    assert(Term.is_var x && Flat.is a); 
    Trace.call 5 "Nl.disj" (x, a, rho) Jst.Equal.pp;
    (let y = Flat.lhs a and z = Flat.rhs a in
      if y == x then                  (* [rho |- x = x*z]. *)
	begin
	  Ac.remove (x, a, rho);
	  process_disj (x, A.mk_decr z, rho); 
	end 
      else if z == x then             (* [rho |- x = y*x]. *)
	begin
	  Ac.remove (x, a, rho);
	  process_disj (x, A.mk_decr y, rho)
	end);
    Trace.exit0 5 "Nl.disj"
	  
  (** Process [a = 0 || b = 0] by introducing a new boolean variable [z]
    and processing [a = a * z] and [b = b * (1 - z) = b - b * z]. *)
  and process_disj (a, b, rho) =
    assert(A.is_pure a && A.is_pure b);
    let is_zero a =
      try if Mpa.Q.is_zero (A.d_num a) then Three.Yes else Three.No
      with Not_found -> Three.X
    in
    let theta = ref rho in
    let a = replace theta a in
    let b = replace theta b in
      match is_zero a, is_zero b with
	| Three.Yes, _ -> ()
	| _, Three.Yes -> ()
	| Three.No, Three.No -> 
	    raise(Jst.Inconsistent(!theta))
	| Three.No, _ -> 
	    Simplex.Infsys.process_equal_zero (b, !theta)
	| _, Three.No -> 
	    Simplex.Infsys.process_equal_zero (a, !theta)
	| Three.X, Three.X ->
	    let z = Term.mk_fresh_var "nl" in 
	      Simplex.Infsys.process_nonneg (z, Jst.dep0);              (* [0 <= z <= 1]. *)
	      Simplex.Infsys.process_nonneg (A.mk_incr (A.mk_neg z), Jst.dep0);
	      let e' = linearize theta (A.mk_sub a (Nonlin.mk_mult z a)) in
	      let e'' = linearize theta (Nonlin.mk_mult z b) in
		Simplex.Infsys.process_equal_zero (e', !theta);         (* [a - z*a = 0]. *)
		Simplex.Infsys.process_equal_zero (e'', !theta)         (* [z * b = 0]. *)
		  
  let all x ((a, _) as cod) =
    assert(Term.is_var x && Flat.is a);
    Trace.call 4 "Nl.deduce" x Term.pp;
    cnstrnts x cod;
    nonneg x cod;
    disj x cod;
    linarith x cod;
    Trace.exit0 4 "Nl.deduce"
    
      
end 
*)

(** Inference System for nonlinear monomials. 
  The configuration consists of equalities [x = y * z]. *)
module Component = struct
  let th = Nonlin.theory
  module Config = struct
    type t = Ac.Config.t
    let empty = Ac.Config.empty
    let is_empty = Ac.Config.is_empty
    let pp = Ac.Config.pp
    let dep = Ac.Config.dep
    let model = Ac.Config.model
    module Apply = Ac.Config.Apply
    module Inv = Ac.Config.Inv
    module Replace = Ac.Config.Replace
    module Diseq = Ac.Config.Diseq
    module Equal = Ac.Config.Equal
    module Nonneg = struct
      let test s t = failwith "to do"
      let justify s t = failwith "To do"
    end 
    module Pos = struct
      let test s t = failwith "to do"
      let justify s t = failwith "To do"
    end 
  end
  module Infsys = Ac.Infsys
end

(*
module Infsys = struct

  let current = Ac.current
  let initialize = Ac.initialize
  let is_unchanged = Ac.is_unchanged
  let finalize = Ac.finalize
  let abstract = Ac.abstract
  let normalize = Ac.normalize

  let crossmultiply_equal e =
    let a, b =  Monomial.crossmultiply (e#lhs, e#rhs) in
      failwith "to do"

  let crossmultiply_diseq e =
    let a, b =  Monomial.crossmultiply (e#lhs, e#rhs) in
      failwith "to do"
	
  (** Processing an equality [a = b]. Denumerators are eliminated
    by means of crossmultiplication. *)
  let process_equal e =
    assert(Monomial.is_pure e#lhs && Monomial.is_pure e#rhs);
    let e = crossmultiply_equal e in
    let s = e#lhs and t = e#rhs in
      match Monomial.is_one s, Monomial.is_one t with
	| true, true -> ()
	| true, false ->                     (* [e1 |- x = 1], [s=1] *)
	    let x, e1 = Simplex.Infsys.alias Mpa.Q.one in
	      assert(Term.is_var x); 
	      Ac.process_equal (J.mk_trans e1 e)
	| false, true ->                      (* [e1 |- x = 1], [t=1] *)
	    let x, e1 = Simplex.Infsys.alias Mpa.Q.one in
	      assert(Term.is_var x);
	      Ac.process_equal (J.mk_trans e1 (J.mk_sym e))
	| false, false ->
	    Ac.process_equal e
	      
  (** Processing a disequality [a <> b]. Denumerators are 
    eliminated by means of crossmultiplication. *)
  let process_diseq d =
    assert(Monomial.is_pure d#lhs && Monomial.is_pure d#rhs);  
    let d = crossmultiply_diseq d in         (* [d |- a <> b]. *)
    let a = d#lhs and b = d#rhs in
      match Monomial.is_one a, Monomial.is_one b with
	| true, true -> 
	    raise(Judgement.Unsat(J.mk_bot d))
	| true, false ->                     (* [e1 |- x = 1], [a == 1]. *)
	    let x, e1 = Simplex.Infsys.alias Mpa.Q.one in
	      assert(Term.is_var x);  
	      Ac.process_diseq (J.mk_replace_in_diseq e1 (J.mk_refl b) d)
	| false, true -> 
	    let x, e1 = Simplex.Infsys.alias Mpa.Q.one in
	      assert(Term.is_var x); 
	      Ac.process_diseq (J.mk_replace_in_diseq (J.mk_refl a) e1 d)
	| false, false ->
	    Ac.process_diseq d
	      
  (** Processing [a >= b]. *)
  let process_nonneg n =
    assert(Monomial.is_pure n#arg);
    let e = Deduce.linearize n#arg in
    let n' = J.mk_replace_in_nonneg e n in
      Simplex.Infsys.process_nonneg n'
  
  let propagate_equal x =
    assert(Term.is_var x);
    Ac.propagate_equal x
        
  let propagate_cnstrnt x =
    assert(Term.is_var x);
    ()
 (*   if not(is_empty()) then 
      begin 
	Trace.call 3 "Nl.prop(c)" x Term.pp;
	Ac.iter_on x Deduce.cnstrnts;
	Trace.exit0 3 "Nl.prop(c)"
      end 
 *)

  let propagate_restricted x =
    assert(Term.is_var x);
    ()
(*
    if not(is_empty()) then
      begin 
	Trace.call 3 "Nl.prop(>=0)" x Term.pp;
	Ac.iter_on x Deduce.nonneg;
	Trace.exit0 3 "Nl.prop(>=0)"
      end 
*)
	
  let propagate_linarith x = 
    assert(Term.is_var x);
    ()
(*
    if not(is_empty()) then
      begin 
	Trace.call 3 "Nl.prop(la)" x Term.pp;  
	Ac.iter_on x Deduce.linarith;
	Trace.exit0 3 "Nl.prop(la)"
      end
*)
	
  let _ = 
    Simplex.Effect.register propagate_linarith

(*
  let _ = 
    Ac.register Deduce.all
*)

  let branch () = raise Not_found
end 
*)

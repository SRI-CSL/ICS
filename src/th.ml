
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

(*i*)
open Term
open Hashcons
(*i*)

module type INTERP = sig
  val name : string  
  val is_dom : Term.t -> bool
  val is_cod : Term.t -> bool
  val norm : rho:(Term.t -> Term.t) -> Term.t -> Term.t
  val solve : Term.t * Term.t -> (Term.t * Term.t) list
  val iter : (Term.t -> unit) -> Term.t -> unit     
end


module type S = sig
  type t
  val subst_of : t -> Term.t Term.map
  val use_of : t -> Term.set Term.map
  val empty: t
  val apply: t -> Term.t -> Term.t  
  val find: t -> Term.t -> Term.t
  val inv : t -> Term.t -> Term.t 
  val use : t -> Term.t -> Term.set 
  val extend : Term.t * Term.t -> t -> t
  val process : Term.t * Term.t -> t -> t
  val propagate: Term.t * Term.t -> t -> t
end
  

module Make(Th: INTERP) = struct

  open Th

  module T = Subst.Make(
    struct
      let iter = Th.iter
    end)

  type t = T.t

  let subst_of = T.subst_of
  let use_of = T.use_of
  let empty = T.empty
  let apply = T.apply
  let find = T.find
  let inv = T.inv
  let use = T.use

  
(*s Normalization function applied to logical context. *)

  let norm s = 
    let rho = T.subst_of s in
    Th.norm (fun x -> try Ptmap.find x rho with Not_found -> x)


(*s Normalization w.r.t. an association list. *)

  let norml l =
    Th.norm (fun x -> try Term.assq x l with Not_found -> x)

(*s Merging. *)

  let union a b s =
    Trace.msg 2 ("Union(" ^ name ^ ")") (a,b) Pretty.eqn;
    T.update a b s

  let restrict x s =
    Trace.msg 2 ("Restrict(" ^ name ^ ")") x Pretty.term;
    T.restrict x s

  let merge but el s =
    let nrm = norml el in                     (* normalize w.r.t. to solved form *)
    let pusheq a b = 
      match but with
	| Some(a',b') when (a' === a && b' === b) || (a' === b && a === b') -> ()
	| _ -> Pending.push name (Atom.mk_equal a b)
    in 
    let rec merge1 (a,b) =
      if a =/= b then                     
	begin
	  if is_external b then
	    pusheq a b
	  else 
	    (try 
	       let a' = T.inv s b in  (* [a' |-> b] already known. Thus, *) 
	       if a =/= a' then       (* infer new equality [a = a'] if [a =/= a'], *)
		 begin                (* and update to [a |-> b] if [a] is more *)
		   pusheq a a';       (* canonical then [a']. *)
		   if a <<< a' then
		     begin
		       restrict a' s;
		       union a b s
		     end
		 end
	     with
		 Not_found ->
		   union a b s);
	  Ptset.iter                        
	    (fun u ->   
	       let a' = T.inv s u in       
               let b' = nrm u in
	       let e' = (a',b') in
	       if is_external b' then   (* for newly generated equality [a' = b'], *)
		  restrict a' s;        (* remove [a'] from the context. *)
	       merge1 e')
	    (T.use s a)
	end
    and is_external b =
      is_dom b || Term.args_of b = []      (* Release interpreted constants, too. *)
    in
    List.iter merge1 el

  let process ((a,b) as e) s =   
    Trace.msg 3 ("Process(" ^ name ^ ")") e Pretty.eqn;
    let el = solve (norm s a, norm s b) in
    let t = T.copy s in
    merge None el t;
    if T.eq s t then s else t


 (*s Propagate is like process but there are no side effects
   on the original formula. *)
      
  let propagate ((a,b) as e) s =
    Trace.msg 3 ("Prop(" ^ name ^ ")") e Pretty.eqn;
    let el = solve (norm s a, norm s b) in
    let t = T.copy s in
    merge (Some(a,b)) el t;
    if T.eq s t then s else t


 (*s [extend s (a,b)] extends the domain of [s]
  with [a] and destructively updates [s] such that
  [find s a] equals [b], [inv s b] equals [a], and
  the [use] structure of all [x] that occur interpreted
  in [b] are updated to also contain [b]. It assumes
  that the argument term [a] is not yet in the domain 
  of [s], i.e. [mem s a] is assumed to be false. *)

 
  let extend (a,b) s = 
    assert(not(T.mem s a));
    Trace.msg 3 ("Extend(" ^ name ^ ")") (a,b) Pretty.eqn;
    let t = T.copy s in
    T.update a b t;
    t

end


(*s Arithmetic context. *)

module A = Make(
  struct
    let name = "a"
    let is_dom a = not(Linarith.is_interp a)
    let is_cod = Linarith.is_interp
    let norm = Linarith.norm
    let solve = 
      let not_is_rename x = not (Rename.is x) in
      Linarith.solve_for not_is_rename
    let iter = Linarith.iter
  end)

(*s Tuple context. *)

module T = Make(
  struct
    let name = "t"
    let is_dom a = not(Tuple.is_tuple a || Tuple.is_fresh a)
    let is_cod b =  Tuple.is_fresh b || Tuple.is_tuple b 
    let norm = Tuple.norm  
    let solve = Tuple.solve
    let iter = Tuple.iter
  end)

(*s Bitvector context. *)

module BV = Make(
  struct
    let name = "bv"
    let is_dom a = not(Bv.is_interp a || Bv.is_fresh a)
    let is_cod b =  Bv.is_fresh b || Bv.is_interp b 
    let norm = Bv.norm 
    let solve = Bv.solve
    let iter = Bv.iter
  end)

(*s Nonlinear arithmetic *)

module NLA = Make(
  struct
    let name = "nla"
    let is_dom a = not(Nonlin.is_interp a)
    let is_cod b = Nonlin.is_interp b
    let norm = Nonlin.norm 
    let solve = Nonlin.solve Rename.make
    let iter = Nonlin.iter
  end)


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

type t = {
  th : Theory.t;
  signature : Funsym.Set.t;
  rewrites : Axioms.Rewrite.t list;
  chains : Axioms.Chain.t list;
}

let pp fmt sp =
  Format.fprintf fmt "@[theory %s\n" (Theory.to_string sp.th);
  Format.fprintf fmt "@;<1 3>signature ";
  Pretty.set Funsym.pp fmt (Funsym.Set.elements sp.signature);
  Format.fprintf fmt "@;<1 3>rewrites ";
  Pretty.list Axioms.Rewrite.pp fmt sp.rewrites;
  Format.fprintf fmt "@;<1 3>chains ";
  Pretty.list Axioms.Chain.pp fmt sp.chains;
  Format.fprintf fmt "end@]@;"
  

let make th s rl cl = {
  th = th;
  signature = s;
  rewrites = rl;
  chains = cl
}


module type SPEC = sig
  val th : Theory.t
  val signature : Funsym.Set.t
  module Axs : Axioms.AXIOMS
end

module type S = sig
  val th : Theory.t
 (* module Sig : Funsym.SIG *)
  val map : (Term.t -> Term.t) -> Term.t -> Term.t
  val can : Term.interp
  val is_diseq : Term.t -> Term.t -> bool
  module Infsys : Can.INFSYS
end

module Make(Spec: SPEC): S = struct

  let th = Spec.th
  
  module Sig = Funsym.Make(
    struct
      let th = Spec.th
      type t = Name.t
      let name n = n
    end)

  let is_theory = Theory.eq th
	    
  let op a =
    let f = Term.sym_of a in
      if is_theory (Funsym.theory_of f) then
	f
      else
	raise Not_found

  let args = Term.args_of

  module Ops = Axioms.Compile(Spec.Axs)

  let can = Ops.normalize

  let is_diseq = Ops.is_diseq  

  let map f =
    let rec mapf a = 
      try
	let f = op a and al = args a in
	let al' = Term.Args.map mapf al in
	  if al == al' then a else can f al'
      with
	  Not_found -> f a
    in
      mapf

  module Th: Can.T = struct
    let th = th
    let can = can
    let map = map
    let chains =
      Spec.Axs.chains @
      List.map Axioms.Rewrite.to_chain Spec.Axs.rewrites
    let disjunction _ = raise Not_found
  end
	
  (** Inference system for defined theory *)
  module Infsys: Can.INFSYS = 
    Can.Infsys(Th)
	   
end


module Register(S: S) = struct

(*
  let _ = 
    Funsym.name_register S.th
*)

  let _ = 
    let m = Term.Methods.empty() in
      m.Term.Methods.can <- Some(S.can);
      m.Term.Methods.is_diseq <- Some(S.is_diseq);
      Term.Methods.register S.th m

(*
  module Unit = 
    Can.Register(S.Infsys)
*)

end

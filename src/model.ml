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


type t = Interp.t * Assign.t

module Eval = struct
  let term rho a = a
  let atom rho b = true
end 

(*
module type S = sig
  type dom

  module I: (Interp.S with type d = dom)
  module A: (Assign.MAP with type d = dom)

  type t = I.t * A.t

  module Eval : sig
    val term : t -> Term.t -> dom
    val atom : t -> Atom.t -> bool
  end 

end

module Make(D: Assign.DOMAIN) = struct

  type dom = D.d

  module I = Interp.Make(D)
  module A = Assign.Map(D)

  type t = I.t * A.t

  module Eval = struct

    (** Inside-out evaluation of terms *)
    let term (i, alpha) =
      let rec eval a = 
	if Term.is_var a then 
	  A.apply alpha a
	else
	  let f = Term.sym_of a and al = Term.args_of a in
	  let vl = List.map eval al in
	    I.apply i f vl
      in
	eval

    let atom m = 
      let value = term m in
      let rec eval atm =
	match Atom.atom_of atm with
	  | Atom.TT -> true
	  | Atom.FF -> false
	  | Atom.Equal(a, b) -> D.eq (value a) (value b)	
	  | Atom.Diseq(a, b) -> not (D.eq (value a) (value b))
	  | Atom.Ineq(a, b) -> D.ineq (value a) (value b)
	  | Atom.Cnstrnt(a, c) -> D.cnstrnt c (value a)
	  | Atom.Conj(atml) -> List.for_all eval atml
      in
	eval
      
  end 

end

module Term: (S with type dom = Term.t) = 
  Make(Assign.Ground)
*)


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

(*s Module [Arity]: arity for uninterpreted function symbols.
 The arity of a constant is simply a type (see module [Type]),
 whereas a functorial arity is of the form [d1,...,dn -> r],
 where [di] are the domain types and [r] is the range type.
 In addition, binary operators of arity [c,c -> c] may have
 an attribute (see module [Attribute]) for specifying additional
 properties such as associativity or commutativity.  With each
 arity we associate the obvious domain of interpretation. *)

type t 

type arity =
  | Constant of Type.t
  | Functorial of Type.t list * Type.t

(*s [eq a b] holds iff the interpretations of [a] and [b] are
 equal. This is a constant time operation. *)

val eq : t -> t -> bool

(*s Constructors for making constant and functorial aritites.
  A [mk_functional dl r] call with an empty list [dl] of domain types 
  reduces to a arity as obtained by [mk_constant r]. *)

val mk_constant : Type.t -> t
val mk_functorial : Type.t list -> Type.t -> t

(*s Destructing an arity. *)   

val destruct : t -> arity


(*s Pretty-printing. *)

val pp : Format.formatter -> t -> unit




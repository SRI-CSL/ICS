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

(** Propagation stacks

  @author Harald Ruess
*)

module type EQ = sig 
  type t 
  val eq: t -> t -> bool 
end

module type S = sig
  type t
  val clear : unit -> unit
  val is_empty : unit -> bool
  val put : t -> unit
  val get : unit -> t
  val to_list: unit -> t list
end


module Make(T: EQ): (S with type t = T.t) = struct

  type t = T.t

  let st: t Stacks.t = Stacks.create () 

  let clear () = Stacks.clear st

  let is_empty () = Stacks.is_empty st 

  let mem e = Stacks.mem T.eq e st

  (** Only pushed when not already on stack. *)
  let put e = 
    if mem e then () else Stacks.push e st

  let get () =
    try
      Stacks.pop st
    with
	Stacks.Empty -> raise Not_found

  let to_list () = Stacks.to_list st

end

module Var = struct
  type t = Term.t
  let eq = (==)
end

module Equal = Make(Var)

module Diseq = Make(
  struct
    type t = Term.t * Term.t
    let eq (x1, y1) (x2, y2) =
      (x1 == x2 && y1 == y2)
  end)
  
module Cnstrnt = Make(Var)

module Nonneg = Make(Var)

let is_empty () =
  Equal.is_empty () &&
  Diseq.is_empty () &&
  Cnstrnt.is_empty () &&
  Nonneg.is_empty ()

let clear () =
  Equal.clear();
  Diseq.clear();
  Cnstrnt.clear();
  Nonneg.clear()
  

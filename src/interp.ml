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

type dom = Term.t

module I = Map.Make(
  struct
    type t = Funsym.t * Term.t list
    let rec compare (f, al) (g, bl) =
      let c = Funsym.cmp f g in
	if c <> 0 then c else Term.cmpl al bl
  end)


type t = dom I.t

let empty = I.empty

let update f al = I.add (f, al)

let to_list rho =
  I.fold
    (fun (f, al) v acc -> 
       (f, al, v) :: acc)
    rho []

let pp fmt rho = failwith "to do"



(*
module type S = sig

  type d
  type t
  val empty : t
  val update : Funsym.t -> d list -> d -> t -> t  
  val of_fun : (Funsym.t -> d list -> d) -> t
  val apply : t -> Funsym.t -> d list -> d
end

module Make(D: Assign.DOMAIN): (S with type d = D.d) = struct

  type d = D.d

  type t = Funsym.t -> d list -> d

  let of_fun f = f

  let empty _ _ = raise Not_found

  let apply i f al = i f al
  
  let rec update f vl v i =
    fun g wl -> 
      if Funsym.eq f g && args_eq vl wl then v else i g wl

  and args_eq vl wl = 
    try List.for_all2 D.eq vl wl with Invalid_argument _ -> false

end


module Term: (S with type d = Term.t) = 
  Make(Assign.Ground)

*)

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

(** Inference system for global inputs facts.

  @author Harald Ruess
*)

exception Empty

(** Inference system with sets of {i input facts} as states.  
  These states can not be saved as inference systems run to completion
  by exhausting the set of available inputs. *)
module Infsys : sig

  val initialize : Judgement.atom list -> unit
    (** Initialize the current input facts with the facts in the argument list. *)

  val reset : unit -> unit
    (** Clear the set of input facts. *)

  val is_empty : unit -> bool
    (** [is_empty ()] holds if there are no current input facts. *)

  val pp : Format.formatter -> unit -> unit
    (** Pretty-print the current configuration of input facts. *)

  val get : unit -> Judgement.atom
    (** [get ()] selects an arbitrary input fact and deletes it 
      from the current set of inputs. Otherwise, {!G.Empty} is returned. *)

  val put : Judgement.atom -> unit
    (** Add an input fact to the current set of input facts. *)
 
end

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

(** Definition and operations on inference systems.

  @author Harald Ruess
*)


(** A {i configuration} [(g; e; p)] consists of
  - Input facts [g],
  - an equality set [e], and
  - a variable partitioning [p]. *)
module Config: sig

  type 'e t = G.t * 'e * Partition.t
  
  val empty : 'e -> 'e t

  val is_empty : ('e -> bool) -> 'e t -> bool
    
  val pp : 'e Pretty.printer -> 'e t Pretty.printer
    (** Pretty-printing configurations. *)

end


(** Global variables for
  - current input facts [g]
  - current variable partitioning [p]. *)
val g : G.t ref
val p : Partition.t ref


(** Abstract interface of an inference system for equality theories. *)
module type EQ = sig

  type e

  val current : unit -> e

  val initialize : e -> unit
    (** Intitialize inference system with equality set. *)
  
  val finalize : unit -> e
    (** Retrieve modified equality set. *)

  val abstract : Term.t -> unit
    (** [(g[a]; e; p)] ==> [(g[x]; e, x = a; p)]
      with 
      - [a] a nonvariable term, 
      - [a] an [i]-pure term, 
      - and [x] fresh. *)
    
  val merge : Fact.Equal.t -> unit
    (** [(g, a = b; e; p)] ==> [(g; e'; p')] 
      with 
      - [a], [b] [i]-pure, 
      - [|= e', p' <=> |= e, a = b, p]
      - if [e' |= x = y] then [p' |= x = y]. *)

  val propagate : Fact.Equal.t -> unit
    (** [(g, e; p)] ==> [(g; e'; p)]
      with 
      - [e |= x = y], 
      - not[p |= x = y], 
      - [|= e, p <=> |= e', p']  *)
  
  val dismerge : Fact.Diseq.t -> unit
    (** [(g, a <> a; e; p)] ==> [(g; e'; p')] 
      with [a], [b] [i]-pure, [|= e', p' <=> |= e, p, a <> b]. *)

  val propagate_diseq : Fact.Diseq.t -> unit
    (** [(g; e; p)] ==> [(g; e'; p')] 
      with 
      - [p' |= x <> y]
      - [|= e', p' <=> |= e, p]. *)

  val branch : unit -> unit
    (** [(g; e; p)] ==> [(g, c1; e; p) | ... | (g, cn; e; p)]
      with 
      - [e, p |= c1 \/ ... \/ cn]
      - not [e, p |= ci] *)

  val normalize : unit -> unit
    (**  [(g; e; p)] ==> [(g'; e'; p')]
      where source and target configuration are equivalent. *)

end



(** Abstract interface of an inference system for arithmetic theories
  as an extension of the {!Infsys.EQ} signature. *)
module type ARITH = sig

  type e

  val current : unit -> e
       
  val initialize : e -> unit
  val finalize : unit -> e
  val abstract : Term.t -> unit
  val merge : Fact.Equal.t -> unit
  val propagate : Fact.Equal.t -> unit
  val dismerge : Fact.Diseq.t -> unit
  val propagate_diseq : Fact.Diseq.t -> unit
  val branch : unit -> unit
  val normalize : unit -> unit
    (** Above is identical to {!Infsys.EQ}. *)

  val nonneg : Fact.Nonneg.t -> unit
    (** [(g, a >= 0; e; p)] ==> [(g; e'; p')] 
      with 
      - [a] pure
      - [|= e', p' <=> |= e, a >= 0, p]
      - if [e' |= x = y] then [p' |= x = y]. *)

  val pos : Fact.Pos.t -> unit
    (** [(g, a > 0; e; p)] ==> [(g; e'; p')] 
      with 
      - [a] pure
      - [|= e', p' <=> |= e, a > 0, p]
      - if [e' |= x = y] then [p' |= x = y]. *)

end


module Empty: EQ
  (** Inference system for the empty theory. *)
  

val difference : bool ref

(** Specification of tracer for inference system. *)
module type LEVEL = sig 
  type t
  val level : Trace.level
  val eq: t -> t -> bool 
  val diff : t -> t -> t
  val pp : t Pretty.printer
end

module Trace(I: EQ)(L: (LEVEL with type t = I.e)): (EQ with type e = I.e)
  (** Tracing rule applications. *)

module TraceArith(I: ARITH)(L: (LEVEL with type t = I.e)): (ARITH with type e = I.e)
  (** Tracing rule applications. *)

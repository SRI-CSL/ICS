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

(** Inference system for Shostak theories.

  @author Harald Ruess
*)

(** A Shostak theory [th] is specified by means of a
  - replacement [map f a] for replacing uninterpreted 
    subterms of [a] with [f a] and canonizing the result, and
  - a {i solver} [solve]. 
  - an extended {i canonizer} [map f a] for replacing uninterpreted
    positions [x] of [a] with [f b] followed by canonization in the
    given theory, and
  - a {i branching} function [disjunction]. If [disjunction] always returns [Not_found],
    then [th] is also said to be a {i convex} Shostak theory. *)
module type T = sig
  val th : Theory.t   
  val can : Term.interp
  val solve : Term.t -> Term.t ->  Term.Set.t * Term.Subst.t
  val disjunction : ((Judgement.equal -> unit) -> unit) -> Judgement.disjunction
end


(** A {i configuration} of a Shostak inference system consists of
  a finite solution set [{x{1} = t{1}, ..., x{n} = t{n}}] such that
  - [x{i}] are term variables,
  - [x{i}], x{j}] are pairwise disjoint,
  - none of the [x{i}] occurs in any [a{j}], 
  - [t{i}] might contain fresh variables from solving,
  - [x{i}] might be an {i aliasing} variable. *)
module type CONFIG = sig
  type t  
  val is_empty : t -> bool  
  val empty : unit -> t  
  val pp : Format.formatter -> t -> unit  
  val is_fresh : t -> Term.t -> bool
  val in_dom : t -> Term.t -> bool
  val in_cod : t -> Term.t -> bool
  val dep : t -> Term.t -> Dep.Set.t
  val occ : t -> Term.t -> bool
  val iter : t -> (Judgement.equal -> unit) -> unit
  val model : t -> Term.Model.t
  module Apply : sig
    val get : t -> Term.t -> Term.t
    val justify : t -> Term.t -> Judgement.equal
  end 
  module Inv : sig
    val get : t -> Term.t -> Term.t
    val justify : t -> Term.t -> Judgement.equal
  end 
  module Replace : sig
    val get : t -> Term.t -> Term.t
    val justify : t -> Term.t -> Judgement.equal
  end 
  module Diseq : sig
    val test : t -> Term.t -> Term.t -> bool
    val justify : t -> Term.t -> Term.t -> Judgement.diseq
  end
  module Equal : sig
    val test : t -> Term.t -> Term.t -> bool
    val justify : t -> Term.t -> Term.t -> Judgement.equal
  end
end 


(** As an invariant, equality sets for representing contexts
  of Shostak theories are {i ordered} equalities of the 
  form [x = a] with [x] a variable and [a] a nonvariable, 
  [Sh.th]-pure term.  In addition, these equality sets are in
  - {i solved form}, that is, there are no [x = a], [y = b]
  with [x] a variable in [b].
  - {i canonical}, that is, if [x = y] has been propagated
  using the [propagate] rule, then the {i noncanonical} [x] does 
  not appear in the equality set.  Also, right-hand sides are
  always kept in canonical form w.r.t to the given theory canonizer [map].
  
  In case of {i convex} Shostak theories, [disjunction] always fails,
  and there is no branching. 
  
  In case of {i incomplete} Shostak theories, [solve] might raise
  {!Exc.Incomplete} on an equality [a = b].  Now, [a], [b] are
  named apart and the corresponding variable equality is merged.
  Notice, that incomplete solvers might lead to an incompleteness in
  the inference procedure. *)
module type INFSYS = sig

  type config

  val current : unit -> config
    
  val initialize : config -> unit
    (** Intitialize inference system with equality set. *)
    
  val finalize : unit -> config
    (** Retrieve modified equality set. *)
    
  val reset : unit -> unit

  val is_unchanged : unit -> bool
    (** [is_unchanged ()] holds if [current()] has not
      changed since last [initialize()]. *)
  
  val abstract : Term.t -> Judgement.atom -> unit 
    (** [(g[a]; e; p)] ==> [(g[x]; e, x = a; p)]
      with 
      - [a] a nonvariable term, 
      - [a] an [i]-pure term, 
      - and [x] fresh. *)
    
  val process_equal : Judgement.equal -> unit
    (** Processing equalities:
      [(g, a = b; e; p)] ==> [(g; e'; p')] with 
      - [a], [b] [i]-pure, 
      - [|= e', p' <=> |= e, a = b, p]
      - if [e' |= x = y] then [p' |= x = y]. *)
    
  val process_diseq : Judgement.diseq -> unit
    (** Processing disequalities:
      [(g, a <> b; e; p)] ==> [(g; e'; p')] 
      with [a], [b] [i]-pure, [|= e', p' <=> |= e, p, a <> b]. *)

  val propagate_equal : Term.t -> unit
    (** Propagate variable equalities:
         [(g, e; p)] ==> [(g; e'; p)] 
          with 
            - [e |= x = y], 
            - not[p |= x = y], 
            - [|= e, p <=> |= e', p']. *)

  val propagate_diseq : Judgement.diseq -> unit
    (** Propagate variable disequalites: 
      [(g; e; p)] ==> [(g; e'; p')] 
      with 
      - [p' |= x <> y]
      - [|= e', p' <=> |= e, p]. *)

  val branch : unit -> Judgement.disjunction option
    (** [(g; e; p)] ==> [(g, c1; e; p) | ... | (g, cn; e; p)]
      with 
      - [e, p |= c1 \/ ... \/ cn]
      - not [e, p |= ci] *)

  val normalize : unit -> unit
    (**  [(g; e; p)] ==> [(g'; e'; p')], where source and target 
      configuration are equivalent. *)
end

module type COMPONENT = sig
  val th : Theory.t
  module Config : CONFIG
  module Infsys : (INFSYS with type config = Config.t)
end

module Make(Sh: T): COMPONENT

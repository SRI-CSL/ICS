(*
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
 *)

(** Abstract datatype for variables.

 The set of all variables is partitioned into {b external}, {b rename}, {b fresh},
 and {b bound} variables.  There is a name of type {!Name.t} associated with 
 each such variable. Names for rename and fresh variables are always of the form ["x!i"], 
 where [x] is an arbitrary string and [i] is an integer string. The name associated 
 with a free variable is of the form ["!i"] for an integer [i].

 @author Harald Ruess
*)

type t =  
  | Slack of int * slack
  | External of Name.t * Dom.t option
  | Rename of Name.t * int  * Dom.t option
  | Fresh of Th.t * int * Dom.t option
  | Bound of int   

and slack = Zero | Nonneg of Dom.t
      

(** {6 Destructors} *)

val name_of : t -> Name.t
  (** [name_of x] returns the name associated with a variable [x]. *)

val dom_of : t -> Dom.t
  (** [dom_of x] returns the domain of interpretation of variable [x],
    and [None] if this domain is "unrestricted". *)


(** {6 Comparisons} *)

val cmp : t -> t -> int
  (** [cmp x y] realizes a total ordering on variables. The result is [0]
    if [eq x y] holds, it is less than [0] we say, '[x] is less than [y]',
    and, otherwise, '[x] is greater than [y]'. An external variable [x] is always
    larger than a rename variable [y]. Otherwise, the outcome of [cmp x y] is 
    unspecified. *)


val (<<<) : t -> t -> bool
  (** [x <<< y] holds iff [cmp x y <= 0]. *)


(** {6 Hashvalue} *)

val hash : t -> int
  (** compute an integer (not necessarily unique) for a variable. *)


(** {6 Constructors} *)

val mk_external : Name.t -> Dom.t option -> t
  (** [mk_var x] creates an external variable with associated name [x]. *)


val mk_rename : Name.t -> int -> Dom.t option -> t
  (** [mk_rename n None] constructs a rename variable with associated name
    ["n!i"], where [i] is the current value of the variable {!Var.k} above. As
    as side-effect, {!Var.k} is incremented by one.  [mk_rename n Some(i)]
    constructs a rename variable with associated name ["n!i"]; there are no
    side effects on {!Var.k}. *)

val mk_slack : int -> slack -> t

val mk_fresh : Th.t -> int -> Dom.t option -> t

val mk_free : int -> t
  (** [mk_free i] constructs a free variable with associated name [!i]. *)


(** {6 Recognizers} *)

val is_var : t -> bool
  (** [is_var x] holds iff [x] is an external variable. *)
  
val is_rename : t -> bool
  (** [is_rename x] holds iff [x] is a rename variable. *)

val is_slack : slack -> t -> bool
  (** [is_cnstrnt x] holds iff [x] is a slack variable. *)

val is_zero_slack : t -> bool

val is_nonneg_slack : t -> bool


val is_fresh : Th.t -> t -> bool
  (** [is_fresh i x] holds iff [x] is a fresh variable of theory [i]. *)

val is_free : t -> bool
  (** [is_free x] holds iff [x] is a free variable. *)

val is_internal : t -> bool


(** Interpretation domains *)

val is_real : t -> bool
val is_int : t -> bool


(** {6 Destructors} *)

val d_free : t -> int
  (** For a free variable [x], [d_free x] returns [i], if ["!i"] is
    the name associated with [x].  The result is undefined for non-free
    variables. *)


(** {6 Printing} *)

val pretty : bool ref

val pp : Format.formatter -> t -> unit


(** {6 Sets and maps of terms} *)

module Set : (Set.S with type elt = t)

module Map : (Map.S with type key = t)

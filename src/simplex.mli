(* 
 * The MIT License (MIT)
 *
 * Copyright (c) 2020 SRI International
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 *)

(** {i Open Simplex inference system.}

  This module provides an {i open inference system} for 
  incrementally processing linear arithmetic constraints 
  [p >= 0] and [p = 0], [p > 0], and [p <> 0], where [p] is a 
  polynomial with an ordered field as coefficients.  All 
  implied variable equalities are propagated to the interface 
  inference system.

  @author Harald Ruess
  @author N. Shankar
*)

(** {i Polynomials.}  Input signature for {!Simplex.Make}. *)
module type POLYNOMIAL = Polynomial.P


(** {i Interface inference system.} Configurations of this
  inference system consists of a finite set of variable 
  equalities [E], variable disequalities [D], and real constraints
  [C] on variables. [E] partitions the variables into equivalence 
  classes modulo [E] with [x =E y] iff [E => x = y] is valid in the 
  theory of identity. *)
module type INTERFACE = sig
  type var
    (** Representation of variables. *)

  val find : var -> var
    (** [find x] returns the canonical representative of the
      equivalence class modulo [E] containing [x]. *)

  val canonical : var -> bool
    (** [canonical x] holds iff [x] is the canonical 
      representative of its equivalence class modulo [E]. *)

  val equal : var -> var -> bool
    (** [equal x y] holds iff [x =E y]. *)

  val diseq : var -> var -> bool
    (** [diseq x y] holds iff [E, D => x <> y] in the theory of identity. *)

  val isReal : var -> bool
    (** [isReal x] iff [E, C => real(x)] in the theory of identity. *)

  val isInteger : var -> bool
    (** [isInteger x] iff [E, C => int(x)] in the theory of identity. *)

  val union : var -> var -> unit
    (** [union x y] adds the equality [x = y] to [E]. *)

  val separate : var -> var -> unit
    (** [separate x y] adds the variable disequality [x <> y] to [D]. *)

  val real : var -> unit
    (** [real x] adds the {i real} constraint [real(x)] to [C]. *)

  val integer : var -> unit
    (** [integer x] adds the {i integer} constraint [integer(x)] to [C]. *)
end

(** {i Simplex inference system.} This inference system maintains a 
  configuration equivalent to a conjunction of linear arithmetic 
  equalities [p = 0] and inequalities [p >= 0]. More precisely, 
  configurations consist of
  - a set of (internally generated) {i slack variables} which are interpreted 
  over the nonnegative reals,
  - constant assignments [x{1} = c{1};... x{n} = c{n}] with [x{i}] non-slack 
  variables and [c{n}] rational constants,
  - a {i regular} solution set including equalities of the form [x = a] with
  [x] a non-slack variable and [a] a linear arithmetic term which is not a 
  non-slack variable,
  - a {i regular} solution set with equalities of the form [x = a] with
  [x] a non-slack variable and [a] a linear arithmetic term which is not 
  a non-slack variable,
  - a {i feasible tableau} solution set with equalities of the form [x = a] 
  with [x] a slack variable and [a] is a linear arithmetic term containing 
  only slack variables and the constant part of [a] is nonnegative. *)
module type INFSYS = sig
  type var 
    (** Representation of variables. *)

  type coeff
    (** Representation of coefficients, which must form an ordered field. *)

  type poly   
    (** Representation of linear arithmetic polynomials. *)

  type t  
    (** Configuration of the Simplex inference system. *)

  val empty : t 
    (** The empty configuration. *)

  (** {i Slack variables}. These are internally generated variables 
    which are interpreted over nonnegative coefficients. *)
  module S : sig
    module Slacks : (Sets.S with type elt = var)
    type t
      (** Representation of a set of slack variables. *)
    val current : unit -> Slacks.t
      (** Return the current set of slack variables. *)
    val mem : var -> bool
      (** [mem x] holds iff [x] is a member of the current set of 
	slack variables. *)
  end

  (** {i Regular solution set}. Configurations are finite sets [R] of the form 
    [{x{1} = p{1}, ...,x{n} = p{n}}] with
    - [x{i}] a non-slack variable, 
    - [p{i}] a linear arithmetic term which is not a non-slack variable,
    - [x{i}] not in [vars(p{j})] for all [i], [j], and
    - [x{i}] is disequal from [x{j}] for [i /\ j], and
    - [p{i}] is disequal from [p{j}] for [i /= j]. *)
  module R : sig
    type t
      (** Representation of a regular solution set. *)

    val empty: t  
      (** The empty regular solution set. *)

    module Deps : (Powermaps.S with type key = var)
    module Constant : (Maps.S with type key = var and type value = coeff)
    module Solset : (Maps.S with type key = var and type value = poly)

    val constant : unit -> Constant.t
      (** Return the subset of [R] which contains all equalities [x = c] 
	with a [c] constant. *)

    val solset : unit -> Solset.t
      (** Return the subset of [R] which contains all equalities [x = p] 
	with [p] a non-constant polynomial. *)

    val dep : unit -> Deps.t
      (** Return the current dependency index. *)

    val pp : Format.formatter -> unit
      (** Print the current regular solution set on the given formatter. *)

    val find : var -> poly
      (** [find x] returns [p] if there is [x = p] in [R]; 
	otherwise it raises [Not_found]. *)

    val findConst : var -> coeff

    val inv : poly -> var
      (** [inv p] returns [x] if there is [x = p] in [R]; otherwise 
	it raises [Not_found]. *)

    val deps : var -> Deps.Values.t
      (** [x] is in [deps y] iff there exists [x = p] in [R] 
	with [y] in [vars(p)]. *)

    val dom : var -> bool
      (** [dom x] holds iff there is an equality [x = t] in [R]. *)
    val cod : var -> bool
      (** [cod y] holds iff there is an equality [x = p] with [y] 
	in [vars(p)] in [R]. *)
  end 
  module T : sig
    type t
    val empty : t
    module Deps : (Powermaps.S with type key = var)
    module Solset : (Maps.S with type key = var and type value = poly)
    val solset : unit -> Solset.t
    val dep : unit -> Deps.t
    val pp : Format.formatter -> unit
    val find : var -> poly
    val inv : poly -> var
    val deps : var -> Deps.Values.t
    val dom : var -> bool
    val cod : var -> bool
  end

  val current : unit -> t 
    (** Return the current configuration. *)

  val is_empty : unit -> bool 
    (** [is_empty()] holds if the current configuration is empty. *)

  val find : var -> poly
    (** [find x] returns [p] if there is an equality [x = p] in the 
      current configuration; otherwise, it raises [Not_found]. *)

  val findConst : var -> coeff
    (** [find x] returns [c] if there is an equality [x = c], which [c] 
      a constant polynomial, in the  current configuration; otherwise, 
      it raises [Not_found]. *)

  val inv : poly -> var
    (** [inv p] returns [x] if there is an equality [x = p] in the current
      configuration; otherwise, it raises [Not_found]. *)

  val invConst : coeff -> var
    (** [invConst c] returns the non-slack variable [x] if there is an 
      equality [x = c] in the  current configuration with [c] a constant 
      polynomial; otherwise, it raises [Not_found]. *)

  val feasible : unit -> bool
    (** As an invariant, the tableau part is always {i feasible}, that is
      all entries are of the form [x = p] with the constant part of 
      polynomial [p] nonnegative. *)

  val synchronized : unit -> bool
    (** A configuration is {i synchronized} if all variables in the regular 
      solution set of the current configuration are {i canonical} wrt to the
      current variable equalities. *)

  val pp : Format.formatter -> unit
    (** Print the current configuration on given formatter. *)

  val can : poly -> poly
    (** In a {i synchronized} configuration, the arithmetic canonizer [can p] 
      returns a polynomial [p'] with 
      - [p = p'] valid in the current configuration,
      - for all variables [x] in [vars p'] neither [R.dom x] nor [T.dom x] 
      holds. 
      - for all variables [x] in [vars p'], [x] is canonical wrt. to the 
      variable equalities [E] of the interface inference system. *)

  val max : poly -> poly
    (** In a {i synchronized} configuration and for restricted polynomials 
      [p], that is [restricted p] holds, [max p] returns a polynomial [p'] 
      such that either [p'] is {i maximized} or it is {i unbounded} in the 
      current configuration.  As a side effect, the current tableau might 
      have changed by equivalence-preserving pivoting steps. *)

  val min : poly -> poly
    (** [min p] is synonymous with [-max(-p)]. In particular, results are 
      either{i minimized} or {i unbounded} in the current configuration. *)

  val restricted : poly -> bool
    (** [restricted p] holds iff [p] contains only slack variables. *)

  val minimized : poly -> bool
    (** [minimized p] iff [p] is of the form [c{0}+c{1}*x{1} + ... +c{n}*x{n}]
      with [c{1}] through [c{n}] is positive and [x{i}] are all slack 
      variables. *)

  val maximized : poly -> bool  
    (** [minimized p] iff [p] is of the form [c{0}-c{1}*x{1}- ... -c{n}*x{n}]
      with [c{1}] through [c{n}] is positive and [x{i}] are all slack 
      variables. *)

  val complete : bool ref
    (** Completeness flag influencing the [isNonneg], [isPos], 
      and [isDiseq0] tests. *)

  val isNonneg : poly -> bool
    (**  If [isNonneg p] holds then [p>=0] is valid 
      in the current configuration. If the [!complete] flag is set and if the
      current configuration is synchronized, then [isNonneg p] iff [p >= 0]; 
      in this case, mininimization might have been applied to [p]. *)

  val isPos : poly -> bool
    (** If [isPos p] holds then [p>0] is valid in the current configuration. 
      If the [!complete] flag is set and if the current configuration is 
      synchronized, then [isPos p] iff [p > 0]; in this case, mininimization 
      might have been applied to [p]. *)

  val isDiseq0 : poly -> bool
    (** If [isDiseq0 p] holds then [p/=0] is valid in the current
      configuration. If the [!complete] flag is set and if the current 
      configuration is synchronized, then [isPos p] iff [p/=0]; in this case, 
      mininimization might have been applied to [p]. *)

  val isEqual0 : poly -> bool
    (** If the current configuration is synchronized, then
      [isEqual0 p] holds iff [p=0] is valid in the current configuration. *)

  val initialize : t -> unit 
    (** [initialize s] initializes the current configuration 
      with the [s] configuration. *)

  val reset : unit -> unit
    (** [reset()] is synonymous with [initialize empty]. *)

  val unchanged : unit -> bool
    (** [unchanged()] holds iff the current configuration has been 
      unchanged since the latest [reset] or [initialize]. *)

  val alias : poly -> var
    (** In a synchronized configuration, [alias p] returns a canonical 
      non-slack variable [x] with [x = p] valid in a possible extended 
      configuration. *)

  val aliasConst : coeff -> var
    (**  In a synchronized configuration, [aliasConst c] returns a 
      canonical non-slack variable [x] with [x = c] valid in a possible 
      extended configuration. *)

  exception Unsat
  val processEq0 : poly -> unit
    (**  [processEq0 p] adds an equality constraint [p = 0] to the current 
      configuration, which is assumed to be synchronized. *)

  val processNonneg : poly -> unit
    (** [processNonneg p] adds an inequality constraint [p >= 0] to the 
      current configuration, which is assumed to be synchronized.  *)

  val processPos : poly -> unit

  val processDeq0 : poly -> unit

  val propagateEq : var -> var -> unit
    (** [propagateEq x y] propagates variable equalities [x = y] as derived
      by the environment inference system. As preconditions, [x =E y] must
      hold, [y] is canonical in [E], and [x] is syntactically different 
      from [y].  Also, it is assumed that, after [x = y] becomes true in 
      the environment system, [propagateEq x y] is being called before any 
      other functions of this module. this guarantees the invariant that 
      domain variables [x] are always canonical with respect to the 
      environment variable equalities. *)

  val gcSlacks : bool ref
    (** Determines if unused slacks are garbage collected or not 
      (Default: true). *)

  val normalize : unit -> unit
    (** [normalize()] normalizes the current configuration. In particular, 
      unused slacks are garbage collected if [gcSlacks] is set. No new 
      variable or variable disequalities are propagated to the environment. *)
end

(** Given an implementation [P] of polynomials and an environment 
  inference system [P], [Make] constructs a closed Simplex inference system. *)
module Make
  (P: Polynomial.P)
  (V: INTERFACE with type var = P.indet)
  : (INFSYS with type var = P.indet
	    and type coeff = P.coeff
	    and type poly = P.t)

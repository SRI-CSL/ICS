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

(** {i Binary Decision Diagrams.}

    This module includes functionality for constructing
    {i binary decision diagrams} (BDDs).

    @author Harald Ruess *)

(** For efficiency, package should be compiled with the [-noassert] option. *)

(** {i Totally ordered set of variables.} *)
module type VAR = sig
  type t

  val equal : t -> t -> bool
  val compare : t -> t -> int
  val hash : t -> int
  val pp : Format.formatter -> t -> unit
  val dummy : t
end

(** {i Reduced, ordered binary decision diagrams}.

    A {i binary decision diagram} (BDD) is either

    - a constant [0], [1] or
    - a conditional [ITE(x, p, n)] with [x] a {i variable} and [p],[n] are
      BDDs.

    [0] is interpreted as the trivially {i unsat} constant, [1] is trivially
    {i valid}, and conditionals [ITE(x,p,n)] are logically equivalent with
    [x&&p || ~x&&n] with [&&] logical conjunction, [||] logical disjunction,
    and [~] logical negation.

    A decision diagram [b] is {i reduced} if

    - it does not contain subterms of the form [ITE(x,b',b')] and
    - [b] is a maximally shared DAG.

    Furthermore, a conditional [ITE(x,p,n)] is {i ordered} if [x > y] for
    all variables in [p], [n] and conditional subterms [p], [n] are also
    ordered.

    Reduced and ordered BDDs are {i canonical} in that [b1 <=> b2] iff
    [b1 = b2]. *)
module type FML = sig
  (** Representation of variables. *)
  type var

  (** Representation of BDDs. *)
  type t

  val pp : bool * bool * int -> Format.formatter -> t -> unit
  (** [pp (infix, shared, maxdepth) fmt b] prints the BDD [b] on formatter
      [fmt].

      - If [infix] is set, then it attempts to print infix operators such as
        conjunction or disjunction;
      - If [shared] is set, then new variables are introduced for shared
        nodes;
      - If [maxdepth >= 0] then formulas are only printed up to the given
        maximal depth. *)

  val hash : t -> int
  (** Return a nonnegative hash value for a BDD. *)

  val equal : t -> t -> bool
  (** [equal b1 b2] holds iff [b1 <=> b2]. This test is performed in
      constant time. *)

  val compare : t -> t -> int
  (** [compare b1 b2] equals [0] iff [equiv b1 b2]. Furthermore,
      [compare b1 b2 > 0] iff [compare b2 b1 < 0]. For nonnegative values,
      the result of [compare] does not have any logical interpretation. *)

  val mk_true : t
  (** Representation of the trivially valid constant [1]. *)

  val mk_false : t
  (** Representation of the trivially unsatisfiable constant [0]. *)

  val mk_posvar : var -> t
  (** [mk_posvar x] returns a BDD equivalent to [ITE(x,1,0)]. *)

  val mk_negvar : var -> t
  (** [mk_posvar x] returns a BDD equivalent to the negated variable
      [ITE(x,0,1)]. *)

  val mk_neg : t -> t
  (** [mk_neg b] returns a BDD equivalent to the negation [~b] of [b]. *)

  val mk_ite : t -> t -> t -> t
  (** [mk_ite b1 b2 b3] returns a BDD equivalent [b1&&b2 || ~b1&&b3]. *)

  val mk_imp : t -> t -> t
  (** [mk_imp b1 b2] returns a BDD equivalent to [b1 => b2] (or
      [~b1 || b2]). *)

  val mk_conj : t -> t -> t
  (** [mk_conj b1 b2] returns a BDD equivalent to the conjunction
      [b1 && b2]. *)

  val mk_disj : t -> t -> t
  (** [mk_disj b1 b2] returns a BDD equivalent to the disjunction
      [b1 || b2]. *)

  val mk_iff : t -> t -> t
  (** [mk_iff b1 b2] returns a BDD equivalent to the equivalence
      [b1 <=> b2]. *)

  val mk_xor : t -> t -> t
  (** [mk_xor b1 b2] returns a BDD equivalent to the exclusive or [b1 # b2]
      (or [mk_ite b1 (mk_neg b2) b2]). *)

  val cofactor_pos : t -> var -> t
  (** [cofactor_pos b x] is a BDD equivalent to [b\[x:=1\]], that is, [x] is
      replaced with

      the trivially true constant [1]. *)

  val cofactor_neg : t -> var -> t
  (** [cofactor_neg b x] is a BDD equivalent to [b\[x:=0\]], that is, [x] is
      replaced with the trivially true constant [0]. *)

  val implies : t -> t -> bool
  (** [implies b1 b2] holds iff [b1 => b2] is valid. *)

  val contrad : t -> t -> bool
  (** [contrad b1 b2] holds iff [b1 && b2] is contradictory. *)

  val cond : t -> var
  (** For a conditional BDD [b] of the form [ITE(x,p,n)], [cond b] returns
      [x]. *)

  val pos : t -> t
  (** For a conditional BDD [b] of the form [ITE(x,p,n)], [pos b] returns
      [p]. *)

  val neg : t -> t
  (** For a conditional BDD [b] of the form [ITE(x,p,n)], [neg b] returns
      [n]. *)

  val is_valid : t -> bool
  (** [is_valid b] holds iff [equiv b mk_true]. *)

  val is_unsat : t -> bool
  (** [is_unsat b] holds iff [equiv b mk_false]. *)

  val is_ite : t -> bool
  (** [is_ite b] holds iff [b] is of the form [ITE(x, p, n)]. For a BDD [b],
      exactly one of [is_valid b], [is_unsat b], and [is_ite b] holds. *)

  val occurs : var -> t -> bool
  (** [occurs x b] holds iff the variable [x] occurs in the BDD [b]. *)

  val union : var -> var -> t -> t
  (** [union x y b] is equivalent to conjoining the equivalence [x <=> y] to
      [b]. *)

  val separate : var -> var -> t -> t
  (** [separate x y b] is equivalent to conjoining the constraint [x # y] to
      [b]. *)

  val implicant : t -> (bool * var) Queue.t -> unit
  (** For a satisfiable BDD [b]---that is, [mk_false b] does not
      hold---[implicant b] returns a pair
      [(\[x{1};...;x{n}\],\[y{1};...;y{m}\])] of variables such that
      [b => (x{1}&&...&&x{n}&&~y{1}&&...&&~y{m})]. *)

  val and_elim : t -> var list * var list * t
  (** [and_elim] extracts all implied literals. In particular, [and_elim b]
      returns [(\[x{1};...;x{n}\], \[y{1};...;y{m}\], b')] with [b => x{i}],
      [b => y{j}] for [i=1,...,n], [j=1,...,m], and [b'] is such that none
      of the [x{i}] or [y{j}] occurs in [b'] and
      [b <=> b' & x{1} & ... & x{n} & ~y{1} & ... ~y{m}]. *)
end

(** Construction of binary decision diagrams with variables of type [Var.t].
    The variable order used for building canonical BDDs is {i unspecified}. *)
module Make (Var : VAR) : FML with type var = Var.t

(**/**)

(** For debugging only. *)
module Test : sig
  val numofprobes : int ref
  val maxvar : int ref
  val maxpred : int ref
  val maxheap : int ref
  val init : unit -> unit
  val run : unit -> unit
end

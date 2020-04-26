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

module type PROPVAR = Type.ORDERED

module type PROP = sig
  type var
  type t

  val mk_true : t
  val mk_false : t
  val mk_posvar : var -> t
  val mk_negvar : var -> t
  val mk_conj : t -> t -> t
  val mk_disj : t -> t -> t
  val union : var -> var -> t -> t
  val separate : var -> var -> t -> t
  val cofactor_pos : t -> var -> t
  val cofactor_neg : t -> var -> t
  val is_valid : t -> bool
  val is_unsat : t -> bool
  val occurs : var -> t -> bool
  val and_elim : t -> var list * var list * t
end

module type INTERFACE = sig
  type var

  val valid : var -> unit
  val unsat : var -> unit
end

module type INFSYS = sig
  type var
  type t

  val empty : t
  val initialize : t -> unit
  val reset : unit -> unit
  val unchanged : unit -> bool
  val is_valid : unit -> bool
  val current : unit -> t

  exception Unsat

  val process_conjoin : t -> unit
  val process_union : var -> var -> unit
  val process_separate : var -> var -> unit
  val process_sub : var -> var -> unit
  val propagate_valid : var -> unit
  val propagate_unsat : var -> unit
end

module Make
    (Var : PROPVAR)
    (Prop : PROP with type var = Var.t)
    (I : INTERFACE with type var = Var.t) =
struct
  type var = Var.t
  type t = Prop.t

  let empty = Prop.mk_true
  let init = ref empty
  let curr = ref empty

  let initialize p =
    init := p ;
    curr := p

  let reset () = initialize empty
  let unchanged () = !init == !curr
  let is_valid () = Prop.is_valid !curr
  let current () = !curr

  exception Unsat

  let rec update p =
    curr := p ;
    close () ;
    if Prop.is_unsat !curr then raise Unsat

  and close () =
    let pl, nl, next = Prop.and_elim !curr in
    if pl <> [] || nl <> [] then (
      curr := next ;
      List.iter I.valid pl ;
      List.iter I.unsat nl )

  let process_conjoin p =
    if Prop.is_valid p then () else update (Prop.mk_conj p !curr)

  let process_union x y = update (Prop.union x y !curr)
  let process_separate x y = update (Prop.separate x y !curr)

  let process_sub x y =
    update
      (Prop.mk_conj
         (Prop.mk_disj (Prop.mk_negvar x) (Prop.mk_posvar y))
         !curr)

  let propagate_valid x =
    if Prop.occurs x !curr then update (Prop.cofactor_pos !curr x)

  let propagate_unsat x =
    if Prop.occurs x !curr then update (Prop.cofactor_neg !curr x)
end

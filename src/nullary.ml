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

(** Propositional formulas with equality.

    This module includes functionality for constructing
    {i binary decision diagrams} (BDDs).

    @author Harald Ruess *)

module type VAR = sig
  type t

  val equal : t -> t -> bool
  val compare : t -> t -> int
  val hash : t -> int
  val pp : Format.formatter -> t -> unit
end

module type INFSYS = sig
  type var
  type t

  val empty : t
  val initialize : t -> unit
  val reset : unit -> unit
  val unchanged : unit -> bool
  val current : unit -> t

  module Valid : Sets.S with type elt = var
  module Unsat : Sets.S with type elt = var

  val valid : unit -> Valid.t
  val unsat : unit -> Unsat.t
  val is_valid : var -> bool
  val is_unsat : var -> bool
  val pp : Format.formatter -> unit

  exception Unsat

  val process_valid : var -> unit
  val process_unsat : var -> unit
end

module Make (Var : VAR) = struct
  type var = Var.t

  module Valid = Sets.Make (Var)
  module Unsat = Sets.Make (Var)

  type t = {mutable valid: Valid.t; mutable unsat: Unsat.t}

  let empty = {valid= Valid.empty (); unsat= Unsat.empty ()}
  let init = ref empty

  module Config = struct
    module Valid = Config.Set (Valid)
    module Unsat = Config.Set (Unsat)
  end

  let valid = Config.Valid.current
  let unsat = Config.Unsat.current

  let initialize s =
    init := s ;
    Config.Valid.initialize s.valid ;
    Config.Unsat.initialize s.unsat

  let reset () = initialize empty
  let unchanged () = Config.Valid.unchanged () && Config.Unsat.unchanged ()

  let current () =
    if unchanged () then !init
    else {valid= Config.Valid.current (); unsat= Config.Unsat.current ()}

  let is_valid = Config.Valid.mem
  let is_unsat = Config.Unsat.mem

  let pp fmt =
    Format.fprintf fmt "@[" ;
    Format.fprintf fmt "valid: " ;
    Valid.pp fmt (valid ()) ;
    Format.fprintf fmt "unsat: " ;
    Unsat.pp fmt (unsat ()) ;
    Format.fprintf fmt "@]@?"

  exception Unsat

  let process_valid p =
    if is_valid p then ()
    else (
      Config.Valid.add p ;
      if is_unsat p then raise Unsat )

  let process_unsat p =
    if is_unsat p then ()
    else (
      Config.Unsat.add p ;
      if is_valid p then raise Unsat )
end

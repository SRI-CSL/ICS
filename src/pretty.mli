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

(** Pretty-printing methods for various datatypes.

  @author Harald Ruess
*)

module Mode : sig
  type t = Mixfix | Prefix | Sexpr
  val to_string : t -> string
  val of_string : string -> t
end

val flag : Mode.t ref

type 'a printer = Format.formatter -> 'a -> unit

val unit : unit printer

val string : string printer

val number : int printer

val option : 'a printer -> 'a option printer

val bool : bool printer

val three : Three.t printer

val list : 'a printer -> 'a list printer

val pair : 'a printer -> 'b printer -> ('a * 'b) printer

val triple : 'a printer -> 'b printer -> 'c printer -> ('a * 'b * 'c) printer

val infix : 'a printer -> string -> 'b printer -> ('a * 'b) printer
  (** [infix p str q (a, b)] prints [a] using printer [p], then it prints
    [str], and then [b] using printer [b]. *)

val mixfix : string -> 'a printer -> 
             string -> 'b printer -> 
             string -> 'c printer -> string
               -> ('a * 'b * 'c) printer

val post : 'a printer -> ('a * string) printer

val infixl : 'a printer -> string -> 'a list printer
  (** [infixl pp op] prints a list [[a1;...;an]] in the
    form [a1 op ... op an]. *)

val apply : 'b printer -> (string * 'b list) printer

val set : 'a printer -> 'a list printer
  (** Printing of a list as a set. *)

val map : 'a printer -> 'b printer -> ('a * 'b) list printer
  (** Printing of a list of pairs as a finite map. *)

val to_stdout : 'a printer -> ('a -> unit)
  (** [to_stdout pp] transforms a printer [pp] to print on [stdout]. *)

val to_stderr : 'a printer -> ('a -> unit)
 (** [to_stderr pp] transforms a printer to print on [stdout]. *)

val to_string : 'a printer -> ('a -> string)
 (** [to_string pp] transforms a printer to print on a string. *)
  


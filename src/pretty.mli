
(*i
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
 * 
 * Author: Harald Ruess
 i*)

(*s Module [Pretty]: Pretty-printing methods. *)

type 'a printer = Format.formatter -> 'a -> unit

(*s If [get_print_all] is [true], then the pretty-printer [term] for
 terms and all other printers which use it, print terms in prefix
 notation and all signature information of function symbols is printed.
 Otherwise, a more or less pretty-printed form with appropriate infix
 notation and suppression of printing signatures is output. *)

val get_print_all : unit -> bool
val set_print_all : bool -> unit

val sym : Sym.t printer
val arity : Arity.t printer 
val cnstrnt : Type.t printer
val term : Term.t printer
val eqn : (Term.t * Term.t) printer
val diseq : (Term.t * Term.t) printer
val inn : (Term.t * Number.t) printer
val tset : Term.set printer
val map: 'a printer -> 'a Term.map printer
val tmap : Term.t Term.map printer
val tlist : Term.t list printer
val list : 'a printer -> 'a list printer
val atom : Atom.t printer
val atoms : Atom.Set.t printer
val prop : Prop.t printer

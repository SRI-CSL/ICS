
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

val string : string printer

val number : int printer

val list : 'a printer -> 'a list printer

val pair : 'a printer -> 'b printer -> ('a * 'b) printer

val infix : 'a printer -> string -> 'b printer -> ('a * 'b) printer

val eqn : 'a printer -> ('a * 'a) printer

val solution : 'a printer -> ('a * 'a) list printer

val infixl : 'a printer -> string -> 'a list printer

val set : 'a printer -> 'a list printer

val map : 'a printer -> 'b printer -> ('a * 'b) list printer

(*s Redirecting pretty-printer output. *)

val to_stdout : 'a printer -> ('a -> unit)
val to_stderr : 'a printer -> ('a -> unit)

val to_string : 'a printer -> ('a -> string)

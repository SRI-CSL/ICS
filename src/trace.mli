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

(** Rudimententary tracing capability. All trace messages are
  directed to {!Format.stderr}. The amount of tracing output
  is determined by the the set of active {b trace levels}. 

  @author Harald Ruess
*)

type level = string

val registered : (level * string) list ref

val reset : unit -> unit
  (** Reset the set of trace levels to the empty set, that is,
    all tracing is disabled. *)


val add : level -> unit
  (** [add l] activates the trace level [l]. *)


val remove : level -> unit
  (** [remove l] deactivates the trace level [l]. *)


val get : unit -> level list
  (** [get ()] returns the set of currently active trace levels
    as a list. *)

val call : level -> string -> 'a -> 'a Pretty.printer -> unit
  (** [call l name arg pp] outputs a function call trace message
    if [l] is currently active by first outputting [name] followed
    by printing [arg] using the [pp] printer. *)

val exit : level -> string -> 'a -> 'a Pretty.printer -> unit
 (** [exit l name arg pp] outputs a function exit trace message
    if [l] is currently active by first outputting [name] followed
    by printing [arg] using the [pp] printer. *)

val msg : level -> string -> 'a -> 'a Pretty.printer -> unit
  (** [msg l name arg pp] outputs a trace message
    if [l] is currently active by first outputting [name] followed
    by printing [arg] using the [pp] printer. *)

val func : level -> string -> 
             'a Pretty.printer -> 'b Pretty.printer -> 
                ('a -> 'b) -> 'a -> 'b
  (** [func l name pp qq f] provides a trace wrapper for function
    [f] for calling [call] before applying [f] and [exit] after
    applying [f]. In addition, messages are output if [f] raises
    an exception.  Except for the outputting of trace messages on
    [stderr], the functional behavior of [f] is unchanged. *)

val proc : level -> string -> 'a Pretty.printer
             -> ('a -> unit) -> 'a -> unit
  (** [proc l name pp f] is just [func l name pp Pretty.unit f]. *)

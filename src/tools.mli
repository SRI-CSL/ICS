
(*
 * ICS - Integrated Canonizer and Solver
 * Copyright (C) 2001-2004 SRI International
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the ICS license as published at www.icansolve.com
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * ICS License for more details.
 *)

(*s The purpose of the [Tools] module is to provide some functions and types,
  which are used througout the code of ICS. *)

    (*s Procedures [f] without parameters can be registered as exit procedures
      by [add_to_exit f]. The registered exit procedures are then called by
     [do_at_exit()]. Exit procedures are usually used for displaying some
     statistics. *)

val add_at_exit : (unit -> unit) -> unit
val do_at_exit : unit -> unit

    (*s [add_to_reset f] registers [f] as a reset procedure, which are then
      called, one-by-one, by [do_at_reset()]. *)

val add_at_reset : (unit -> unit) -> unit
val do_at_reset : unit -> unit

    (*s Various ICS function output, on standard output, a trace of their input
      and output behavior depending on the current [verbose] value; generally speaking,
      the highter this value, the more information is output. The verbose
      value is set with [set_verbose], and can be obtained with [get_verbose].
      [verbose n f x] calls a procedure [f] on argument [x] only if the current
      verbose level is greater or equal to [n]. *)

val set_verbose : int -> unit
val get_verbose : unit -> int
val verbose : int -> ('a -> unit) -> 'a -> unit

    (*s [utime f a] returns not only the result of applying [f] to [a]
      but also the time required to compute the function. *)
val utime : ('a -> 'b) -> 'a -> 'b * float
    
    (*s [profile str f] profiles function [f], and
      registers an exit function which outputs the number of calls
      of this function, and the total time spent in this function;
      the argument [str] is usually just the name of the function.*)
val profile : string -> ('a -> 'b) -> ('a -> 'b)

    (*s Given a pretty printing function [f] for type ['a],
      [pp_to_string f a] redirects the output of [f(a)] to a string. *)
val pp_to_string : (Format.formatter -> 'a -> unit) -> 'a -> string

    (*s Type for comparison. More readable than the convential [-1], [0], and [1]
      results of comparisons. *)
type cmp = Less | Equal | Greater


(*i
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
 i*)

(*s Module [Cmd]: Commands for ICS command interpreter for building up
  a logical data base, displaying information about the content and the
  structure of this data base, and normalizing terms. *)

   (*s Normal form using theory-specific normalizations only. In particular,
       no context information is used. *)

val sigma : Ics.term -> unit

   (*s Solves equation on terms. *)
    
val solve : Ics.term option -> Ics.term * Ics.term -> unit

   (*s Asserts argument term to the current context.
     There are three different outcomes. In case, <term> is found to be implied
     by the current context, `Valid.' is returned, and if <term> is found to be
     inconsistent, then 'Inconsistent.' is printed. Otherwise, the is added to
     the current context. *)
    
val process : Ics.term -> unit
    
    (*s Reset to empty context, flashed various hash tables and reinitializes counters
      used by ICS. *)
    
val reset : unit -> unit
    
    (*s Drop into Caml if run on bytecode (back with Main.repl ();;) *)
    
val drop : unit -> unit

    (*s Check term ordering. *)
    
val less : Ics.term * Ics.term -> unit

    (*s Sets the verbose level. The higher the argument value, the more information is displayed. *)
    
val verbose : int -> unit

    (*s Print current state. *)

val curr : unit -> unit

    (*s Canonical representative of the equivalence class of argument term as stored in the context.
        If argument term is omitted, the complete find structure is displayed *)
    
val find : Ics.term option -> unit

    (*s Displays the canonical representatives in which
      <term> occurs interpreted. If <term> is omitted, the complete use structure
      is displayed. *)
    
val use : Ics.term option -> unit

   (*s Prints equivalence class of <term> without <term> itself.
     If <term> is omitted, extensions of all canonical forms are displayed. *)
    
val ext : Ics.term option -> unit

    (*s Display the logical context; not necessarily in canonized form. *)
    
val ctxt : unit -> unit

     (*s Compute type interpretation of <term> wrt. current context.
       If <term> is omitted, then the constraints for all canonical representatives
       are displayed. *)
    
val cnstrnt: Ics.term option -> unit
val uninterp : Ics.term option -> unit

    (*s Computes canonical representative of argument using context information. *)
    
val can : Ics.term -> unit

    (*s Computes a normal form of argument term. [simp] is like [can], but without recursive
      processing in conditionals. *)
    
val simp : Ics.term -> unit

    (*s Simplifies all interpreted terms, and replaces uninterpreted terms with
      their finds. *)
val norm : Ics.term -> unit

    (*s Check if argument term holds or is unsatisfiable in current context.
        Prints 'Valid.' if argument is redundant, 'Inconsistent.' if it is inconsistent,
        otherwise silent. In contrast to the assert command, the context is not updated *)
    
val check : Ics.term -> unit

    
val help : unit -> unit
    
    (*s Lists all commands. *)
val help_syntax : unit -> unit

    (*s Outlines term syntax *)
val help_commands : unit -> unit


(*i*)
open Ics
(*i*)

val sigma : term -> unit
val solve : term option -> term * term -> unit
val process : term -> unit
val reset : unit -> unit
val drop : unit -> unit
val less : term * term -> unit
val verbose : int -> unit
val find : term option -> unit
val use : term option -> unit
val ext : term option -> unit
val ctxt : unit -> unit
val cnstrnt: term option -> unit
val uninterp : term option -> unit
val can : term -> unit
val simp : term -> unit
val norm : term -> unit
val check : term -> unit
val help : unit -> unit
val help_syntax : unit -> unit
val help_commands : unit -> unit


(*i*)
open Mpa
open Term
(*i*)

val lt : term * term -> term
val le : term * term -> term

(*s {\bf Inequalities.} The type [t] implements a set of
    inequalities.  The functions [add_lt], [add_le], etc. insert a new
    inequality. This insertion may produce an equality [t = 0] (when
    both [t <= 0] and [t >= 0] hold) and in this case the first
    component of the result is [Some t]; otherwise it is [None].  The
    second component is the new set of inequalities (whenever [t = 0]
    is produced, the inequalities involving [t] are discarded.)  The
    insertion functions raise the exception [Inconsistent] whenever an
    inconsistency is detected (when both [t > 0] and [t <= 0] hold for
    instance). Type constraints are used to turn [t < 0] into
    [t+1 <= 0] whenever [t] is an integer expression. *)



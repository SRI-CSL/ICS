
(*s [update a i u] is a constructor for updating function [a] at position [i] with
  value [u]. It employs the simplification\\
  \begin{tabular}{lcl}
  [update (update b j v) j u] & = & update b i u \\
  \end{tabular} *)
 
val update : Term.t * Term.t * Term.t -> Term.t

(*s Sigmatizing. *)

val sigma : Term.builtin -> Term.t list -> Term.t

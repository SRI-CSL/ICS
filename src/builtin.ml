
(*i*)
open Hashcons
open Term
(*i*)

    (*s [update a i u] is a constructor for updating function [a] at position [i] with
      value [u]. It employs the simplification\\
        \begin{tabular}{lcl}
        [update (update b j v) j u] & = & update b i u \\
        \end{tabular} *)

let update =
  let rec upd (a,i,u) =
    match a.node with
      | App({node=Builtin(Update)},[b;j;v])
	  when i === j  ->
	    upd (b,i,u)
      | _ ->
	  mk_update (a,i,u)
  in
  Bool.ternary_lift_ite upd



(*s Sigmatizing. *)

let sigma op l =
  match op, l with
    | Update, [x;y;z] -> update(x,y,z)
    | _ -> assert false


(*i*)
open Hashcons
open Mpa
open Term
(*i*)

let deriv_eq i j = eq_term i j

let deriv_diseq i j =
  eq_term (Equal.diseq i j) (ptrue ())

(*s Smart Constructors:
    \begin{displaymath}\begin{array}{rcl}
      \arrlk{\arrup{a}{i}{x}}{i} & = & x \\
      \arrlk{\arrup{a}{i}{x}}{j} & = & \arrlk{a}{j} \mbox{ if } i \not= j \\
      \arrup{\arrup{a}{i}{x}}{i}{y} & = & \arrup{a}{i}{y}
    \end{array}\end{displaymath}
 *)

let rec app a = function
  | [] -> a
  | l -> (match a.node with
	    | Update(b,j,v) ->
		let i = Tuple.tuple l in
		if deriv_eq i j then
		  v
		else if deriv_diseq i j then
		  app b l
		else
		  Bool.ite (Equal.equal i j) v (Term.app b l)
	    | App(b,m) ->
		app b (m @ l)
	    | _ ->
		Term.app a l) 

let rec update a i u =
  match a.node with
    | Update(b,j,v) when deriv_eq i j  -> update b i u
    | _ -> Term.update a i u

(*s Arrays solver. *)

let solve ((a,b) as e) =
  let return a b =
    if eq_term a b then [] else [(a,b)]
  in
  match a.node,b.node with
    | Update (u,i,s), Update (v,j,t)
	when eq_term u v && eq_term i j ->
          return s t
    | _, App _ -> 
	return b a
    | _ -> 
	assert false






(*i*)
open Hashcons
open Mpa
open Term
(*i*)

let deriv_diseq i j =
  Bool.diseq i j === Bool.tt()

(*s Smart Constructors:
    \begin{displaymath}\begin{array}{rcl}
      \arrlk{\arrup{a}{i}{x}}{i} & = & x \\
      \arrlk{\arrup{a}{i}{x}}{j} & = & \arrlk{a}{j} \mbox{ if } i \not= j \\
      \arrup{\arrup{a}{i}{x}}{i}{y} & = & \arrup{a}{i}{y}
    \end{array}\end{displaymath}
 *)


let finite s x =
  if Term.Set.mem x s then
    Bool.tt()
  else if Term.Set.for_all (deriv_diseq x) s  then
    Bool.ff()
  else
    hc(App(Sets.finite s,[x]))

let rec app a l =
  if l = [] then a
  else match a.node with
    | App(b,m) ->                  (* Uncurrying *)
	app b (m @ l)
    | Bool(Ite(x,y,z)) ->          (* Lift if *)
	Bool.ite (app a [x]) (app a [y]) (app a [z])
    | Update(b,j,v) ->
	let i = Tuple.tuple l in
	if i === j then
	  v
	else if deriv_diseq i j then
	  app b l
	else
	  Bool.ite (Bool.equal i j) v (app b l)
    | Set s ->
	(match s with
	   | Empty _ ->
	       Bool.ff()
	   | Full _ ->
	       Bool.tt()
	   | Cnstrnt(c) ->
	       Cnstrnt.app c (Tuple.tuple l)
	   | Finite(s) ->
	       finite s (Tuple.tuple l)
	   | SetIte(_,s1,s2,s3) ->
	       Bool.ite (app a [s1]) (app a [s2]) (app a [s3]))
    | _ ->
	hc(App(a,l))

let rec update a i u =
  match a.node with
    | Update(b,j,v) when i === j  ->
	update b i u
    | _ ->
	hc(Update(a,i,u))
	  
 
(*s Constructor for constraint applications *)

	  (*
let int a = Cnstrnt.app Cnstrnt.int a
    
let real a = Cnstrnt.app Cnstrnt.real a
	       
let le (x,y) = 
  if x === y then
    Bool.tt ()
  else
    let (p,q) = Arith.d_poly(Arith.sub(x,y)) in
    Cnstrnt.app (Cnstrnt.le Interval.Real (Q.minus q)) p

let lt (x,y) =
  let (p,q) = Arith.d_poly(Arith.sub (x,y)) in
  Cnstrnt.app (Cnstrnt.lt Interval.Real (Q.minus q)) p
*)



(*i*)
open Term
open Hashcons
open Morphisms
(*i*)

let rec occurs s t =
  s == t ||
  match t.node with
    | Update(t1,t2,t3) -> occurs s t1 || occurs s t2 || occurs s t3
    | Atom(Equal (t1,t2)) -> occurs s t1 || occurs s t2
    | Arith _ -> Arith.occurs s t
    | Tuple _ -> Tuple.occurs s t
    | Set _ -> Sets.occurs s t
    | Bool _  -> Bool.occurs s t
    | Bv _  -> Bv.occurs s t
    | _ -> false

let subst x t =
  List.map (fun (a1,a2) -> (replace a1 x t, replace a2 x t))

let is_type_inconsistent x t = false

let add x t rho =
  if x == t then
    rho
  else if is_type_inconsistent x t then
    raise (Exc.Inconsistent "Type inconsistency")
  else
    (x,t) :: (subst x t rho)

let solve x e =
  let rec solvel rho = function
    | [] -> rho
    | (a,b) :: el ->
	if a == b then
	  solvel rho el
	else if is_pure a && is_pure b then
	  raise (Exc.Inconsistent "Identical pure terms")
	else if is_uninterpreted a && not(occurs a b) then
	  solvel (add a b rho) (subst a b el)
	else
	  match solve_interp (a,b) @ rho with
	    | [a',b'] when a == a' && b == b' ->
		solvel (add a b rho) el
	    | sigma ->
		solvel rho (sigma @ el)  

  and solve_interp (a,b) =
    match a.node, b.node with
      | Atom _, _ -> Atom.solve (a,b)
      | _, Atom _ -> Atom.solve (b,a)
      | Arith _, _ -> Arith.solve x (a,b)
      | _, Arith _ -> Arith.solve x (b,a)    
      | Tuple _, _ -> Tuple.solve (a,b)
      | _, Tuple _ -> Tuple.solve (b,a)
      | Set _, _ -> Sets.solve 0 (a,b)
      | _, Set _ -> Sets.solve 0 (b,a)
      | Bool _, _ -> Bool.solve (a,b)
      | _, Bool _ -> Bool.solve (b,a)
      | Bv _, _ ->  Bv.solve (a,b)
      | _, Bv _ -> Bv.solve (b,a)
      | _ -> assert false
 
  in
  solvel [] [e]





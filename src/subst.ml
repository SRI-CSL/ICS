
(*i*)
open Term
open Hashcons
(*i*)


type t = Term.t Term.Map.t
 
let empty = Term.Map.empty

let add x t rho =
  if x === t then rho else Term.Map.add x t rho

let mem s x =
  Term.Map.mem x s

let apply s a =
  Term.Map.find a s
    
let find s a =
  try Term.Map.find a s with Not_found -> a

let of_list l =
  List.fold_right (fun (x,a) -> add x a) l empty

let to_list l =
  Map.fold (fun x a acc -> (x,a) :: acc) l [] 

let iter f rho =
  Map.iter (fun x y -> f (x,y)) rho

let fold f rho a =
  Map.fold (fun x y acc -> f (x,y) acc) rho a
    
let dom s =
  Map.fold (fun x _ acc -> Term.Set.add x acc) s Term.Set.empty
    
let pp fmt rho =
  let pp_binding (x,a) =
    Format.fprintf fmt "@[";
    Pretty.term fmt x;
    Format.fprintf fmt " |-> ";
    Pretty.term fmt a;
    Format.fprintf fmt "@]";
  in
  let rec pp_bindings = function
    | [] -> ()
    | [b] -> pp_binding b
    | b :: l -> pp_binding b; Format.fprintf fmt ",@ "; pp_bindings l
  in
  Format.fprintf fmt "@[[";
  pp_bindings (to_list rho);
  Format.fprintf fmt "]@]"
 

(*s {\bf Norm.} This operation, written \replace{S}{a}, applies [S]
    under the interpreted symbols of [a]. It is defined by
    \begin{displaymath}\begin{array}{rcl}
    \replace{S}{f(a_1,\dots,a_n)} & = & 
        f(\replace{S}{a_1},\dots,\replace{S}{a_n}), 
        \mbox{ if $f$ is interpreted} \\
    \replace{S}{a} & = & S(a), \mbox{ otherwise}
    \end{array}\end{displaymath}
    We use the [map] function over terms to apply [replace] recursively under
    interpreted terms.
*)

let norm s a =
  let eq_lists l1 l2 =
    try List.for_all2 (fun x y -> x === y) l1 l2 with Invalid_argument _ -> false
  in
  let rec repl t =
    (match t.node with
      | Var _ | App _ ->                   (* "completely uninterpreted" *)
	  find s t
      | Update(x,y,z) ->
	  let x' = repl x and y' = repl y and z' = repl z in
	  if x' === x && y' === y && z' === z then t
	  else App.update x' y' z'
      | Set s ->
	  (match s with
	     | Full _ | Empty _ -> t
	     | Cnstrnt _ -> t
	     | Finite s ->
		 let s' = Ptset.fold (fun x acc -> Ptset.add (repl x) acc) s Ptset.empty in
		 if Ptset.equal s s' then t else Sets.finite s'
	     | SetIte(tg,x,y,z) ->
		  let x' = repl x and y' = repl y and z' = repl z in
		  if x' === x && y' === y && z' === z then t else Sets.ite tg x' y' z')
      | Bool b ->
	  (match b with
	     | True | False -> t
	     | Equal(x,y) ->
		  let x' = repl x and y' = repl y in
		  if x' === x && y' === y then t else Bool.equal x' y'
	     | Ite(x,y,z) ->
		 let x' = repl x and y' = repl y and z' = repl z in
		 if x' === x && y' === y && z' === z then t else Bool.ite x' y' z'
	     | _ -> assert false)
      | Arith a ->
	  (match a with
	     | Num _ -> t
	     | Add l ->
		 let l' = List.map repl l in
		 if eq_lists l l' then t else Arith.add l'
	     | Multq(q,x) ->
		 let x' = repl x in
		 if x === x' then t else Arith.multq q x'
	     | Mult l ->
		 let l' = List.map repl l in
		 if eq_lists l l' then t else Arith.mult l'
	     | Div(x,y) ->
		 let x' = repl x and y' = repl y in
		 if x' === x && y' === y then t else Arith.div2(x',y'))
      | Bv b ->
	  (match b with
	     | Const _ -> t
	     | Extr((n,x),i,j) ->
		 let x' = repl x in
		 if x === x' then t else Bv.sub n i j x'
	     | BvToNat x ->
		 let x' = repl x in
		 if x === x' then t else Bv.bv2nat 42 x'
	     | Conc l ->
		 Bv.conc (List.map (fun (n,t) -> (n, repl t)) l)
	     | BvIte((n,x),(_,y),(_,z)) ->
		 let x' = repl x and y' = repl y and z' = repl z in
		 if x' === x && y' === y && z' === z then t else Bv.ite n x' y' z')
      | Tuple tp ->
	  (match tp with
	     | Proj(i,j,x) ->
		 let x' = repl x in
		 if x' === x then t else Tuple.proj i j x'
	     | Tup l ->
		 let l' = List.map repl l in
		 if eq_lists l l' then t else  Tuple.tuple l'))
  in
  repl a









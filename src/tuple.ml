
(*i*)
open Hashcons
open Term
(*i*)

let rec occurs s t =
  s == t ||
  match t.node with
    | Tuple t -> (match t with
		    | Proj (_,_,t) -> s == t || occurs s t
		    | Tup tl -> List.exists (occurs s) tl)
    | _ -> false

(*s Smart constructors *)

let tuple = function
  | [x] -> x
  | [] -> assert false
  | l' -> hc (Tuple (Tup l'))

let proj i n s =
  match s.node with
    | Tuple t -> (match t with
		    | Tup l -> List.nth l i
		    | Proj(j,m,a) -> hc (Tuple(Proj(i + j,n + m,a))))
    | _ -> hc (Tuple (Proj (i,n,s)))


(*s Solving tuples. *) 

let add ((a,b) as e) el =
  if a == b then el
  else match b.node with
    | Tuple(Tup l) when List.exists (fun y -> a == y) l ->
	raise (Exc.Inconsistent "Tuple solver")
    | _ -> e :: el

(*s [solve (s, (t0,...,tn)) = [(proj s 0, t0),...,(proj s n, tn)]] *)

let tuple_solve s l =
  let n = List.length l in
  let (eqs, _) = 
    List.fold_right
      (fun t (acc, i) -> (add (proj i n s, t) acc, i + 1)) l ([], 0)
  in
  eqs

(*s [solve ((s0,...,sn), (t0,...,tn)) = [(s0,t0),...(sn,tn)] *)  

let tuple_tuple_solve al bl = 
   List.fold_right2 (fun a b acc -> add (a, b) acc) al bl []

(*s [solve (proj i n s, t) = (s, [c0,...,t,...cn-1])]
     where [ci] are fresh, [s] at [i]th position. *)

let proj_solve i n s t =
  let rec args j acc =
    if j = -1 then acc
    else
      let a = if i = j then t else Var.fresh "p" [] in
      args (j - 1) (a :: acc)
  in
  add (s, tuple (args (n - 1) [])) []

let solve ((a,b) as e) =
  match a.node,b.node with
    | Tuple(Tup al), Tuple(Tup bl) -> tuple_tuple_solve al bl
    | Tuple(Tup al), _ -> tuple_solve b al
    | Tuple(Proj (i,n,a)), _ -> proj_solve i n a b
    | _ -> assert false












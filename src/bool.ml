
(*i*)
open Term
open Hashcons
(*i*)

let rec occurs s t =
  eq_term s t ||
  match t.node with
    | Bool(Ite(b1,b2,b3)) ->
	occurs s b1 || occurs s b2 || occurs s b3
    | _ -> false

(* Building up BDDs *)

module BDD = Bdd.Make(
  struct
    type bdd_node = term_node
    type bdd = term
    type tag = unit
    let compare = compare
    let high _ = ptrue ()
    let low _ = pfalse ()
    let ite _ = Term.ite
    let is_high p =
      match p.node with
	| Bool True -> true
	| _ -> false
    let is_low p =
      match p.node with
	| Bool False -> true
	| _ -> false
    let is_ite p =
      match p.node with
	| Bool (Ite _) -> true
	| _ -> false
    let destructure_ite p =
      match p.node with
	| Bool (Ite (a,b,c)) -> Some(a, b, c)
	| _ -> None
    let fresh _ = (fresh "a" [] All)
		    
  end)

let neg p = BDD.neg () p
let conj p q = BDD.conj () p q
let disj p q = BDD.disj () p q
let xor p q  = BDD.xor () p q
let imp p q = BDD.imp () p q
let iff p q = BDD.iff () p q

let ite a b c = BDD.build () (a,b,c)

(*s Solving propositional equations and disequalities (xor) *)

let solve_eqn (s1,s2) = BDD.solve () (iff s1 s2)
let solve_deq (s1,s2) = BDD.solve () (xor s1 s2)

let solve e = solve_eqn e


(*s Boolean completion:
   ite(x,p,n) => (~n => p)  (<=> n | p)
   since ite(x,p,n) <=> (~n=>x & x => p)
 *)

let complete b =
  let rec loop acc =
    let acc' = Tset.fold (fun b ->
			    match b.node with
			      | Bool(Ite(x,p,n)) ->
				  Tset.add (disj p n)
			      | _ -> assert false)
		 acc acc
    in
    if Tset.sub acc' acc then acc else loop acc'
  in
  loop (Tset.singleton b)
		

	






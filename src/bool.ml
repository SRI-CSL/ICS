
(*i*)
open Term
open Hashcons
(*i*)

let rec occurs s t =
  s == t ||
  match t.node with
    | Bool(Ite(b1,b2,b3)) ->
	occurs s b1 || occurs s b2 || occurs s b3
    | _ -> false

(* Constants *)

let tt = hc (Bool True)
let ff = hc (Bool False)

let is_tt t =
  match t.node with Bool(True) -> true | _ -> false

let is_ff t =
  match t.node with Bool(False) -> true | _ -> false
     

(* Building up BDDs *)

module BDD = Bdd.Make(
  struct
    type bdd_node = term_node
    type bdd = term
    type tag = unit
    let compare = fast_cmp
    let high _ = tt
    let low _ = ff
    let ite _ a b c =
      hc (Bool(Ite(a,b,c)))
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
    let fresh _ = (Var.fresh ("z",None,None) [])
		    
  end)
       
let neg p = BDD.neg () p
let conj p q = BDD.conj () p q
let disj p q = BDD.disj () p q
let xor p q  = BDD.xor () p q
let imp p q = BDD.imp () p q
let iff p q = BDD.iff () p q

let ite a b c = BDD.build () (a,b,c)

let forall xl p = hc (Bool(Forall (xl, p)))
let exists xl p = hc (Bool(Exists (xl, p)))

(*s Solving propositional equations and disequalities (xor) *)

let solve_eqn (s1,s2) = BDD.solve () (iff s1 s2)
let solve_deq (s1,s2) = BDD.solve () (xor s1 s2)

(* Solve by repeatedly applying the equivalences.

         (x = y) = T <-> x = y
              ~x = T <-> x = F
           x & y = T <-> x = T, y = T
           x | y = F <-> x = F, y = F
 *)

let rec solve ((a,b) as e) =
  match a.node, b.node with
    | Bool(Ite(x,{node=Bool(False)},{node=Bool(True)})), Bool(True) ->
	solve (x, ff)
    | Bool(Ite(x,y,{node=Bool(False)})), Bool(True) -> 
	solve (x, tt) @ solve (y, tt)
    | Bool(Ite(x,{node=Bool(True)},y)), Bool(False) -> 
	solve (x, ff) @ solve (y, ff)
    | _ ->
	[e]
		

(* infer new boolean equalities:

      ite(x,p,n) = T --> p | n = T
 *) 

let rec infer ((a,b) as e) =
  match a.node, b.node with
     | Bool(Ite(_,y,z)), Bool(True) ->
	 e :: infer (disj y z , tt)
     | _ ->
	 [e]


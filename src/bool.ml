
(*i*)
open Term
open Hashcons
(*i*)


(* Constants *)

let tt () = hc(Bool True)
	      
let ff () = hc(Bool False)

let finite s x =
  if Term.Set.mem x s then
    tt()
  else if Term.Set.for_all (fun y -> not(x === y)) s then
    ff()
  else
    hc(App(Sets.finite s,[x]))

      
(* Intersection of two atoms *)
    
let inter_equal_equal (x1,y1) (x2,y2) =
  if x1 === x2 then
    (match y1.node, y2.node with
       | Arith(Num q1), Arith(Num q2) when not(Mpa.Q.equal q1 q2) -> Some(ff())
       | _ -> None)
  else if y1 === y2 then
    (match x1.node, x2.node with
       | Arith(Num q1), Arith(Num q2) when not(Mpa.Q.equal q1 q2) -> Some(ff())
       | _ -> None)
  else
    None

let inter_equal_app (x1,y1) (c2,x2) =
  if x1 === x2 then
    if Cnstrnt.mem y1 c2 then Some(hc(Bool(Equal(x1,y1)))) else None
  else if y1 === x2 then
    if Cnstrnt.mem x1 c2 then Some(hc(Bool(Equal(x1,y1)))) else None
  else
    None

let inter_diseq_equal (x1,y1) (x2,y2) =
  if x1 === x2  then
    if y1 === y2 then
      Some(ff())
    else if is_const y1 && is_const y2 then
      Some(hc(Bool(Equal(x2,y2))))
    else
      None
  else if y1 === y2 then
    if x1 === x2 then
      Some(ff())
    else if is_const x1 && is_const x2 then
      Some(hc(Bool(Equal(x2,y2))))
    else
      None
  else
    None

let inter a b =                   (* intersection of [a] and [c] *)
  if a === b then
    Some(a)
  else
    match a.node, b.node with
      | Bool(True), _ -> Some(b)
      | _, Bool(True) -> Some(a)
      | Bool(False), _ -> Some(ff())
      | _, Bool(False) -> Some(ff())   
      | App({node=Set(Cnstrnt(c1))},[x1]), App({node=Set(Cnstrnt(c2))},[x2]) when x1 === x2 ->
	  Some(Cnstrnt.app (Cnstrnt.inter c1 c2) x1)
      | Bool(Equal(x1,y1)), Bool(Equal(x2,y2)) ->
	  inter_equal_equal (x1,y1) (x2,y2)
      | Bool(Ite({node=Bool(Equal(x1,y1))},{node=Bool False},{node=Bool True})),  Bool(Equal(x2,y2)) ->
	  inter_diseq_equal (x1,y1) (x2,y2)
      | Bool(Equal(x1,y1)), Bool(Ite({node=Bool(Equal(x2,y2))},{node=Bool False},{node=Bool True})) ->
	  inter_diseq_equal (x2,y2) (x1,y1)    
      | Bool(Equal(x1,y1)), App({node=Set(Cnstrnt(c2))},[x2]) ->
	  inter_equal_app (x1,y1) (c2,x2)
      | App({node=Set(Cnstrnt(c1))},[x1]), Bool(Equal(x2,y2)) ->
	  inter_equal_app (x2,y2) (c1,x1)
      | _ ->
	  None   

let compl a =
  match a.node with
    | Bool(True) ->
	Some(ff())
    | Bool(False) ->
	Some(tt())
    | Bool(Equal(x,y)) ->
	Some(hc(Bool(Ite(hc(Bool(Equal(x,y))),ff(),tt()))))
    | Bool(Ite(x,{node=Bool False}, {node=Bool True})) ->
	Some(x)
    | App({node=Set(Cnstrnt(c))},[x]) ->
	Some(Cnstrnt.app (Cnstrnt.compl c) x)
    | _ ->
	None

let compl_inter a c =                 (* intersection of [not(a)] and [c] *)
  match compl a with
    | None -> None
    | Some(a') -> inter a' c

let ite a b c =
  match inter a b, compl_inter a c with
    | Some(x), Some(y) ->
	(match x.node, y.node with
	   | Bool True, _ -> tt()
	   | _, Bool True -> tt()
	   | Bool False, _ -> y
	   | _, Bool False -> x
	   | _ -> hc(Bool(Ite(x,tt(),y))))
    | _ -> 
	hc(Bool(Ite(a,b,c)))

(*s Some recognizers. *)

let is_tt t =
  match t.node with Bool(True) -> true | _ -> false

let is_ff t =
  match t.node with Bool(False) -> true | _ -> false
    
let is_ite t = 
  match t.node with 
    | Bool Ite _ -> true 
    | _ -> false

(* Building up BDDs *)

module BDD = Bdd.Make(
  struct
    type bdd_node = Term.tnode
    type bdd = Term.t
    type tag = unit
    let compare = fast_cmp
    let high _ = tt ()
    let low _ = ff ()
    let ite _ a b c = ite a b c
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
	| Bool(Ite(a,b,c)) -> Some(a,b,c)
	| _ -> None
    let fresh _ = (Var.fresh "z" [])    
  end)

let ite p q r = BDD.build () (p,q,r)  
let neg = BDD.neg ()
let conj = BDD.conj ()
let disj = BDD.disj ()
let xor = BDD.xor ()
let imp = BDD.imp ()
let iff = BDD.iff ()

let rec conjl = function
  | [] -> tt()
  | [x] -> x
  | x :: l -> conj x (conjl l)

let rec disjl = function
  | [] -> ff()
  | [x] -> x
  | x :: l -> conj x (disjl l)

let is_neg  = BDD.is_neg
let is_conj = BDD.is_conj
let is_disj = BDD.is_disj
let is_xor  = BDD.is_xor
let is_imp = BDD.is_imp
let is_iff = BDD.is_iff

let d_neg = BDD.d_neg 
let d_conj = BDD.d_conj 
let d_disj = BDD.d_disj
let d_xor = BDD.d_xor
let d_imp = BDD.d_imp
let d_iff = BDD.d_iff
let d_ite a =
  match a.node with
    | Bool(Ite(x,y,z)) -> (x,y,z)
    | _ -> assert false
	      
(* let ite a b c = BDD.build () (a,b,c) *)

let forall xl p = hc (Bool(Forall (xl, p)))
let exists xl p = hc (Bool(Exists (xl, p)))

(*s Solving propositional equations and disequalities (xor) *)

let solve_eqn (s1,s2) = BDD.solve () (iff s1 s2)
let solve_deq (s1,s2) = BDD.solve () (xor s1 s2)

let solve (a,b) = BDD.solve () (iff a b)
		      
(* infer new boolean equalities:

      ite(x,p,n) = T --> p | n = T
 *) 

let rec infer ((a,b) as e) =
  match a.node, b.node with
     | Bool(Ite(_,y,z)), Bool(True) ->
	 e :: infer (disj y z , tt ())
     | _ ->
	 [e]

	 
(*s Equalities *)

let rec equal a b =
  if a === b then
    tt()
  else if is_const a && is_const b then
    ff()
  else
    match a.node, b.node with
      | Bool(Ite _), _ | _, Bool(Ite _) ->
	  iff a b
      | _ ->  hc(Bool(Equal(a,b)))

let is_equal a =
  match a.node with
    | Bool(Equal _ ) -> true
    | _ -> false

let d_equal a =
  match a.node with
    | Bool(Equal(a,b)) -> (a,b)
    | _ -> raise (Invalid_argument "Bool.d_diseq: Not an equality.")

	  
(*s Disequalities [a <> b] are encoded as [~(a = b)]. *)

let diseq a b =
  neg(equal a b)
 
let is_diseq a =
  match a.node with
    | Bool(Ite({node=Bool(Equal _)}, {node=Bool False}, {node=Bool True})) -> true
    | _ -> false

let d_diseq a =
  match a.node with
    | Bool (Ite({node=Bool(Equal(x,y))},
		{node=Bool False},
		{node=Bool True})) -> (x,y)
    | _ -> raise (Invalid_argument "Bool.d_diseq: Not a disequality.")


(*i*)
open Hashcons
open Term
(*i*)

  

(*s Simplification of the disjunction of two atoms. *)

let rec disj a b =
  match a.node, b.node with
    | Bool(True), _ ->
	Some(tt())
    | _, Bool(True) ->
	Some(ff())
    | Bool(False), _ ->
	Some(b)
    | _, Bool(False) ->
	Some(a)
    | Bool(x), Bool(y) ->
	disj_bool_bool x y
    | Bool(Equal(x1,y1)),
      App({node=Set(Cnstrnt(c2))},[x2]) ->
	disj_equal_app (x1,y1) (c2,x2)
    | App({node=Set(Cnstrnt(c1))},[x1]),
      Bool(Equal(x2,y2)) ->
	disj_equal_app (x2,y2) (c1,x1)
    | App({node=Set(Cnstrnt(c1))},[x1]),
      App({node=Set(Cnstrnt(c2))},[x2]) when x1 === x2 ->
	Some(Cnstrnt.app (Interval.union c1 c2) x1)
    | _ ->
	None

and disj_bool_bool a b =
  match a, b with
    | Equal(x1,y1), Equal(x2,y2) ->
	if x1 === x2 && y1 === y2 then
	  Some(hc(Bool(a)))
	else
	  None
    | Ite({node=Bool(Equal(x1,y1))},{node=Bool False},{node=Bool True}),
      Equal(x2,y2) ->
	disj_diseq_equal (x1,y1) (x2,y2)
    | Equal(x1,y1),
      Ite({node=Bool(Equal(x2,y2))},{node=Bool False},{node=Bool True}) ->
	disj_diseq_equal (x2,y2) (x1,y1)
    | _ ->
	None

and disj_diseq_equal (x1,y1) (x2,y2) =
  if x1 === x2 && y1 === y2 then        (*s [x <> y disj x = y] equivalent to [tt] *)
    Some(tt())
  else if x1 === y2 && y1 === x2 then   (*s [x <> y disj y = x] equivalent to [tt] *)
    Some(tt())
  else
    None

and disj_equal_app (x1,y1) (c2,x2) =
  if x1 === x2  then
    Some(Cnstrnt.app c2 y1)
  else
    None

      
(*s Simplification of the conjunction of two atoms. *)
    
let conj_equal_equal (x1,y1) (x2,y2) =
  if x1 === x2 then
    (match y1.node, y2.node with
       | Arith(Num q1), Arith(Num q2) when not(Mpa.Q.equal q1 q2) -> Some(hc(Bool(False)))
       | _ -> None)
  else if y1 === y2 then
    (match x1.node, x2.node with
       | Arith(Num q1), Arith(Num q2) when not(Mpa.Q.equal q1 q2) -> Some(hc(Bool(False)))
       | _ -> None)
  else
    None

let conj_equal_app (x1,y1) (c2,x2) =
  if x1 === x2 then
    if (* is_tt(Term.mem y1 c2) *) false then Some(hc(Bool(Equal(x1,y1)))) else None
  else if y1 === x2 then
    if (* is_tt(Term.mem x1 c2) *) false then Some(hc(Bool(Equal(x1,y1)))) else None
  else
    None

let conj_diseq_equal (x1,y1) (x2,y2) =
  if (x1 === x2 && y1 === y2) || (x1 === y2 && x2 === y1) then
    Some(hc(Bool(False)))
  else if x1 === x2 && is_const y1 && is_const y2 then   (* thus: [y1 =/= y2] *)
    Some(hc(Bool(Equal(x2,y2))))
  else if y1 === y2 && is_const x1 && is_const x2 then   (* thus: [x1 =/= x2] *)
    Some(hc(Bool(Equal(x1,y1))))
  else
    None

let conj_bool_bool a b =
  match a, b with
    | Equal(x1,y1),
      Equal(x2,y2) ->
	conj_equal_equal (x1,y1) (x2,y2)
    | Ite({node=Bool(Equal(x1,y1))},{node=Bool False},{node=Bool True}),
      Equal(x2,y2) ->
	conj_diseq_equal (x1,y1) (x2,y2)
    | Equal(x1,y1),
      Ite({node=Bool(Equal(x2,y2))},{node=Bool False},{node=Bool True}) ->
	conj_diseq_equal (x2,y2) (x1,y1)
    | _ ->
	None
	

let conj a b =                   (* conjunction of [a] and [b] *)
  if a === b then
    Some(a)
  else
    match a.node, b.node with
      | Bool(True), _ ->
	  Some(b)
      | _, Bool(True) ->
	  Some(a)
      | Bool(False), _ ->
	  Some(hc(Bool(False)))
      | _, Bool(False) ->
	  Some(hc(Bool(False)))
      | Bool(x), Bool(y) ->
	  conj_bool_bool x y
      | App({node=Set(Cnstrnt(c1))},[x1]),
	App({node=Set(Cnstrnt(c2))},[x2]) when x1 === x2 ->
	  Some(Cnstrnt.app (Interval.inter c1 c2) x1)
      | Bool(Equal(x1,y1)),
	App({node=Set(Cnstrnt(c2))},[x2]) ->
	  conj_equal_app (x1,y1) (c2,x2)
      | App({node=Set(Cnstrnt(c1))},[x1]),
	Bool(Equal(x2,y2)) ->
	  conj_equal_app (x2,y2) (c1,x1)
      | _ ->
	  None


let rec neg a =
  match a.node with
    | Bool(x) ->
	bool_neg x
    | App({node=Set(Cnstrnt(c))},[x]) ->
	Some(Cnstrnt.app (Interval.compl c) x)
    | _ ->
	None

and bool_neg a =
  match a with
    | True ->
	Some(ff())
    | False ->
	Some(tt())
    | Equal(x,y) ->
	Some(hc(Bool(Ite(hc(Bool(Equal(x,y))),ff(),tt()))))
    | Ite(x,{node=Bool False}, {node=Bool True}) ->
	Some(x)
    | _ ->
	None
      

let neg_conj a c =                 (* intersection of [not(a)] and [c] *)
  match neg a with
    | None -> None
    | Some(a') -> conj a' c







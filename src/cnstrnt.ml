
(*i*)
open Term
open Hashcons
open Mpa
open Binrel
(*i*)

type t = cnstrnt

(*s Constructors. *)

let top = Top
let bot = Bot
let boolean = BooleanCnstrnt
let tuple = TupleCnstrnt

let arith c = 
  if Interval.is_bot c then 
    Bot 
  else if Interval.is_top c then
    Top
  else 
    ArithCnstrnt(c)

let oo d q p = arith(Interval.oo d q p)
let oc d q p = arith(Interval.oc d q p)
let co d q p = arith(Interval.co d q p)
let cc d q p = arith(Interval.cc d q p)

let lt d q = arith(Interval.lt d q)
let le d q = arith(Interval.le d q)
let gt d q = arith(Interval.gt d q)
let ge d q = arith(Interval.ge d q)

let int = arith(Interval.int)
let nonint = arith(Interval.nonint)
let real = arith(Interval.real)

let neg = arith(Interval.lt Interval.Real Q.zero)
let nonpos = arith(Interval.le Interval.Real Q.zero)

let singleton q = arith(Interval.singleton q)

let diseq q = arith(Interval.diseq q)

(*s Recognizers. *)

let is_bot c = (ceq c bot)
let is_top c = (ceq c top)
let is_tuple c = (ceq c tuple)
let is_boolean c = (ceq c boolean)

let is_arith c =
  match c with
    | ArithCnstrnt _ -> true
    | _ -> false

let d_arith c =
  assert(is_arith c);
  match c with
    | ArithCnstrnt(x) -> x
    | _ -> assert false

let is_singleton c =
  match c with
    | ArithCnstrnt(x) -> Interval.is_singleton(x) 
    | _ -> false

let d_singleton c =
  assert(is_singleton c);
  match c with
    | ArithCnstrnt(x) -> Interval.value_of x
    | _ -> assert false

let is_nonzero c =
  match c with
    | ArithCnstrnt(x) ->
	not(Interval.mem Q.zero x)
    | _ ->
	false

(*s Comparison of intervals. *)

let cmp c d =
  match c, d with
    | Top, Top -> Same
    | Top, _ -> Super
    | _, Top -> Sub
    | Bot, Bot -> Same
    | Bot, _ -> Sub
    | _, Bot -> Super
    | ArithCnstrnt(x), ArithCnstrnt(y) -> Interval.cmp x y
    | BooleanCnstrnt, BooleanCnstrnt -> Same
    | TupleCnstrnt, TupleCnstrnt -> Same
    | _ -> Disjoint

let sub c d =
  match cmp c d with
    | Same | Sub -> true
    | _ -> false


(*s Intersection of two constraints. *)

let inter c d =
  match c, d with
    | Top, _ -> d
    | _, Top -> c
    | ArithCnstrnt(x), ArithCnstrnt(y) ->
	arith(Interval.inter x y)
    | BooleanCnstrnt, BooleanCnstrnt -> 
	boolean
    | TupleCnstrnt, TupleCnstrnt ->
	tuple
    | _ ->
	Bot

(*s Complement of a constraint. May underapproximate the real negated domain. *)

let compl c =
  assert(is_arith c);
  match c with
    | ArithCnstrnt(x) -> 
	arith(Interval.compl x)
    | _ ->
	failwith "Cnstrnt.compl: fatal error"


(*s Union of two constraints. May overapproximate the domain. *)

let union c d =
  match c, d with
    | Top, _ -> Top
    | _, Top -> Top
    | Bot, _ -> d
    | _, Bot -> c
    | ArithCnstrnt(x), ArithCnstrnt(y) ->
	arith(Interval.union x y)
    | BooleanCnstrnt, BooleanCnstrnt ->
	boolean
    | TupleCnstrnt, TupleCnstrnt ->
	tuple
    | _ -> Top                        (* e.g. [union bool tuple] is just [top]. *)


(*s Constraint corresponding to a domain [d]. *)

let of_domain = function
  | IntDom -> int
  | RatDom -> real
  | BoolDom -> boolean


(*s Constraint corresponding to a term. *)

let of_interp a =
  match a.node with
    | Var(_,Ext(Some(d))) -> of_domain d
    | Var(_,Fresh(d)) -> of_domain d
    | _ -> top

 
(*s Test for membership in a constraint. *)


type status = Yes | No | X

let rec mem a c =
  match c with
    | Bot -> No
    | Top -> Yes
    | _ ->
	(match a.node with
	   | Var(_,k) ->
	       mem_var k c
	   | App({node=Interp(op)}, _) ->
	       (match op with
		  | Bool _ -> 
		      if ceq c boolean then Yes else No
	          | Tuple(Product) -> 
		      if ceq c tuple then Yes else No
	          | Arith(Num(q)) ->
		      mem_arith q c
		  | _ -> X)
           | _ -> X)

and mem_var k c =
  match k with
    | Ext(Some(d)) ->
	if sub (of_domain d) c then Yes else X
    | Fresh(d) ->
	if sub (of_domain d) c then Yes else X
    | _ -> 
	X

and mem_arith q c =                          (* notice: [c] may be overapproximating. *)
  match c with
    | ArithCnstrnt(x) ->
	if Interval.mem q x then Yes else X
    | _ -> 
	No

(*s Analyze *)

type analyze =
  | Empty
  | Full
  | Singleton of Q.t
  | Other

let analyze c =
  if is_bot c then
    Empty
  else if is_top c then
    Full
  else if is_singleton c then
    Singleton(d_singleton c)
  else 
    Other


(*s Abstract interpretation of arithmetic operators. *)

let mult c d =
  match c, d with
    | Bot, _ -> Bot
    | _, Bot -> Bot
    | ArithCnstrnt(x), ArithCnstrnt(y) ->
	arith(Interval.mult x y)
    | _ -> Top

let multq q c =
  match c with
    | Bot -> Bot
    | ArithCnstrnt(x) ->
	arith(Interval.multq q x)
    | _ -> Top

let add c d =
  match c, d with
    | Bot, _ -> Bot
    | _, Bot -> Bot
    | ArithCnstrnt(x), ArithCnstrnt(y) ->
	arith(Interval.add x y)
    | _ -> Top


(*s Computing the best constraint, given a context of constraint declarations. *)

let of_term ctxt a =
  let rec cnstrnt_of_term a =
    let c1 = match a.node with
      | Var(_, Ext(Some(dom))) -> 
	  cnstrnt_of_var_of_dom dom
      | Var(_, Fresh(dom)) -> 
	  cnstrnt_of_var_of_dom dom
      | App({node=Interp(Arith(op))},l) ->
	  cnstrnt_of_arith op l
      | App({node=Interp(Bool _)},_) ->
	  boolean
      | App({node=Interp(Tuple(Product))},_) ->
	  tuple
      | _ -> 
	  top
    and c2 = ctxt a in
    inter c1 c2

  and cnstrnt_of_arith op l =
    match op,l with
      | Num(q), [] -> 
	  cnstrnt_of_num q
      | Multq(q),[x] ->
	  cnstrnt_of_multq q x
      | Mult, l ->
	  cnstrnt_of_mult l
      | Add, l ->
	  cnstrnt_of_add l
      | Div, [x;y] ->  
	  cnstrnt_of_div x y
      | _ ->
	  assert false
    
  and cnstrnt_of_var_of_dom dom =
    match dom with
      | IntDom -> int
      | RatDom -> real
      | BoolDom -> boolean

  and cnstrnt_of_num q =
    singleton q

  and cnstrnt_of_multq q x =
    mult (singleton q) (cnstrnt_of_term x)

  and cnstrnt_of_mult l =
    match l with
      | [] -> 
	  singleton Q.one
      | [x] -> 
	  cnstrnt_of_term x
      | x :: y :: l when x === y ->
	  mult (cnstrnt_of_square x) (cnstrnt_of_mult l)
      | x :: l -> 
	  mult (cnstrnt_of_term x) (cnstrnt_of_mult l)

  and cnstrnt_of_square x =
    let c0 = cnstrnt_of_term x in
    let c1 = mult c0 c0 in
    let c2 = ge Interval.Real Q.zero in
    inter c1 c2

  and cnstrnt_of_add l =
    match l with
      | [] -> singleton Q.zero
      | [x] -> cnstrnt_of_term x
      | x :: l -> add (cnstrnt_of_term x) (cnstrnt_of_add l)

  and cnstrnt_of_div a b =
    real
  in
  cnstrnt_of_term a

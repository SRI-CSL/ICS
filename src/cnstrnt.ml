(*i*)
open Mpa
open Term
open Hashcons
(*i*)

type t = Term.cnstrnt
	   
type domain = Interval.domain

		
              (*s Constructors. *)
	
let make c = hc(Set(Cnstrnt(c)))
	
  
let empty = Bot
let full  = Top
		   
let zero = Sub(Nonreals.empty, Interval.singleton Q.zero)
let one = Sub(Nonreals.empty, Interval.singleton Q.one)
let singleton q = Sub(Nonreals.empty,Interval.singleton q)
		    
let oo dom p q =
  let i = Interval.oo dom p q in
  if Interval.is_empty i then empty else Sub(Nonreals.empty,i)
    
let oc dom p q =
  let i = Interval.oc dom p q in
  if Interval.is_empty i then empty else Sub(Nonreals.empty,i)

let co dom p q =
  let i = Interval.co dom p q in
  if Interval.is_empty i then empty else Sub(Nonreals.empty,i)
    
let cc dom p q =
  let i = Interval.cc dom p q in
  if Interval.is_empty i then empty else Sub(Nonreals.empty,i)
		   
let int = Sub(Nonreals.empty ,Interval.int)
let real = Sub(Nonreals.empty, Interval.real)
let nonintreal = Sub(Nonreals.empty, Interval.nonint)

let nonreals =
  Nonreals.add Boolean
    (Nonreals.add Predicate
       (Nonreals.add Cartesian
	  (Nonreals.add Bitvector
	     (Nonreals.singleton Other))))
		   
let nonreal = Sub(nonreals,Interval.empty)
let boolean = Sub(Nonreals.singleton Boolean, Interval.empty)
let predicate = Sub(Nonreals.singleton Predicate, Interval.empty)
let cartesian = Sub(Nonreals.singleton Cartesian, Interval.empty)
let bitvector = Sub(Nonreals.singleton Bitvector, Interval.empty)

let is_predicate = function
  | Sub(s,_) -> Nonreals.mem Predicate s
  | _ -> false
		     
let diseq q = Sub(Nonreals.empty,Interval.diseq q)    

let lt dom q = Sub(Nonreals.empty,Interval.lt dom q)
let le dom q = Sub(Nonreals.empty,Interval.le dom q)
let gt dom q = Sub(Nonreals.empty,Interval.gt dom q)
let ge dom q = Sub(Nonreals.empty,Interval.ge dom q)

		 
		 (*s Recognizers. *)
	
let is_empty = function
  | Bot -> true
  | _ -> false
	
let is_full = function
  | Top -> true
  | _ -> false
	
let is_real = function
  | Sub(s,_) when Nonreals.is_empty s -> true
  | _ -> false
	
let is_int = function
  | Sub(s,i) when Nonreals.is_empty s && Interval.is_int i -> true
  | _ -> false
	
let is_singleton = function
  | Sub(s,i) when Nonreals.is_empty s && Interval.is_singleton i -> true
  | _ -> false
  
	(*s Value of a singleton. *)

let value_of = function
  | Sub(_,i) -> Interval.value_of i
  | _ -> assert false
	
let rec mem x = function
  | Top -> true
  | Bot -> false
  | Sub(s,i) ->
      mem_of_nonreal x s ||  mem_of_real x i

and mem_of_real x i =
  match x.node with
    | Arith(Num q) -> Interval.mem q i
    | _ -> false

and mem_of_nonreal x s =
  match x.node with
    | Bool _ -> Nonreals.mem Boolean s
    | Tuple _ -> Nonreals.mem Cartesian s
    | Set _ -> Nonreals.mem Predicate s  
    | Bv(BvToNat _) -> false
    | Bv _ -> Nonreals.mem Bitvector s
    | _ -> false
	  
	  (*s Intersection, union, and complement of constraints. *)

let inter c1 c2 =
  match c1,c2 with
    | Top, _ -> c2
    | _, Top -> c1
    | Bot, _ -> Bot
    | _, Bot -> Bot
    | Sub(s1,i1), Sub(s2,i2) ->
	let s = Nonreals.inter s1 s2 in
	let i = Interval.inter i1 i2 in
	if Nonreals.is_empty s && Interval.is_empty i then
	  Bot
	else
	  Sub(s,i)
	
let union c1 c2 =
  match c1,c2 with
    | Top, _ -> Top
    | _, Top -> Top
    | Bot, _ -> c2
    | _, Bot -> c1
    | Sub(s1,i1), Sub(s2,i2) ->
	let s = Nonreals.union s1 s2 in
	let i = Interval.union i1 i2 in
	if Interval.is_full i && Nonreals.equal s nonreals then
	  Top
	else
	  Sub(s,i)

let compl = function
  | Bot -> Top
  | Top -> Bot
  | Sub(s,i) ->
      let s' = Nonreals.diff nonreals s in
      let i' = Interval.compl i in
      Sub(s',i')

let ite c1 c2 c3 =
  union (inter c1 c2) (inter (compl c1) c3)

    
    (*s Comparison of constraints. *)

let rec cmp c1 c2 =
  match c1,c2 with
    | Top, Top ->
	Binrel.Same
    | Bot, Bot ->
	Binrel.Same
    | Bot, _ ->
	Binrel.Disjoint
    | _, Bot ->
	Binrel.Disjoint
    | Sub(s1,i1), Sub(s2,i2) when Nonreals.equal s1 s2 ->
	Interval.cmp i1 i2
    | Sub(s1,i1), Sub(s2,i2) ->
	Binrel.union (nonreals_cmp s1 s2) (Interval.cmp i1 i2)         (* ??? *)
    | Sub _, Top ->
	Binrel.Sub
    | Top, Sub _ ->
	Binrel.Super

and nonreals_cmp s1 s2 =
  if Nonreals.equal s1 s2 then
    Binrel.Same
  else 
    let s = Nonreals.inter s1 s2 in
    if Nonreals.is_empty s then
      Binrel.Disjoint
    else if Nonreals.equal s s1 then
      Binrel.Sub
    else if Nonreals.equal s s2 then
      Binrel.Super
    else
      Binrel.Overlap
    
	(*s Abstract interpretation of addition and multiplication. *)

let mult c1 c2 =
  match c1, c2 with
    | Top, _ -> Top
    | _, Top -> Top
    | Sub(s1,i1), Sub(s2,i2) ->
	let i = Interval.mult i1 i2 in
	if Interval.is_empty i then Bot
	else Sub(Nonreals.empty, i)
    | _ -> Bot
    
let add c1 c2 =
  match c1, c2 with
    | Top, _ -> Top
    | _, Top -> Top
    | Sub(s1,i1), Sub(s2,i2) ->
	let i = Interval.add i1 i2 in
	if Interval.is_empty i then Bot
	else Sub(Nonreals.empty, i)
    | _ -> Bot

    (*s Computing the best constraint, given a context of constraint declarations. *)

let cnstrnt ctxt a =
  let rec cnstrnt_of_term a =
    try
      ctxt a
    with
	Not_found ->
	  (match a.node with
	     | Arith x ->
		 (match x with
		   | Num q -> cnstrnt_of_num q
		   | Multq(q,x) -> mult (cnstrnt_of_num q) (cnstrnt_of_term x)
		   | Mult l -> cnstrnt_of_mult l
		   | Add l -> cnstrnt_of_add l
		   | Div(x,y) -> cnstrnt_of_div x y)
	     | App(f,_) when is_predicate(cnstrnt_of_term f) ->
		 boolean
	     | Bool _ ->
		 boolean
	     | Set _  ->
	         predicate
	     | Tuple _ ->
	         cartesian
	     | Bv(BvToNat _) ->
		 int
	     | Bv _  ->
		 bitvector
	     | _ ->
		 full)

  and cnstrnt_of_num q =
    singleton q

  and cnstrnt_of_mult l =
    match l with
      | [] -> singleton Q.one
      | [x] -> cnstrnt_of_term x
      | x :: l -> mult (cnstrnt_of_term x) (cnstrnt_of_mult l)

  and cnstrnt_of_add l =
    match l with
      | [] -> singleton Q.zero
      | [x] -> cnstrnt_of_term x
      | x :: l -> add (cnstrnt_of_term x) (cnstrnt_of_add l)

  and cnstrnt_of_div a b =
    real
  in
  cnstrnt_of_term a

 
    (*s Applying a constraint to a term. *)

let app c a =
  if is_empty c then hc(Bool False)
  else if is_full c then hc(Bool True)
  else if mem a c then hc(Bool True)
  else if is_ground a then hc(Bool False)
  else hc(App(hc(Set(Cnstrnt(c))),[a]))

 















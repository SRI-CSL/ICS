
(*i*)
open Format
open Hashcons
open Term
(*i*)

(*s Terms. *)
  
type term = Term.t

(*s Variables. *)

let mk_var x = Var.var x
	 
let is_var t =
  match t.node with
    | Var _ -> true
    | _ -> false

let d_var t =
  match t.node with
    | Var s -> s
    | _ -> raise (Invalid_argument "Ics.d_var: Variable required")
		 
let mk_fresh s =
  Var.fresh s None

let is_fresh =
  Var.is_fresh

let fresh_equiv a =
  assert(is_fresh a);
  Var.equiv a
	
let _ = Callback.register "mk_var" mk_var
let _ = Callback.register "is_var" is_var
let _ = Callback.register "d_var" d_var  

let _ = Callback.register "mk_fresh" mk_fresh
let _ = Callback.register "is_fresh" is_var
let _ = Callback.register "fresh_equiv" mk_fresh

	  
(*s Function application and function update. *)
         
let mk_app = App.app
let mk_update a b c = App.update (a,b,c)
   
let is_app t = match t.node with App _ -> true | _ -> false
let is_update t = match t.node with Update _ -> true | _ -> false

let _ = Callback.register "mk_app" mk_app
let _ = Callback.register "mk_update" mk_update

let d_app t =
  match t.node with
    | App(f,l) -> f,l
    | _ -> raise (Invalid_argument "Ics.d_app: Application required")

let d_update t =
  match t.node with
    | Update(a,x,i) -> a,x,i
    | _ -> raise (Invalid_argument "Ics.d_update: Update required")
	  
let _ = Callback.register "is_app" is_app
let _ = Callback.register "is_update" is_update  

	  
(*s Constructing arithmetic expressions. *)

let mk_num = Arith.num
let mk_div t1 t2 = Arith.div2 (t1,t2)
let mk_plus tl = Arith.add tl
let mk_plus2 x y = Arith.add2 (x,y)
let mk_minus t1 t2 = Arith.sub (t1,t2)
let mk_unary_minus  = Arith.neg
let mk_times tl = Arith.mult tl
let mk_times2 t1 t2 = Arith.mult2 (t1,t2)      

let _ = Callback.register "mk_num" mk_num
let _ = Callback.register "mk_plus" mk_plus
let _ = Callback.register "mk_plus2" mk_plus2
let _ = Callback.register "mk_minus" mk_minus
let _ = Callback.register "mk_unary_minus" mk_unary_minus
let _ = Callback.register "mk_times" mk_times
let _ = Callback.register "mk_times2" mk_times2

let is_num t = match t.node with Arith(Num _) -> true | _ -> false
let is_multq t = match t.node with Arith(Multq _) -> true | _ -> false  
let is_mult t = match t.node with Arith(Mult _) -> true | _ -> false
let is_add t = match t.node with Arith(Add _) -> true | _ -> false

let d_num t =
  match t.node with
    | Arith Num q -> q
    | _ -> raise (Invalid_argument "Ics.d_num: Num required")

let d_add t =
  match t.node with
    | Arith Add l -> l
    | _ -> raise (Invalid_argument "Ics.d_plus: Plus required")

let d_mult t =
  match t.node with
    | Arith Mult l -> l
    | _ -> raise (Invalid_argument "Ics.d_times: Times required")

let d_multq t =
  match t.node with
    | Arith Multq(q,x) -> (q,x)
    | _ -> raise (Invalid_argument "Ics.d_times: Linear multiplication required")
  
  
	  
(*s Constructing tuples and projections. *)

let mk_tuple = Tuple.tuple
let mk_proj = Tuple.proj

let _ = Callback.register "mk_tuple" mk_tuple
let _ = Callback.register "mk_proj" mk_proj

	  
(*s Constructing Boolean terms. *)

let mk_true () = Bool.tt ()
let mk_false () = Bool.ff ()
let mk_ite a b c = Bool.ite(a,b,c)
let mk_not = Bool.neg
let mk_and = Bool.conj
let mk_or = Bool.disj
let mk_xor = Bool.xor
let mk_imp = Bool.imp
let mk_iff = Bool.iff
   
let _ = Callback.register "mk_true" mk_true
let _ = Callback.register "mk_false" mk_false
let _ = Callback.register "mk_ite" mk_ite
let _ = Callback.register "mk_not" mk_not
let _ = Callback.register "mk_and" mk_and
let _ = Callback.register "mk_or" mk_or
let _ = Callback.register "mk_xor" mk_xor
let _ = Callback.register "mk_imp" mk_imp
let _ = Callback.register "mk_iff" mk_iff

let d_ite = Bool.d_ite
let d_not = Bool.d_neg
let d_and = Bool.d_conj
let d_or = Bool.d_disj
let d_xor = Bool.d_xor
let d_imp = Bool.d_imp
let d_iff = Bool.d_iff


let mk_equal a b = Bool.equal(a,b)
let mk_diseq = Bool.diseq

let mk_in a b = App.app b [a]
let mk_notin a b = mk_not(App.app b [a])

let mk_int a = Arith.int a
let mk_real a = Arith.real a
let mk_lt a b = Arith.lt (a,b)
let mk_le a b = Arith.le (a,b)
let mk_gt a b = Arith.lt (b,a)
let mk_ge a b = Arith.le (b,a)

let _ = Callback.register "mk_equal" mk_equal  
let _ = Callback.register "mk_diseq" mk_diseq
let _ = Callback.register "mk_in" mk_in
let _ = Callback.register "mk_notin" mk_notin
let _ = Callback.register "mk_int" mk_int
let _ = Callback.register "mk_real" mk_real
let _ = Callback.register "mk_lt" mk_lt
let _ = Callback.register "mk_le" mk_le
let _ = Callback.register "mk_gt" mk_gt
let _ = Callback.register "mk_ge" mk_ge 
	  

(*s Set of terms. *)

type terms = Term.terms

let terms_empty () = Term.Set.empty
let terms_add = Term.Set.add
let terms_pp = Pretty.tset Format.std_formatter
let terms_mem = Term.Set.mem
let terms_sub = Term.Set.sub
let terms_is_empty = Term.Set.is_empty
let terms_to_list = Term.Set.to_list
let terms_choose = Term.Set.destructure
			      
let _ = Callback.register "terms_empty" terms_empty
let _ = Callback.register "terms_add" terms_add
let _ = Callback.register "terms_pp" terms_pp
let _ = Callback.register "terms_mem" terms_mem
let _ = Callback.register "terms_sub" terms_sub
let _ = Callback.register "terms_is_empty" terms_is_empty
let _ = Callback.register "terms_to_list" terms_to_list
let _ = Callback.register "terms_choose" terms_choose

(*s Maps with terms as domain. *)

type 'a map = 'a Term.Map.t

let map_empty () = Term.Map.empty
let map_add = Term.Map.add
let map_is_empty = Term.Map.is_empty
let map_find = Term.Map.find
let map_remove = Term.Map.remove
let map_mem = Term.Map.mem
let map_to_list = Term.Map.to_list
let map_pp p = Pretty.tmap (fun fmt -> p) Format.std_formatter

let _ = Callback.register "map_empty" map_empty
let _ = Callback.register "map_add" map_add
let _ = Callback.register "map_is_empty" map_is_empty
let _ = Callback.register "map_find" map_find
let _ = Callback.register "map_remove" map_remove
let _ = Callback.register "map_mem" map_mem
let _ = Callback.register "map_to_list" map_to_list
let _ = Callback.register "map_pp" map_pp
	  
		    
(*s Substititon. *)
    
type subst = Subst.t

let subst_empty () = Subst.empty
let subst_add x t s = Subst.empty
let subst_mem = Subst.mem
let subst_find = Subst.find
let subst_apply = Subst.apply
let subst_of_list l = Subst.empty
let subst_to_list s = []
let subst_pp = Subst.pp Format.std_formatter
let subst_norm = Subst.norm
	    
let _ = Callback.register "subst_empty" subst_empty
let _ = Callback.register "subst_add" subst_add
let _ = Callback.register "subst_mem" subst_mem
let _ = Callback.register "subst_find" subst_find
let _ = Callback.register "subst_apply" subst_apply
let _ = Callback.register "subst_of_list" subst_of_list
let _ = Callback.register "subst_to_list" subst_to_list
let _ = Callback.register "subst_pp" subst_pp
let _ = Callback.register "subst_norm" subst_norm


(*s Constraints *)
		     
type cnstrnt = Interval.t
		     
let cnstrnt_lt d q = Interval.lt d q
let cnstrnt_le d q = Interval.le d q
let cnstrnt_ge d q = Interval.ge d q
let cnstrnt_gt d q = Interval.gt d q

let cnstrnt_domain d =
  match d with
    | Interval.Real -> Interval.real
    | Interval.Int -> Interval.int
    | Interval.NonintReal -> Interval.nonint
	  
let cnstrnt_int = Interval.int
let cnstrnt_real = Interval.real
   

let cnstrnt_openopen d p q = Interval.oo d p q
let cnstrnt_openclosed d p q = Interval.oc d p q
let cnstrnt_closedopen d p q = Interval.co d p q
let cnstrnt_closedclosed d p q = Interval.cc d p q

let cnstrnt_app = Cnstrnt.app

let cnstrnt_pp c =
  Pretty.cnstrnt Format.std_formatter c;
  Format.pp_print_flush Format.std_formatter ()

let _ = Callback.register "cnstrnt_lt" cnstrnt_lt
let _ = Callback.register "cnstrnt_le" cnstrnt_le
let _ = Callback.register "cnstrnt_gt" cnstrnt_gt
let _ = Callback.register "cnstrnt_ge" cnstrnt_ge
let _ = Callback.register "cnstrnt_openopen" cnstrnt_openopen
let _ = Callback.register "cnstrnt_closedopen" cnstrnt_closedopen
let _ = Callback.register "cnstrnt_openclosed" cnstrnt_openclosed
let _ = Callback.register "cnstrnt_closedclosed" cnstrnt_closedclosed

let _ = Callback.register "cnstrnt_app" cnstrnt_app

	 (*s [low_bound] is the type of lower bounds in intervals and they are
      either negative infinity, or a rational number together with a strictness attribute.
      The recognizers [low_bound_is_strict]
      and [low_bound_is_nonstrict] are only defined when [low_bound_is_neginf] does
      not hold. In these cases, one can access the lower bound using [low_bound_value] . *)
  
type low_bound = Interval.low

let low_bound_neginf () = Interval.Neginf
let low_bound_strict q = Interval.Low(Interval.Strict,q)
let low_bound_nonstrict q = Interval.Low(Interval.Nonstrict,q)	    

let low_bound_is_neginf = function
  | Interval.Neginf -> true
  | _ -> false
	
let low_bound_is_strict = function
  | Interval.Low(Interval.Strict,_) -> true
  | _ -> false
	
let low_bound_is_nonstrict = function
  | Interval.Low(Interval.Nonstrict,_) -> true
  | _ -> false
		   
let low_bound_value = function
  | Interval.Low(_,q) -> q
  | _ -> assert false

let _ = Callback.register "low_bound_neginf" low_bound_neginf
let _ = Callback.register "low_bound_strict" low_bound_strict
let _ = Callback.register "low_bound_nonstrict" low_bound_nonstrict 
let _ = Callback.register "low_bound_is_neginf" low_bound_is_neginf
let _ = Callback.register "low_bound_is_strict" low_bound_is_strict
let _ = Callback.register "low_bound_is_nonstrict" low_bound_is_nonstrict
let _ = Callback.register "low_bound_value" low_bound_value
  
	  

    (*s [high_bound] is the type of upper bounds in interlets and they are
      either positive infinity, or a rational number together with a strictness attribute.
      The recognizers [high_bound_is_strict]
      and [high_bound_is_nonstrict] are only defined when [high_bound_is_posinf] does
      not hold. In these cases, one can access the lower bound using [high_bound_letue] . *)
  

type high_bound = Interval.high
		    
let high_bound_posinf () = Interval.Posinf
let high_bound_strict q = Interval.High(Interval.Strict,q)
let high_bound_nonstrict q = Interval.High(Interval.Nonstrict,q)	    


let high_bound_is_posinf = function
  | Interval.Posinf -> true
  | _ -> false
	
let high_bound_is_strict = function
  | Interval.High(Interval.Strict,_) -> true
  | _ -> false
	
let high_bound_is_nonstrict = function
  | Interval.High(Interval.Nonstrict,_) -> true
  | _ -> false
		   
let high_bound_value = function
  | Interval.High(_,q) -> q
  | _ -> assert false

let _ = Callback.register "high_bound_posinf" high_bound_posinf
let _ = Callback.register "high_bound_strict" high_bound_strict
let _ = Callback.register "high_bound_nonstrict" high_bound_nonstrict 	
let _ = Callback.register "high_bound_is_posinf" high_bound_is_posinf
let _ = Callback.register "high_bound_is_strict" high_bound_is_strict
let _ = Callback.register "high_bound_is_nonstrict" high_bound_is_nonstrict
let _ = Callback.register "high_bound_value" high_bound_value
  

    (*s Intervals are interpreted either over the reals, the integers, or the reals without
      the integers. *)
   
type interval_domain = Interval.domain

let interval_domain_real () = Interval.Real
let interval_domain_int () = Interval.Int
let interval_domain_nonintreal () = Interval.NonintReal
					

let interval_domain_is_real d = (d = Interval.Int)
let interval_domain_is_int d = (d = Interval.Real)
let interval_domain_is_nonintreal d = (d = Interval.NonintReal)

					
let _ = Callback.register "interval_domain_real" interval_domain_real
let _ = Callback.register "interval_domain_int" interval_domain_int
let _ = Callback.register "interval_domain_nonintreal" interval_domain_nonintreal
let _ = Callback.register "interval_domain_is_real" interval_domain_is_real
let _ = Callback.register "interval_domain_is_int" interval_domain_is_int
let _ = Callback.register "interval_domain_is_nonintreal" interval_domain_is_nonintreal

  
	  
      (*s Listify constraints as the disjunction of singleton constraints. *)

type interval = Interval.interval
	  
let cnstrnt_to_list = Interval.to_list
let cnstrnt_of_list = Interval.of_list

let _ = Callback.register "cnstrnt_to_list" cnstrnt_to_list
let _ = Callback.register "cnstrnt_of_list" cnstrnt_of_list

(*s Sets. *)

let mk_empty = Sets.empty
let mk_full = Sets.full
let mk_setite a b c = Sets.ite 0 (a,b,c)
let mk_compl = Sets.compl
let mk_inter = Sets.inter
let mk_union = Sets.union
let mk_diff = Sets.diff
let mk_sym_diff = Sets.sym_diff (* symmetric difference *)
let mk_sub = Sets.sub
let mk_seteq = Sets.equal

let mk_finite = Sets.finite

let mk_cnstrnt = Sets.cnstrnt
	
let _ = Callback.register "mk_empty" mk_empty
let _ = Callback.register "mk_full" mk_full
let _ = Callback.register "mk_setite" mk_setite
let _ = Callback.register "mk_compl" mk_compl
let _ = Callback.register "mk_inter" mk_inter
let _ = Callback.register "mk_union" mk_union
let _ = Callback.register "mk_diff" mk_diff
let _ = Callback.register "mk_sym_diff" mk_sym_diff
let _ = Callback.register "mk_finite" mk_finite
let _ = Callback.register "mk_cnstrnt" mk_cnstrnt

let mk_bv_eps = Bv.eps
let mk_bv_zero = Bv.zero
let mk_bv_one = Bv.one
let mk_bv_const s = Bv.const (Bitv.from_string s)
let mk_bv_conc (n1,b1) (n2,b2) =
  if n1 >= 0 && n2 >= 0 then
    Bv.conc2 n1 n2 b1 b2
  else
    raise (Invalid_argument "Bitvector concatenation")
    
let mk_bv_extr (n,b) i j =
  if 0 <= i && i < j && i < n then
    Bv.sub n i j b
  else
    raise (Invalid_argument "Bitvector extraction")
    
let mk_bv_neg  = Bv.neg
let mk_bv_and = Bv.conj
let mk_bv_or = Bv.disj
let mk_bv_xor = Bv.xor

let width_of a =
  try
    Some(Bv.width a)
  with
      Invalid_argument _ -> None
	     
let _ = Callback.register "mk_bv_eps" mk_bv_eps
let _ = Callback.register "mk_bv_zero" mk_bv_zero
let _ = Callback.register "mk_bv_one" mk_bv_one
let _ = Callback.register "mk_bv_const" mk_bv_const
let _ = Callback.register "mk_bv_conc" mk_bv_conc
let _ = Callback.register "mk_bv_neg" mk_bv_neg
let _ = Callback.register "mk_bv_extr" mk_bv_extr
let _ = Callback.register "mk_bv_and" mk_bv_and
let _ = Callback.register "mk_bv_or" mk_bv_or
let _ = Callback.register "mk_bv_xor" mk_bv_xor
let _ = Callback.register "width_of" width_of    

let tag t = t.tag
let _ = Callback.register "tag" tag
	  
(*s Test functions. *)

    (*
let is_equal t = match t.node with Equal _ -> true | _ -> false
let is_nonneg t = match t.node with Cnstrnt(Nonneg,_) -> true | _ -> false
let is_neg t = match t.node with Cnstrnt(Neg,_) -> true | _ -> false
let is_pos t = match t.node with Cnstrnt(Pos,_) -> true | _ -> false
let is_nonpos t = match t.node with Cnstrnt(Nonpos,_) -> true | _ -> false
let is_int t = match t.node with Cnstrnt(Int,_) -> true | _ -> false
let is_real t = match t.node with Cnstrnt(Real,_) -> true | _ -> false
      *)

let is_equal = Bool.is_equal

let is_tuple t = match t.node with Tuple Tup _ -> true | _ -> false
let is_proj t = match t.node with Tuple (Proj _) -> true | _ -> false

let is_empty t = match t.node with Set (Empty _) -> true | _ -> false
let is_full t = match t.node with Set (Full _) -> true | _ -> false
let is_setite t = match t.node with Set (SetIte _) -> true | _ -> false
let is_compl = Sets.is_compl
let is_inter = Sets.is_inter
let is_union = Sets.is_union

let is_true t = match t.node with Bool True -> true | _ -> false
let is_false t = match t.node with Bool True -> true | _ -> false
let is_ite t = match t.node with Bool Ite _ -> true | _ -> false
let is_not = Bool.is_neg
let is_and = Bool.is_neg
let is_or = Bool.is_neg
let is_xor = Bool.is_xor
let is_imp = Bool.is_imp
let is_iff = Bool.is_iff

let is_bv_const t = match t.node with Bv Const _ -> true | _ -> false
let is_bv_zero = Bv.is_zero
let is_bv_one = Bv.is_one
let is_bv_conc t = match t.node with Bv Conc _ -> true | _ -> false
let is_bv_extr t = match t.node with Bv Extr _ -> true | _ -> false
let is_bv_ite t = match t.node with Bv BvIte _ -> true | _ -> false


let _ = Callback.register "is_equal" is_equal
let _ = Callback.register "is_tuple" is_tuple
let _ = Callback.register "is_proj" is_proj
let _ = Callback.register "is_num" is_num
let _ = Callback.register "is_multq" is_multq  
let _ = Callback.register "is_mult" is_mult
let _ = Callback.register "is_add" is_add
let _ = Callback.register "is_empty" is_empty
let _ = Callback.register "is_full" is_full
let _ = Callback.register "is_setite" is_setite
let _ = Callback.register "is_compl" is_compl
let _ = Callback.register "is_inter" is_inter
let _ = Callback.register "is_union" is_union
let _ = Callback.register "is_true" is_true
let _ = Callback.register "is_false" is_false
let _ = Callback.register "is_ite" is_ite
let _ = Callback.register "is_not" is_not
let _ = Callback.register "is_and" is_and
let _ = Callback.register "is_or" is_or
let _ = Callback.register "is_xor" is_xor
let _ = Callback.register "is_imp" is_imp
let _ = Callback.register "is_iff" is_iff
let _ = Callback.register "is_bv_const" is_bv_const
let _ = Callback.register "is_bv_zero" is_bv_zero
let _ = Callback.register "is_bv_one" is_bv_one
let _ = Callback.register "is_bv_conc" is_bv_conc
let _ = Callback.register "is_bv_extr" is_bv_extr
let _ = Callback.register "is_bv_ite" is_bv_ite


(*s Destructors *)

let d_tuple t = 
  match t.node with
    | Tuple Tup l -> l
    | _ -> raise (Invalid_argument "Ics.d_tuple: Tuple required")

let d_proj t = 
  match t.node with
    | Tuple Proj (n,i,x) -> n,i,x
    | _ -> raise (Invalid_argument "Ics.d_proj: Projection required")

let d_ite t = 
  match t.node with
    | Bool Ite (x,y,z) -> x,y,z
    | _ -> raise (Invalid_argument "Ics.d_ite: Ite structure required")

let d_not t = 
  match t.node with
    | Bool Ite (x,{node=Bool False},{node=Bool True}) -> x
    | _ -> raise (Invalid_argument "Ics.d_ite: Ite structure required")

let d_and t = 
  match t.node with
    | Bool Ite (x,{node=Bool False},y) -> x,y
    | _ -> raise (Invalid_argument "Ics.d_and: Conjunction required")

let d_or t = 
  match t.node with
    | Bool Ite (x,y,{node=Bool True}) -> x,y
    | _ -> raise (Invalid_argument "Ics.d_or: Disjunction required")

let d_xor t = 
  match t.node with 
    | Bool Ite(x,{node=Bool Ite (y,{node=Bool False}, {node= Bool True})}, y') when y == y' -> x, y
    | _ -> raise (Invalid_argument "Ics.d_xor: Exclusive or required")

let d_imp t = 
  match t.node with 
    | Bool Ite(x,y,{node=Bool True}) -> x, y 
    | _ -> raise (Invalid_argument "Ics.d_imp: Implication required")

let d_iff t =
  match t.node with
    | Bool Ite(x,y,{node=Bool Ite(y',{node=Bool False},{node=Bool True})}) when y == y' -> (x,y)
    | _ -> raise  (Invalid_argument "Ics.d_iff: Equivalence required")

let d_equal t =
  match t.node with
    | Bool(Equal(x,y)) -> (x,y)
    | _ -> raise  (Invalid_argument "Ics.d_equal: Equal required")

let d_setite t =
  match t.node with
    | Set (SetIte(tg,x,y,z)) -> (tg,x)
    | _ -> raise  (Invalid_argument "Ics.d_setite: Setite required")

let d_compl t =
  match t.node with
    | Set (SetIte(tg,x,{node=Set Empty _},{node=Set Full _})) -> (tg,x)
    | _ -> raise  (Invalid_argument "Ics.d_compl: Set complement required")

let d_inter t =
  match t.node with
    | Set (SetIte(tg,x,y,{node=Set Empty _})) -> (tg,x,y)
    | _ -> raise  (Invalid_argument "Ics.d_inter: Set intersection required")

let d_union t =
  match t.node with
    | Set (SetIte(tg,x,{node=Set Full _},y)) -> (tg,x,y)
    | _ -> raise  (Invalid_argument "Ics.d_union: Set union required")

let d_bv_const t =
  match t.node with
    | Bv (Const b) -> b
    | _ -> raise  (Invalid_argument "Ics.d_bv_const: Constant bitvector required")

let d_bv_conc t =
  match t.node with
    | Bv Conc l -> l
    | _ -> raise  (Invalid_argument "Ics.d_bv_conc: Bitvector Concatenation required")

let d_bv_extr t =
  match t.node with
    | Bv Extr((n,x),i,j) -> ((n,x),i,j)
    | _ -> raise  (Invalid_argument "Ics.d_bv_extr: Bitvector Extraction required")

let d_bv_ite t =
  match t.node with
    | Bv BvIte((n,x),(m,y),(k,z)) when n = m && m == k -> (n,x,y,z)
    | _ -> raise  (Invalid_argument "Ics.d_bv_ite: Bitwise operator required")
 


let _ = Callback.register "d_app" d_app
let _ = Callback.register "d_update" d_update
let _ = Callback.register "d_num" d_num
let _ = Callback.register "d_add" d_add
let _ = Callback.register "d_mult" d_mult
let _ = Callback.register "d_multq" d_multq 
let _ = Callback.register "d_tuple" d_tuple
let _ = Callback.register "d_proj" d_proj
let _ = Callback.register "d_not" d_not
let _ = Callback.register "d_and" d_and
let _ = Callback.register "d_or" d_or
let _ = Callback.register "d_xor" d_xor
let _ = Callback.register "d_imp" d_imp
let _ = Callback.register "d_iff" d_iff
let _ = Callback.register "d_setite" d_setite
let _ = Callback.register "d_compl" d_compl
let _ = Callback.register "d_union" d_union
let _ = Callback.register "d_inter" d_inter
let _ = Callback.register "d_bv_const" d_bv_const
let _ = Callback.register "d_bv_conc" d_bv_conc
let _ = Callback.register "d_bv_extr" d_bv_extr
let _ = Callback.register "d_bv_ite" d_bv_ite


(*s Pretty-print of terms. *)

let term_pp t =
  begin
    Pretty.term Format.std_formatter t;
    Format.pp_print_flush Format.std_formatter ()
  end

let eqn_pp e =
  begin
    Pretty.eqn Format.std_formatter e;
    Format.pp_print_flush Format.std_formatter ()
  end

let _ = Callback.register "term_pp" term_pp
let _ = Callback.register "eqn_pp" eqn_pp
	  

(*s Equalities. *)

let term_eq = (===)
let term_cmp = cmp

let _ = Callback.register "term_eq" term_eq
let _ = Callback.register "term_cmp" term_cmp

(*s Verbose level. *)

let set_verbose = Tools.set_verbose
let _ = Callback.register "set_verbose" set_verbose

(*s States. *)

module Tmap = Term.Map

type state = State.t

let state_eq = (==)

let init () = State.empty

let ctxt_of = State.ctxt_of
let find_of = State.find_of
let ext_of = State.ext_of
let cnstrnt_of = State.cnstrnt_of
let use_of  = State.use_of

let find = State.find

let ext st a =
  assert (State.find st a === a);
  let ts = State.ext st a in
  Set.to_list ts

let use st a =
  (* assert (Term.is_uninterpreted a); *)
  let ts = State.use st a in
  Set.to_list ts
 
let uninterp st a =
  match Funsym.of_term a with
    | Some(f) ->
	let ts = State.uninterp st f in
	Set.to_list ts
    | _ -> assert false

let state_pp s =
  let find = find_of s in
  let cnstrnt = cnstrnt_of s in
  let freshvars = ref Term.Set.empty in
  let printed = ref Term.Set.empty in (* which constraints have already been printed. *)
  Format.printf "@[";
  Subst.iter
    (fun (a,b) ->
       freshvars :=
		  Term.Set.union
		    (Term.Set.union
		       (Var.fresh_of a)
		       (Var.fresh_of b))
	           !freshvars;
       Format.printf "@[";
       term_pp a;
       Format.printf " |-> ";
       term_pp b;
       if not(is_num b) then                  (*i don't print the trivial constraints. i*)
	 begin
	   let c = Term.Map.find b cnstrnt in
	   printed := Term.Set.add b !printed;
	   begin
	     if not(Interval.is_top c) then
	       begin     
		 Format.printf "@ with ";
		 cnstrnt_pp c
	       end
	   end
	 end;
	 Format.printf "@.@]")
    find;
  let cnstrnt' = Term.Set.fold Term.Map.remove !printed cnstrnt in
  if not(Term.Map.is_empty cnstrnt') then
    begin
      Format.printf "@[@.With additional constraints:@.";
      Pretty.tmap Pretty.cnstrnt Format.std_formatter cnstrnt';
      Format.printf "@.@]"
    end;
  if not(Term.Set.is_empty !freshvars) then
    begin
    Format.printf "@[@.With fresh variables:@.";
    Pretty.tset Format.std_formatter !freshvars;
    Format.printf "@.@]"
    end;
  Format.printf "@]"
  
let _ = Callback.register "state_eq" state_eq  
let _ = Callback.register "ctxt_of" ctxt_of
let _ = Callback.register "ext_of" ext_of
let _ = Callback.register "find_of" find_of  
let _ = Callback.register "use_of" use_of
let _ = Callback.register "init" init
let _ = Callback.register "find" find
let _ = Callback.register "use" use
let _ = Callback.register "uninterp" uninterp
let _ = Callback.register "state_pp" state_pp

	  
(*s Processing of new equalities. *)

type status =
  | Valid
  | Inconsistent
  | Consistent of State.t

let is_consistent = function Consistent _ -> true | _ -> false
let is_redundant r = (r = Valid)
let is_inconsistent r = (r = Inconsistent)

let d_consistent = function
  | Consistent s -> s
  | _ -> assert false  
	  
let process st t =
  try
    let t' = Can.can st t in
    match t'.node with
      | Bool True ->
	  Valid
      | Bool False ->
	  Inconsistent
      | Bool(Equal(t1,t2)) ->
	  Consistent (Process.process None (t1,t2) st)
      | _ ->
	  Consistent (Process.process None (t', Bool.tt ()) st)
  with
    | Exc.Inconsistent -> Inconsistent
    | Exc.Valid -> Valid

	  
let is_valid st p = 
  match process st p with
    | Valid -> true
    | _ -> false

let is_unsat st p =
  match process st p with
    | Inconsistent -> true
    | _ -> false
	
let _ = Callback.register "is_consistent" is_consistent
let _ = Callback.register "is_redundant" is_redundant
let _ = Callback.register "is_inconsistent" is_inconsistent  
let _ = Callback.register "d_consistent" d_consistent  

let _ = Callback.register "is_valid" is_valid
let _ = Callback.register "is_unsat" is_unsat
let _ = Callback.register "process" process     


(*s Normalization functions *)

let sigma st t = t
let norm s t = Subst.norm (State.to_subst s) t
let can = Can.can
let simplify = Can.simplify
	    
let solve x s e =
  let s' = State.copy s in
  Solve.solve x s' e
    
let _ = Callback.register "norm" norm 
let _ = Callback.register "can" can
let _ = Callback.register "simplify" simplify
let _ = Callback.register "solve" solve

	  
(*s Abstract sign interpretation. *)

let cnstrnt = State.cnstrnt

(*s Tools *)

let reset () = Tools.do_at_reset ()
let gc () = Gc.full_major ()
let flush = print_flush

let _ = Callback.register "reset" reset
let _ = Callback.register "gc" gc
let _ = Callback.register "flush" flush


(*s Imperative API. *)

type istate = {
  stack : state Stack.t;
  mutable current : state 
}

let iinit () = { 
  stack = Stack.create (); 
  current = init () 
}

let current s = 
  s.current

let push s = 
  Stack.push s.current s.stack

let pop s = 
  try 
    let c = Stack.pop s.stack in 
    s.current <- c
  with Stack.Empty -> 
    s.current <- (init())

let iprocess s t = 
  match process s.current t with
    | Consistent s' as r -> s.current <- s'; r
    | s' -> s'

let _ = Callback.register "iinit" iinit
let _ = Callback.register "current" current
let _ = Callback.register "push" push
let _ = Callback.register "pop" pop
let _ = Callback.register "iprocess" iprocess


(*s Callbacks for the basic Caml data structures. *)

(*s Lists. *)

let is_nil = function [] -> true | _ -> false
let cons x l = x :: l
let head = List.hd
let tail = List.tl
	     
let list_pp p l =
  Pretty.list (fun fmt -> p) Format.std_formatter l;
  Format.pp_print_flush Format.std_formatter ()

let _ = Callback.register "is_nil" is_nil
let _ = Callback.register "cons" cons
let _ = Callback.register "head" head
let _ = Callback.register "tail" tail
let _ = Callback.register "list_pp" list_pp

(*s Pairs. *)

let pair x y = (x,y)
let fst = fst
let snd = snd

let _ = Callback.register "pair" pair
let _ = Callback.register "fst" fst
let _ = Callback.register "snd" snd

(*s Options. *)

let is_some = function
  | Some _ -> true
  | None -> false

let is_none = function
  | None -> true
  | _ -> false

let value_of = function
  | Some(x) -> x
  | _ -> assert false
	
let _ = Callback.register "is_some" is_some
let _ = Callback.register "is_none" is_none
let _ = Callback.register "value_of" value_of
	  
	 
(*s Multi-precision arithmetic.*)

open Mpa

type q = Q.t

let num_of_int = Q.of_int
		   
let num_of_ints i1 i2 =
  Q.div (num_of_int i1) (num_of_int i2)
    
let string_of_num = Q.to_string
let num_of_string = Q.of_string
let mk_string s = s

let _ = Callback.register "num_of_int" num_of_int
let _ = Callback.register "string_of_num" string_of_num
let _ = Callback.register "num_of_string" num_of_string

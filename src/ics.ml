
(*i*)
open Format
open Hashcons
open Term
(*i*)

let init (n) =
  Tools.set_verbose n;
  Sys.catch_break true                 (*s raise [Sys.Break] exception upon *)
                                       (*s user interrupt. *)

let _ = Callback.register "init" init

(*s Intervals. *)

type low_bound = Interval.low           (*s lower bounds of intervals. *)

let low_bound_neginf () = Interval.Neginf
let _ = Callback.register "low_bound_neginf" low_bound_neginf

let low_bound_open q = Interval.Low(Interval.Open,q)
let _ = Callback.register "low_bound_open" low_bound_open

let low_bound_closed q = Interval.Low(Interval.Closed,q)	
let _ = Callback.register "low_bound_closed" low_bound_closed    

let low_bound_is_neginf = function
  | Interval.Neginf -> true
  | _ -> false
let _ = Callback.register "low_bound_is_neginf" low_bound_is_neginf
	
let low_bound_is_open = function
  | Interval.Low(Interval.Open,_) -> true
  | _ -> false
let _ = Callback.register "low_bound_is_open" low_bound_is_open
	
let low_bound_is_closed = function
  | Interval.Low(Interval.Closed,_) -> true
  | _ -> false
let _ = Callback.register "low_bound_is_closed" low_bound_is_closed
		   
let low_bound_value = function
  | Interval.Low(_,q) -> q
  | _ -> assert false
let _ = Callback.register "low_bound_value" low_bound_value
  
  

type high_bound = Interval.high        (* higher bounds of intervals. *)
		    
let high_bound_posinf () = Interval.Posinf
let _ = Callback.register "high_bound_posinf" high_bound_posinf

let high_bound_open q = Interval.High(Interval.Open,q)
let _ = Callback.register "high_bound_open" high_bound_open

let high_bound_closed q = Interval.High(Interval.Closed,q)	    
let _ = Callback.register "high_bound_closed" high_bound_closed 	

let high_bound_is_posinf = function
  | Interval.Posinf -> true
  | _ -> false
let _ = Callback.register "high_bound_is_posinf" high_bound_is_posinf
	
let high_bound_is_open = function
  | Interval.High(Interval.Open,_) -> true
  | _ -> false
let _ = Callback.register "high_bound_is_open" high_bound_is_open
	
let high_bound_is_closed = function
  | Interval.High(Interval.Closed,_) -> true
  | _ -> false
let _ = Callback.register "high_bound_is_closed" high_bound_is_closed
		   
let high_bound_value = function
  | Interval.High(_,q) -> q
  | _ -> assert false
let _ = Callback.register "high_bound_value" high_bound_value
  

    (*s Intervals are interpreted either over the reals, the integers, or the reals without
      the integers. *)
   
type interval_domain = Interval.domain

let interval_domain_real () = Interval.Real
let _ = Callback.register "interval_domain_real" interval_domain_real

let interval_domain_int () = Interval.Int
let _ = Callback.register "interval_domain_int" interval_domain_int

let interval_domain_nonint () = Interval.NonintReal
let _ = Callback.register "interval_domain_nonint" interval_domain_nonint


let interval_domain_is_real d = (d = Interval.Int)
let _ = Callback.register "interval_domain_is_real" interval_domain_is_real

let interval_domain_is_int d = (d = Interval.Real)
let _ = Callback.register "interval_domain_is_int" interval_domain_is_int

let interval_domain_is_nonint d = (d = Interval.NonintReal)
let _ = Callback.register "interval_domain_is_nonint" interval_domain_is_nonint


      (*s Listify constraints as the disjunction of singleton constraints. *)

type interval = Interval.interval

let interval_pp i = Interval.pp Format.std_formatter (Interval.inj i)
let _ = Callback.register "interval_pp" interval_pp

(*s Constrains. *)

type cnstrnt = Term.cnstrnt

let cnstrnt_pp = Pretty.cnstrnt Format.std_formatter
let _ = Callback.register "cnstrnt_pp" cnstrnt_pp

let cnstrnt_top  = Cnstrnt.top
let _ = Callback.register "cnstrnt_top" cnstrnt_top

let cnstrnt_bot = Cnstrnt.bot
let _ = Callback.register "cnstrnt_bot" cnstrnt_bot

let cnstrnt_boolean = Cnstrnt.boolean
let _ = Callback.register "cnstrnt_boolean" cnstrnt_boolean

let cnstrnt_tuple = Cnstrnt.tuple
let _ = Callback.register "cnstrnt_tuple" cnstrnt_tuple

let cnstrnt_arith l =  Cnstrnt.arith (Interval.of_list l)
let _ = Callback.register "cnstrnt_arith" cnstrnt_arith

let cnstrnt_oo = Cnstrnt.oo
let _ = Callback.register "cnstrnt_oo" cnstrnt_oo

let cnstrnt_oc = Cnstrnt.oc
let _ = Callback.register "cnstrnt_oc" cnstrnt_oc

let cnstrnt_co = Cnstrnt.co
let _ = Callback.register "cnstrnt_co" cnstrnt_co

let cnstrnt_cc = Cnstrnt.cc
let _ = Callback.register "cnstrnt_cc" cnstrnt_cc

let cnstrnt_lt = Cnstrnt.lt
let _ = Callback.register "cnstrnt_lt" cnstrnt_lt

let cnstrnt_le = Cnstrnt.le
let _ = Callback.register "cnstrnt_le" cnstrnt_le

let cnstrnt_gt = Cnstrnt.gt
let _ = Callback.register "cnstrnt_gt" cnstrnt_gt

let cnstrnt_ge = Cnstrnt.ge
let _ = Callback.register "cnstrnt_ge" cnstrnt_ge

let cnstrnt_real = Cnstrnt.real
let _ = Callback.register "cnstrnt_real" cnstrnt_real

let cnstrnt_int  = Cnstrnt.int
let _ = Callback.register "cnstrnt_int" cnstrnt_int

let cnstrnt_nonint = Cnstrnt.nonint
let _ = Callback.register "cnstrnt_nonint" cnstrnt_nonint

let cnstrnt_neg = Cnstrnt.neg
let _ = Callback.register "cnstrnt_neg" cnstrnt_neg

let cnstrnt_nonpos = Cnstrnt.nonpos
let _ = Callback.register "cnstrnt_nonpos" cnstrnt_nonpos

let cnstrnt_singleton = Cnstrnt.singleton
let _ = Callback.register "cnstrnt_singleton" cnstrnt_singleton

let cnstrnt_diseq = Cnstrnt.diseq
let _ = Callback.register "cnstrnt_diseq" cnstrnt_diseq

let cnstrnt_is_bot = Cnstrnt.is_bot
let _ = Callback.register "cnstrnt_is_bot" cnstrnt_is_bot

let cnstrnt_is_top = Cnstrnt.is_top
let _ = Callback.register "cnstrnt_is_top" cnstrnt_is_top

let cnstrnt_is_tuple = Cnstrnt.is_tuple
let _ = Callback.register "cnstrnt_is_tuple" cnstrnt_is_tuple

let cnstrnt_is_boolean = Cnstrnt.is_boolean
let _ = Callback.register "cnstrnt_is_boolean" cnstrnt_is_boolean

let cnstrnt_is_arith = Cnstrnt.is_arith
let _ = Callback.register "cnstrnt_is_arith" cnstrnt_is_arith

let cnstrnt_d_arith c = Interval.to_list (Cnstrnt.d_arith c)
let _ = Callback.register "cnstrnt_d_arith" cnstrnt_d_arith

let cnstrnt_is_singleton = Cnstrnt.is_singleton
let _ = Callback.register "cnstrnt_is_singleton" cnstrnt_is_singleton

let cnstrnt_d_singleton = Cnstrnt.d_singleton
let _ = Callback.register "cnstrnt_d_singleton" cnstrnt_d_singleton

let cnstrnt_is_nonzero = Cnstrnt.is_nonzero
let _ = Callback.register "cnstrnt_is_nonzero" cnstrnt_is_nonzero

let cnstrnt_inter = Cnstrnt.inter
let _ = Callback.register "cnstrnt_inter" cnstrnt_inter

let cnstrnt_union = Cnstrnt.union
let _ = Callback.register "cnstrnt_union" cnstrnt_union

let cnstrnt_compl = Cnstrnt.compl
let _ = Callback.register "cnstrnt_compl" cnstrnt_compl

let cnstrnt_of_interp = Cnstrnt.of_interp
let _ = Callback.register "cnstrnt_of_interp" cnstrnt_of_interp


(*s Predefined domains of interpretation for terms. *)

type domain = Term.domain

let is_intdom d = (d = IntDom)
let _ = Callback.register "is_intdom" is_intdom

let is_ratdom d = (d = RatDom)
let _ = Callback.register "is_ratdom" is_ratdom

let is_booldom d = (d = BoolDom)
let _ = Callback.register "is_booldom" is_booldom

(*s Terms ar either variables, uninterpreted applications,
  or interpreted applications including boolean terms. *)
  
type term = Term.t

let is_external = Term.is_external
let _ = Callback.register "is_external" is_external

(*s Variables. *)
	
let mk_var = Term.mk_var
let _ = Callback.register "mk_var" mk_var

let mk_intvar = Term.mk_intvar
let _ = Callback.register "mk_intvar" mk_intvar

let mk_boolvar = Term.mk_boolvar
let _ = Callback.register "mk_boolvar" mk_boolvar

let mk_ratvar = Term.mk_ratvar
let _ = Callback.register "mk_ratvar" mk_ratvar

let is_var = Term.is_var
let _ = Callback.register "is_var" is_var

let d_var = Term.d_var
let _ = Callback.register "d_var" d_var

let is_intvar = Term.is_intvar
let _ = Callback.register "is_intvar" is_intvar

let is_boolvar = Term.is_boolvar
let _ = Callback.register "is_boolvar" is_boolvar

let is_ratvar = Term.is_ratvar
let _ = Callback.register "is_ratvar" is_ratvar

let mk_rename_var = Term.mk_rename_var
let _ = Callback.register "mk_rename_var" mk_rename_var

let is_rename_var = Term.is_rename_var
let _ = Callback.register "is_rename_var" is_rename_var

let d_rename_var = Term.d_rename_var
let _ = Callback.register "d_rename_var" d_rename_var

let mk_fresh = Term.mk_fresh
let _ = Callback.register "mk_fresh" mk_fresh

let is_fresh = Term.is_fresh
let _ = Callback.register "is_fresh" is_fresh

let d_fresh = Term.d_fresh
let _ = Callback.register "d_fresh" d_fresh

let is_internal_var = Term.is_internal_var
let _ = Callback.register "is_internal_var" is_internal_var

(*s Uninterpred function application and function update. *)
         
let mk_uninterp f = Bool.nary_lift_ite (App.uninterp None None f)
let _ = Callback.register "mk_uninterp" mk_uninterp

let mk_uninterp_a f = Bool.nary_lift_ite (App.uninterp None (Some(A)) f)
let _ = Callback.register "mk_uninterp_a" mk_uninterp_a

let mk_uninterp_ac f = Bool.nary_lift_ite (App.uninterp None (Some(AC)) f)
let _ = Callback.register "mk_uninterp_ac" mk_uninterp_ac

let mk_uninterp_c f = Bool.nary_lift_ite (App.uninterp None (Some(C)) f)
let _ = Callback.register "mk_uninterp_c" mk_uninterp_c

let is_uninterp = Term.is_uninterp
let _ = Callback.register "is_uninterp" is_uninterp

let d_uninterp a = 
  assert(is_uninterp a);
  match a.node with
    | App({node=Uninterp(f,_,_)},l) -> (f,l)
    | _ -> assert false

let _ = Callback.register "d_uninterp" d_uninterp


(*s Update. *)

let mk_update a b c = Bool.ternary_lift_ite Builtin.update (a,b,c)
let _ = Callback.register "mk_update" mk_update    

let is_update = Term.is_update
let _ = Callback.register "is_update" is_update

let d_update = Term.d_update
let _ = Callback.register "d_update" d_update  

  


  
(*s Constructing arithmetic expressions. *)

let mk_num = Arith.num
let _ = Callback.register "mk_num" mk_num

let mk_add = Bool.nary_lift_ite Arith.add
let _ = Callback.register "mk_add" mk_add

let mk_add2 a b = Bool.binary_lift_ite Arith.add2 (a,b)
let _ = Callback.register "mk_add2" mk_add2

let mk_sub a b = Bool.binary_lift_ite Arith.sub (a,b)
let _ = Callback.register "mk_sub" mk_sub

let mk_unary_minus  = Bool.unary_lift_ite Arith.neg
let _ = Callback.register "mk_unary_minus" mk_unary_minus

let mk_mult2 a b = Bool.binary_lift_ite Arith.mult2 (a,b)
let _ = Callback.register "mk_mult2" mk_mult2

let mk_mult = Bool.nary_lift_ite Arith.mult
let _ = Callback.register "mk_mult" mk_mult

let is_arith = Arith.is_arith
let _ = Callback.register "is_arith" is_arith

let is_num = Term.is_num
let _ = Callback.register "is_num" is_num

let is_multq = Term.is_multq
let _ = Callback.register "is_multq" is_multq

let is_mult = Term.is_mult
let _ = Callback.register "is_mult" is_mult

let is_add = Term.is_add
let _ = Callback.register "is_add" is_add

let d_num = Term.d_num
let _ = Callback.register "d_num" d_num

let d_add = Term.d_add
let _ = Callback.register "d_add" d_add

let d_mult = Term.d_mult
let _ = Callback.register "d_mult" d_mult

let d_multq = Term.d_multq
let _ = Callback.register "d_multq" d_multq

let mk_expt a n =
  let rec loop acc i =
    if i = n then acc else loop (a :: acc) (i + 1)
  in
  Arith.mult (loop [] 0)
let _ = Callback.register "mk_expt" mk_expt

(*s Division. *)

let mk_div a b = Bool.binary_lift_ite Arith.div (a,b)
let _ = Callback.register "mk_div" mk_div
  
let is_div = Term.is_div
let _ = Callback.register "is_div" is_div

let d_div = Term.d_div
let _ = Callback.register "d_div" d_div

let is_expt a =
  let rec same l =
    match l with
      | [] -> true
      | [_] -> true 
      | x1 :: ((x2 :: xl) as xl') ->
	  x1 === x2 && same xl'
  in  
  Term.is_mult a && same (d_mult a)

let _ = Callback.register "is_expt" is_expt

let d_expt a =
  assert(is_expt a);
  let l = d_mult a in
  assert(List.length l > 1);
  (List.hd l, List.length l)

let _ = Callback.register "d_expt" d_expt


(*s Constructing tuples and projections. *)

let mk_tuple = Bool.nary_lift_ite Tuple.tuple
let _ = Callback.register "mk_tuple" mk_tuple

let mk_proj i j = Bool.unary_lift_ite (Tuple.proj i j)
let _ = Callback.register "mk_proj" mk_proj

let is_tuple = Term.is_tuple
let _ = Callback.register "is_tuple" is_tuple

let is_proj = Term.is_proj
let _ = Callback.register "is_proj" is_proj

let d_tuple = Term.d_tuple
let _ = Callback.register "d_tuple" d_tuple
 
let d_proj = Term.d_proj
let _ = Callback.register "d_proj" d_proj

	  
(*s Boolean terms. *)

let mk_true () = Bool.tt ()
let _ = Callback.register "mk_true" mk_true

let mk_false () = Bool.ff ()
let _ = Callback.register "mk_false" mk_false

let mk_equal a b = Bool.binary_lift_ite Atom.equal (a,b)
let _ = Callback.register "mk_equal" mk_equal  

let mk_diseq a b = Bool.binary_lift_ite Atom.diseq (a,b)
let _ = Callback.register "mk_diseq" mk_diseq

let mk_cnstrnt c = Bool.unary_lift_ite (Atom.cnstrnt c)
let _ = Callback.register "mk_cnstrnt" mk_cnstrnt

let mk_int = Bool.unary_lift_ite (Atom.cnstrnt Cnstrnt.int)
let _ = Callback.register "mk_int" mk_int

let mk_real = Bool.unary_lift_ite (Atom.cnstrnt Cnstrnt.real)
let _ = Callback.register "mk_real" mk_real

let mk_nonint = Bool.unary_lift_ite (Atom.cnstrnt Cnstrnt.nonint)
let _ = Callback.register "mk_nonint" mk_nonint

let mk_lt a b = Bool.binary_lift_ite Arith.lt (a,b)
let _ = Callback.register "mk_lt" mk_lt

let mk_le a b = Bool.binary_lift_ite Arith.le (a,b)
let _ = Callback.register "mk_le" mk_le

let mk_gt a b = Bool.binary_lift_ite Arith.lt (b,a)
let _ = Callback.register "mk_gt" mk_gt

let mk_ge a b = Bool.binary_lift_ite Arith.le (b,a)
let _ = Callback.register "mk_ge" mk_ge

let mk_ite a b c = Bool.ite(a,b,c)
let _ = Callback.register "mk_ite" mk_ite

let mk_neg a =
  if Bool.is_neg a then 
    Bool.d_neg a
  else if Atom.is_atom a then
    try 
      let s = Dp.empty() in
      let _ = Dp.process s (Dp.can s a) in
      Bool.neg a
    with
      | Exc.Valid -> Bool.ff()
      | Exc.Inconsistent -> Bool.tt()
  else 
    Bool.neg a

let _ = Callback.register "mk_neg" mk_neg

let mk_conjl =
  let s = Dp.empty() in
  let rec loop acc =
    function 
      | [] -> acc
      | a :: al ->
	  if Atom.is_atom a then
	    try
	      let _ = Dp.process s (Dp.can s a) in
	      loop (Bool.conj a acc) al
	    with
	      | Exc.Inconsistent -> Bool.ff()
	      | Exc.Valid -> acc
	  else 
	    loop (Bool.conj a acc) al
	    
  in
  loop (Bool.tt())

let _ = Callback.register "mk_conjl" mk_conjl

let mk_conj a b =
  let rec flatten x =
    if Bool.is_conj x then 
      let (x1,x2) = Bool.d_conj x in
      flatten x1 @ flatten x2
    else 
      [x]
  in
  mk_conjl (flatten a @ flatten b)


let mk_disjl =
  let s = Dp.empty() in
  let rec loop acc =
    function 
      | [] -> acc
      | a :: al ->
	  if Atom.is_atom a then
	    try
	      let _ = Dp.process s (Dp.can s a) in
	      loop (Bool.disj a acc) al
	    with
	      | Exc.Inconsistent -> acc
	      | Exc.Valid -> Bool.ff()
	  else
	    loop (Bool.disj a acc) al
  in
  loop (Bool.ff())

let _ = Callback.register "mk_disjl" mk_disjl


let _ = Callback.register "mk_conj" mk_conj

let mk_disj a b = 
  let rec flatten x =
    if Bool.is_disj x then 
      let (x1,x2) = Bool.d_disj x in
      flatten x1 @ flatten x2
    else 
      [x]
  in
  mk_disjl (flatten a @ flatten b)

let _ = Callback.register "mk_disj" mk_disj


let mk_xor = Bool.xor
let _ = Callback.register "mk_xor" mk_xor

let mk_imp = Bool.imp
let _ = Callback.register "mk_imp" mk_imp

let mk_iff = Bool.iff
let _ = Callback.register "mk_iff" mk_iff

let is_bool = Bool.is_bool
let _ = Callback.register "is_bool" is_bool       

let is_true = Term.is_tt
let _ = Callback.register "is_true" is_true

let is_false = Term.is_ff
let _ = Callback.register "is_false" is_false

let is_equal = Term.is_equal
let _ = Callback.register "is_equal" is_equal

let is_diseq = Term.is_diseq
let _ = Callback.register "is_diseq" is_diseq

let is_cnstrnt = Term.is_cnstrnt
let _ = Callback.register "is_cnstrnt" is_cnstrnt

let is_ite  = Term.is_ite
let _ = Callback.register "is_ite" is_ite

let is_neg = Bool.is_neg
let _ = Callback.register "is_neg" is_neg

let is_conj = Bool.is_conj
let _ = Callback.register "is_conj" is_conj

let is_disj = Bool.is_disj
let _ = Callback.register "is_disj" is_disj

let is_xor = Bool.is_xor
let _ = Callback.register "is_xor" is_xor

let is_imp = Bool.is_imp
let _ = Callback.register "is_imp" is_imp

let is_iff = Bool.is_iff
let _ = Callback.register "is_iff" is_iff

let d_equal = Term.d_equal
let _ = Callback.register "d_equal" d_equal

let d_diseq = Term.d_diseq
let _ = Callback.register "d_diseq" d_diseq

let d_cnstrnt = Term.d_cnstrnt
let _ = Callback.register "d_cnstrnt" d_cnstrnt

let d_ite = Term.d_ite
let _ = Callback.register "d_ite" d_ite

let d_neg = Bool.d_neg
let _ = Callback.register "d_neg" d_neg

let d_conj = Bool.d_conj
let _ = Callback.register "d_conj" d_conj

let d_disj = Bool.d_disj
let _ = Callback.register "d_disj" d_disj

let d_xor = Bool.d_xor
let _ = Callback.register "d_xor" d_xor

let d_imp = Bool.d_imp
let _ = Callback.register "d_imp" d_imp

let d_iff = Bool.d_iff
let _ = Callback.register "d_iff" d_iff


(*s Set of terms. *)

type terms = Term.ts

let terms_empty () = Term.Set.empty
let _ = Callback.register "terms_empty" terms_empty

let terms_add = Term.Set.add
let _ = Callback.register "terms_add" terms_add

let terms_pp = Pretty.tset Format.std_formatter
let _ = Callback.register "terms_pp" terms_pp

let terms_mem = Term.Set.mem
let _ = Callback.register "terms_mem" terms_mem

let terms_sub = Term.Set.sub
let _ = Callback.register "terms_sub" terms_sub

let terms_is_empty = Term.Set.is_empty
let _ = Callback.register "terms_is_empty" terms_is_empty

let terms_to_list = Term.Set.to_list
let _ = Callback.register "terms_to_list" terms_to_list

let terms_of_list =
  List.fold_left (fun acc x -> Term.Set.add x acc) Term.Set.empty
let _ = Callback.register "terms_of_list" terms_of_list

let terms_choose = Term.Set.destructure
let _ = Callback.register "terms_choose" terms_choose


(*s Set of fresh variables. *)

let freshvars_of = Term.freshvars_of 
let _ = Callback.register "freshvars_of" freshvars_of

(*s Maps with terms as domain. *)

type 'a map = 'a Term.Map.t

let map_empty () = Term.Map.empty
let _ = Callback.register "map_empty" map_empty

let map_add = Term.Map.add
let _ = Callback.register "map_add" map_add

let map_is_empty = Term.Map.is_empty
let _ = Callback.register "map_is_empty" map_is_empty

let map_find = Term.Map.find
let _ = Callback.register "map_find" map_find

let map_remove = Term.Map.remove
let _ = Callback.register "map_remove" map_remove

let map_mem = Term.Map.mem
let _ = Callback.register "map_mem" map_mem

let map_to_list = Term.Map.to_list
let _ = Callback.register "map_to_list" map_to_list

let map_pp p = Pretty.tmap (fun fmt -> p) Format.std_formatter
let _ = Callback.register "map_pp" map_pp
	  
		    
(*s Substititon. *)
    
type subst = Subst.t

let subst_empty () = Subst.empty
let _ = Callback.register "subst_empty" subst_empty

let subst_add x t s = Subst.empty
let _ = Callback.register "subst_add" subst_add

let subst_mem = Subst.mem
let _ = Callback.register "subst_mem" subst_mem

let subst_find = Subst.find
let _ = Callback.register "subst_find" subst_find

let subst_apply = Subst.apply
let _ = Callback.register "subst_apply" subst_apply

let subst_of_list l = Subst.empty
let _ = Callback.register "subst_of_list" subst_of_list

let subst_to_list = Subst.to_list
let _ = Callback.register "subst_to_list" subst_to_list

let subst_pp = Subst.pp Format.std_formatter
let _ = Callback.register "subst_pp" subst_pp

(*s Term tag. *)

let term_tag a = a.tag
let _ = Callback.register "term_tag" term_tag


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
let _ = Callback.register "term_eq" term_eq

let term_cmp = Term.cmp
let _ = Callback.register "term_cmp" term_cmp

let term_fast_cmp = Term.fast_cmp
let _ = Callback.register "term_fast_cmp" term_fast_cmp



(*s Verbose level. *)

let set_verbose = Tools.set_verbose
let _ = Callback.register "set_verbose" set_verbose


(*s States. *)

open Dp

module Tmap = Term.Map

type state = Dp.t

type theories = Term.theories

let theory_arith () = Term.ArithTh
let _ = Callback.register "theory_arith" theory_arith

let theory_tuple () = Term.TupleTh
let _ = Callback.register "theory_tuple" theory_tuple

let theory_boolean () = Term.BooleanTh
let _ = Callback.register "theory_boolean" theory_boolean

let theory_eq () = Term.EqTh
let _ = Callback.register "theory_eq" theory_eq



let state_eq = (==)
let _ = Callback.register "state_eq" state_eq  

let state_init () = Dp.empty ()
let _ = Callback.register "state_init" state_init

let state_ctxt_of s = s.ctxt
let _ = Callback.register "state_ctxt_of" state_ctxt_of

let state_find th s =
  match th with
    | ArithTh -> Arith.find s.a
    | BooleanTh -> Bool.find s.b
    | TupleTh -> Tuple.find s.t
    | EqTh -> Cc.find s.u

let _ = Callback.register "state_find" state_find

let state_find_of th s =
  match th with
    | EqTh -> Cc.subst_of s.u
    | ArithTh -> Arith.subst_of s.a
    | TupleTh -> Tuple.subst_of s.t
    | BooleanTh -> Bool.subst_of s.b

let _ = Callback.register "state_find_of" state_find_of

let state_use_of th s =
  match th with
    | EqTh -> Cc.use_of s.u
    | ArithTh -> Arith.use_of s.a
    | TupleTh -> Tuple.use_of s.t
    | BooleanTh -> Bool.use_of s.b

let _ = Callback.register "state_use_of" state_use_of

let state_diseqs_of s = Cc.diseqs_of s.u
let _ = Callback.register "state_diseqs_of" state_diseqs_of


let state_cnstrnts_of s = Cc.cnstrnt_of s.u
let _ = Callback.register "state_cnstrnts_of" state_cnstrnts_of


let state_inconsistent = Dp.inconsistent

let _ = Callback.register "state_inconsistent" state_inconsistent

let state_solutions = Dp.solutions
let _ = Callback.register "state_solutions" state_solutions


let state_pp s = 
  let fmt = Format.std_formatter in
  let arith = state_find_of ArithTh s in
  let boolean = state_find_of BooleanTh s in
  let tuple = state_find_of TupleTh s in  
  let uninterp = state_find_of EqTh s in 
  let cnstrnts = Cc.cnstrnt_of s.u in
  let diseqs = Cc.diseqs_of s.u in
  if not(Subst.is_empty uninterp) then
    begin
      Format.printf "\nUninterpreted:\n";
      Subst.pp fmt uninterp
    end;
  if not(Subst.is_empty arith) then
    begin
      Format.printf "\nArithmetic:\n";
      Subst.pp fmt arith
    end;
  if not(Term.Map.is_empty cnstrnts) then
    begin
      Format.printf "\nCnstrnts:\n";
      Pretty.tmap Pretty.cnstrnt fmt cnstrnts
    end;
  if not(Term.Map.is_empty diseqs) then
    begin
      Format.printf "\nDisequalities:\n";
      Pretty.tmap Pretty.tset fmt diseqs
    end;
  if not(Subst.is_empty tuple) then
    begin
      Format.printf "\nTuples:\n";
      Subst.pp fmt tuple
    end;
  if not(Subst.is_empty boolean) then
    begin
      Format.printf "\nBoolean:\n";
      Subst.pp fmt boolean
    end

let _ = Callback.register "state_pp" state_pp

let state_use th s = 
  match th with
    | EqTh -> Cc.use s.u
    | ArithTh -> Arith.use s.a
    | TupleTh -> Tuple.use s.t
    | BooleanTh -> Bool.use s.b

let state_diseqs s = Cc.diseqs s.u
let _ = Callback.register "state_diseqs" state_diseqs

let state_ext = Dp.ext
let _ = Callback.register "state_ext" state_ext
	  
(*s Processing of new equalities. *)

type status =
  | Valid
  | Inconsistent
  | Consistent of Dp.t

let is_consistent = function Consistent _ -> true | _ -> false
let _ = Callback.register "is_consistent" is_consistent

let is_redundant r = (r = Valid)
let _ = Callback.register "is_redundant" is_redundant

let is_inconsistent r = (r = Inconsistent)
let _ = Callback.register "is_inconsistent" is_inconsistent  

let d_consistent = function
  | Consistent s -> s
  | _ -> failwith "Ics.d_consistent: fatal error"
	
let _ = Callback.register "d_consistent" d_consistent  

let rec process s a = 
  try
    Consistent(Dp.process s (Dp.can s a))
  with
    | Exc.Inconsistent -> Inconsistent
    | Exc.Valid -> Valid

let _ = Callback.register "process" process   


type tstatus = Dp.status

let is_check_inconsistent ts = (ts = Dp.Inconsistent)
let _ = Callback.register "is_check_inconsistent" is_check_inconsistent

let is_check_satisfiable ts =
  match ts with
    | Dp.Satisfiable _ -> true
    | _ -> false

let _ = Callback.register "is_check_satisfiable" is_check_satisfiable


let d_check_satisfiable ts = 
  match ts with
    | Dp.Satisfiable sl -> sl
    | _ -> assert false

let _ = Callback.register "d_check_satisfiable" d_check_satisfiable

let is_check_valid ts = (ts = Dp.Valid)
let _ = Callback.register "is_check_valid" is_check_valid
  
let check = Dp.check
let _ = Callback.register "check" check

(*s Normalization functions *)

let can = Dp.can_external
let _ = Callback.register "can" can


let rec defreshify a =       (*s Get rid of rename variables, and collect a list of *)
  let rec loop (acc,x) =     (*s bindings for the fresh variables in [a]. *)
    match x.node with
      | Var(_,k) ->
	  (match k with
	     | Ext _ -> 
		 (acc, x)
	     | Fresh _ -> 
		 (Term.Set.add x acc, x)
	     | Rename(y) -> 
		 loop (acc,y))
      | App(sym,l) ->
	  (match sym.node with
	     | Uninterp(f,d,p) ->       (* Only uninterpreted function symbols 'contain' terms. *)
		 let (acc',f') = loop (acc,f) in
		 let (acc'',l') = loopl (acc',l) in
		 (acc'', App.sigma (mk_uninterp_sym d p f') l')
	     | _ ->
		 let (acc',l') = loopl (acc,l) in
		 (acc', App.sigma sym l'))
  and loopl (acc,l) =
    match l with
      | [] -> 
	  (acc,[])
      | x :: xl ->
	  let (acc',x') = loop (acc,x) in
          let (acc'',xl') = loopl (acc',xl) in
	  (acc'', x' :: xl')
  in
  loop (Term.Set.empty, a)  


(*s Solving equation [e] in a given theory. *)
	    
let solve th s e =
  try
    Some(match th with
	   | EqTh -> 
	       [e]
	   | ArithTh ->
	       Arith.solve (Dp.cnstrnt s) e
	   | TupleTh -> 
	       Tuple.solve e
	   | BooleanTh ->
	       Bool.solve e)
  with
      Exc.Inconsistent -> None

let _ = Callback.register "solve" solve

  
(*s Abstract sign interpretation. *)

let cnstrnt s a = 
  Dp.cnstrnt s a


(*s Groebner basis completion. *)

let groebner s = 
  try
    Some(Dp.groebner s)
  with
      Exc.Inconsistent -> None


(*s Tools *)

let reset () = Tools.do_at_reset ()
let _ = Callback.register "reset" reset

let gc () = Gc.full_major ()
let _ = Callback.register "gc" gc

let flush = print_flush
let _ = Callback.register "flush" flush

let _ = Callback.register "groebner" groebner


(*s Callbacks for some basic Caml data structures. *)

(*s Lists. *)

let is_nil = function [] -> true | _ -> false
let _ = Callback.register "is_nil" is_nil

let cons x l = x :: l
let _ = Callback.register "cons" cons

let head = List.hd
let _ = Callback.register "head" head

let tail = List.tl
let _ = Callback.register "tail" tail
	     
let list_pp p l =
  Pretty.list (fun fmt -> p) Format.std_formatter l;
  Format.pp_print_flush Format.std_formatter ()

let _ = Callback.register "list_pp" list_pp

(*s Pairs. *)

let pair x y = (x,y)
let _ = Callback.register "pair" pair

let fst = fst
let _ = Callback.register "fst" fst

let snd = snd
let _ = Callback.register "snd" snd

(*s Triples. *)

let triple x y z = (x,y,z)
let _ = Callback.register "triple" triple

let fst_of_triple = function (x,_,_) -> x
let _ = Callback.register "fst_of_triple" fst_of_triple

let snd_of_triple = function (_,y,_) -> y
let _ = Callback.register "snd_of_triple" snd_of_triple

let third_of_triple = function (_,_,z) -> z
let _ = Callback.register "third_of_triple" third_of_triple
  

(*s Quadruples. *)
   
let fst_of_quadruple  = function (x1,_,_,_) -> x1
let _ = Callback.register "fst_of_quadruple" fst_of_quadruple

let snd_of_quadruple = function (_,x2,_,_) -> x2
let _ = Callback.register "snd_of_quadruple" snd_of_quadruple

let third_of_quadruple = function (_,_,x3,_) -> x3
let _ = Callback.register "third_of_quadruple" third_of_quadruple

let fourth_of_quadruple = function (_,_,_,x4) -> x4
let _ = Callback.register "fourth_of_quadruple" fourth_of_quadruple
    

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

let ints_of_num q =
  (Z.to_string (Q.numerator q), Z.to_string (Q.denominator q))
let _ = Callback.register "ints_of_num" ints_of_num


let num_of_int = Q.of_int
let _ = Callback.register "num_of_int" num_of_int
		   
let num_of_ints i1 i2 = Q.div (num_of_int i1) (num_of_int i2)
let _ = Callback.register "num_of_ints" num_of_ints

let string_of_num = Q.to_string
let _ = Callback.register "string_of_num" string_of_num

let num_of_string = Q.of_string
let _ = Callback.register "num_of_string" num_of_string

let mk_string s = s

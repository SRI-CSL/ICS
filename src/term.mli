
(*i*)
open Mpa
open Hashcons
open Bitv
(*i*)

(*s Constraints. *)

type nonreal =
  | Boolean
  | Predicate
  | Cartesian
  | Bitvector
  | Other

module Nonreals: (Set.S with type elt = nonreal) 

type cnstrnt =
  | Top
  | Sub of Nonreals.t * Interval.t
  | Bot
  
(*s Types of terms. *)

type variable = string

type tag = int

type t = tnode hashed

and tnode =
  | Var of variable
  | App of t * t list
  | Update of t * t * t
  | Arith of arith
  | Tuple of tuple
  | Bool of prop
  | Set of set
  | Bv of bv

and arith =
  | Num of Q.t
  | Add of t list
  | Multq of Q.t * t
  | Mult of t list
  | Div of t * t
  
and prop =
  | True
  | False
  | Equal of t * t  
  | Ite of t * t * t
  | Forall of variable list * t
  | Exists of variable list * t

and set =
  | Empty of tag
  | Full of tag
  | Finite of terms
  | Cnstrnt of cnstrnt
  | SetIte of tag * t * t * t
      
and tuple =
  | Tup of t list
  | Proj of int * int * t

and bv =
  | Const of Bitv.t
  | Conc of fixed list
  | BvToNat of t
  | Extr of fixed * int * int
  | BvIte of fixed * fixed * fixed
 
and fixed = int * t

and terms = tnode Ptset.t

type eqn = t * t
 
(*s Hashconsing of terms. Terms constructed this way can be compared using (==) *)

val hc : tnode -> t

(*s Fast comparison is done in constant time, but is session-dependent,
    since it uses physical addresses. In constrast, [cmp] is session-independent
    but requires linear time
 *)

val fast_cmp : t -> t -> int
val cmp : t -> t -> int

val is_const : t -> bool

(*s Some functions for distinguishing between interpreted and uninterpreted terms. *)

val is_uninterpreted : t -> bool

val iter_uninterpreted : (t -> unit) -> t -> unit

val occurs_interpreted : t -> t -> bool

val is_ground : t -> bool 
    
(*s Caches for functions over ts. *)
		  
val cache : int -> (t -> 'a) -> (t -> 'a)
val cache2 : int -> (t*t -> 'a) -> (t*t -> 'a)    
val cachel : int -> (t list -> 'a) -> (t list -> 'a)

(*s Set of terms *)

type term = t
    
module Set : sig
  type t = terms
  val empty : t
  val mem : term -> t -> bool
  val add : term -> t -> t
  val singleton: term -> t
  val sub : t -> t -> bool
  val is_empty : t -> bool
  val remove : term -> t -> t
  val union : t -> t -> t
  val inter : t -> t -> t
  val iter : (term -> unit) -> t -> unit
  val iter2 : (term -> term -> unit) -> t -> unit
  val fold : (term -> 'a -> 'a) -> t -> 'a -> 'a
  val map : (term -> term) -> t -> t
  val exists : (term -> bool) -> t -> bool
  val for_all : (term -> bool) -> t -> bool
  val filter : (term -> bool) -> t -> t
  val to_list : t -> term list
  val choose : (term -> bool) -> t -> term
  val destructure : t -> term * t
end

(*s Finite maps with terms as domain *)

module Map : sig
  type 'a t
  val empty : 'a t
  val is_empty : 'a t -> bool 
  val add : term -> 'a -> 'a t -> 'a t
  val find : term -> 'a t -> 'a
  val remove : term -> 'a t -> 'a t
  val mem :  term -> 'a t -> bool
  val iter : (term -> 'a -> unit) -> 'a t -> unit
  val map : ('a -> 'b) -> 'a t -> 'b t
  val fold : (term -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
  val choose : (term -> term -> bool) -> term t -> term * term
  val to_list : 'a t -> (term * 'a) list
end

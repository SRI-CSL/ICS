
(*i*)
open Hashcons
(*i*)

module type ITE = sig
  type bdd_node
  type bdd = bdd_node hashed
  type tag
  val compare : bdd -> bdd -> int
  val high : tag -> bdd
  val low : tag -> bdd
  val ite : tag -> bdd -> bdd -> bdd -> bdd
  val is_high : bdd -> bool
  val is_low : bdd -> bool
  val is_ite : bdd -> bool
  val destructure_ite : bdd -> (bdd * bdd * bdd) option
  val fresh : tag -> bdd
end

module Make(Ite : ITE) : sig
  open Ite
  val build : tag -> bdd * bdd * bdd -> bdd
         
  val neg : tag -> bdd -> bdd                  (* derived constructors *)
  val conj : tag -> bdd -> bdd -> bdd
  val disj : tag -> bdd -> bdd -> bdd
  val xor : tag -> bdd -> bdd -> bdd
  val imp : tag -> bdd -> bdd -> bdd
  val iff : tag -> bdd -> bdd -> bdd
      
  val is_neg : bdd -> bool                     (* derived recognizers *)
  val is_conj : bdd -> bool
  val is_disj : bdd -> bool
  val is_xor : bdd -> bool  
  val is_imp : bdd -> bool
  val is_iff : bdd -> bool
      
  val d_neg : bdd -> bdd                    (* derived deconstructors *)
  val d_conj : bdd -> bdd * bdd
  val d_disj : bdd ->bdd * bdd
  val d_xor : bdd -> bdd * bdd
  val d_imp : bdd -> bdd * bdd
  val d_iff : bdd -> bdd * bdd
      
  val solve : tag -> bdd -> (bdd * bdd) list  option        (* Solving bdds *)
  val solve1 : tag -> bdd -> (bdd * bdd) list option
end















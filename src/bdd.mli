
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
  val neg : tag -> bdd -> bdd
  val conj : tag -> bdd -> bdd -> bdd
  val disj : tag -> bdd -> bdd -> bdd
  val xor : tag -> bdd -> bdd -> bdd
  val imp : tag -> bdd -> bdd -> bdd
  val iff : tag -> bdd -> bdd -> bdd
  val solve : tag -> bdd -> (bdd * bdd) list
  val solve1 : tag -> bdd -> (bdd * bdd) list  
end


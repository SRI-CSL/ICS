(******************************************************
 * GNU MP interface for Objective CAML
 * Code by David Monniaux (David.Monniaux@ens-lyon.fr)
 * Adapted to ocaml 3.00 by Jean-Christophe Filliâtre
 **********x********************************************)

module Z2 : sig
  type t
  external copy : t -> t -> t = "ml_mpz2_copy"
  external from_int : t -> int -> t = "ml_mpz2_from_int"
  external from_string : t -> string -> int -> t = "ml_mpz2_from_string"
  external from_float : t -> float -> t = "ml_mpz2_from_float"
  external add : t -> t -> t -> t = "ml_mpz2_add"
  external sub : t -> t -> t -> t = "ml_mpz2_sub"
  external mul : t -> t -> t -> t = "ml_mpz2_mul"
  external pow_ui : t -> t -> int -> t = "ml_mpz2_pow_ui"
  external pow_ui_ui : t -> int -> int -> t = "ml_mpz2_ui_pow_ui"
  external powm : t -> t -> t -> t -> t = "ml_mpz2_powm"
  external powm_ui : t -> t -> int -> t -> t = "ml_mpz2_powm_ui"
  external sqrt : t -> t -> t = "ml_mpz2_sqrt"
  external add_ui : t -> t -> int -> t = "ml_mpz2_add_ui"
  external sub_ui : t -> t -> int -> t = "ml_mpz2_sub_ui"
  external mul_ui : t -> t -> int -> t = "ml_mpz2_mul_ui"
  external neg : t -> t -> t = "ml_mpz2_neg"
  external abs : t -> t -> t = "ml_mpz2_abs"
  external mul2exp : t -> t -> int -> t = "ml_mpz2_mul2exp"
  external fac_ui : t -> t -> int -> t = "ml_mpz2_fac_ui"
  external tdiv_q : t -> t -> t -> t = "ml_mpz2_tdiv_q"
  external tdiv_r : t -> t -> t -> t = "ml_mpz2_tdiv_r"
  external fdiv_q : t -> t -> t -> t = "ml_mpz2_fdiv_q"
  external fdiv_r : t -> t -> t -> t = "ml_mpz2_fdiv_r"
  external cdiv_q : t -> t -> t -> t = "ml_mpz2_cdiv_q"
  external cdiv_r : t -> t -> t -> t = "ml_mpz2_cdiv_r"
  external tdiv_q_2exp : t -> t -> int -> t = "ml_mpz2_tdiv_q_2exp"
  external tdiv_r_2exp : t -> t -> int -> t = "ml_mpz2_tdiv_r_2exp"
  external fdiv_q_2exp : t -> t -> int -> t = "ml_mpz2_fdiv_q_2exp"
  external fdiv_r_2exp : t -> t -> int -> t = "ml_mpz2_fdiv_r_2exp"
  external tdiv_q_ui : t -> t -> int -> t = "ml_mpz2_tdiv_q_ui"
  external tdiv_r_ui : t -> t -> int -> t = "ml_mpz2_tdiv_r_ui"
  external fdiv_q_ui : t -> t -> int -> t = "ml_mpz2_fdiv_q_ui"
  external fdiv_r_ui : t -> t -> int -> t = "ml_mpz2_fdiv_r_ui"
  external cdiv_q_ui : t -> t -> int -> t = "ml_mpz2_cdiv_q_ui"
  external cdiv_r_ui : t -> t -> int -> t = "ml_mpz2_cdiv_r_ui"
  external dmod : t -> t -> t -> t = "ml_mpz2_mod"
  external dmod_ui : t -> t -> int -> t = "ml_mpz2_mod_ui"
  external divexact : t -> t -> t -> t = "ml_mpz2_divexact"
  external band : t -> t -> t -> t = "ml_mpz2_and"
  external bxor : t -> t -> t -> t = "ml_mpz2_ior"
  external bnot : t -> t -> t = "ml_mpz2_com"
  external invert : t -> t -> t -> t = "ml_mpz2_invert"
  external setbit : t -> int -> t = "ml_mpz2_setbit"
  external clrbit : t -> int -> t = "ml_mpz2_clrbit"
end

module Z : sig
  type t = Z2.t
  external copy : t -> t = "ml_mpz_copy"
  external from_int : int -> t = "ml_mpz_from_int"
  external from_string : string -> int -> t = "ml_mpz_from_string"
  external from_float : float -> t = "ml_mpz_from_float"
  external int_from : t -> int = "ml_int_from_mpz"
  external float_from : t -> float = "ml_float_from_mpz"
  external string_from : t -> int -> string = "ml_string_from_mpz"
  external add : t -> t -> t = "ml_mpz_add"
  external sub : t -> t -> t = "ml_mpz_sub"
  external mul : t -> t -> t = "ml_mpz_mul"
  external pow_ui : t -> int -> t = "ml_mpz_pow_ui"
  external pow_ui_ui : int -> int -> t = "ml_mpz_ui_pow_ui"
  external powm : t -> t -> t -> t = "ml_mpz_powm"
  external powm_ui : t -> int -> t -> t = "ml_mpz_powm_ui"
  external sqrt : t -> t = "ml_mpz_sqrt"
  external sqrtrem : t -> t * t = "ml_mpz_sqrtrem"
  external perfect_square : t -> bool = "ml_mpz_perfect_square_p"
  external add_ui : t -> int -> t = "ml_mpz_add_ui"
  external sub_ui : t -> int -> t = "ml_mpz_sub_ui"
  external mul_ui : t -> int -> t = "ml_mpz_mul_ui"
  external neg : t -> t = "ml_mpz_neg"
  external abs : t -> t = "ml_mpz_abs"
  external mul2exp : t -> int -> t = "ml_mpz_mul2exp"
  external fac_ui : int -> t = "ml_mpz_fac_ui"
  external tdiv_q : t -> t -> t = "ml_mpz_tdiv_q"
  external tdiv_r : t -> t -> t = "ml_mpz_tdiv_r"
  external tdiv_qr : t -> t -> t * t = "ml_mpz_tdiv_qr"
  external fdiv_q : t -> t -> t = "ml_mpz_fdiv_q"
  external fdiv_r : t -> t -> t = "ml_mpz_fdiv_r"
  external fdiv_qr : t -> t -> t * t = "ml_mpz_fdiv_qr"
  external cdiv_q : t -> t -> t = "ml_mpz_cdiv_q"
  external cdiv_r : t -> t -> t = "ml_mpz_cdiv_r"
  external cdiv_qr : t -> t -> t * t = "ml_mpz_cdiv_qr"
  external tdiv_q_2exp : t -> int -> t = "ml_mpz_tdiv_q_2exp"
  external tdiv_r_2exp : t -> int -> t = "ml_mpz_tdiv_r_2exp"
  external fdiv_q_2exp : t -> int -> t = "ml_mpz_fdiv_q_2exp"
  external fdiv_r_2exp : t -> int -> t = "ml_mpz_fdiv_r_2exp"
  external tdiv_q_ui : t -> int -> t = "ml_mpz_tdiv_q_ui"
  external tdiv_r_ui : t -> int -> t = "ml_mpz_tdiv_r_ui"
  external tdiv_qr_ui : t -> int -> t * t = "ml_mpz_tdiv_qr_ui"
  external fdiv_q_ui : t -> int -> t = "ml_mpz_fdiv_q_ui"
  external fdiv_r_ui : t -> int -> t = "ml_mpz_fdiv_r_ui"
  external fdiv_qr_ui : t -> int -> t * t = "ml_mpz_fdiv_qr_ui"
  external cdiv_q_ui : t -> int -> t = "ml_mpz_cdiv_q_ui"
  external cdiv_r_ui : t -> int -> t = "ml_mpz_cdiv_r_ui"
  external cdiv_qr_ui : t -> int -> t * t = "ml_mpz_cdiv_qr_ui"
  external dmod : t -> t -> t = "ml_mpz_mod"
  external dmod_ui : t -> int -> t = "ml_mpz_mod_ui"
  external divexact : t -> t -> t = "ml_mpz_divexact"
  external cmp : t -> t -> int = "ml_mpz_cmp"
  external cmp_si : t -> int -> int = "ml_mpz_cmp_si"
  external sgn : t -> int = "ml_mpz_sgn"
  external band : t -> t -> t = "ml_mpz_and"
  external bxor : t -> t -> t = "ml_mpz_ior"
  external bnot : t -> t = "ml_mpz_com"
  external popcount : t -> int = "ml_mpz_popcount"
  external hamdist : t -> t -> int = "ml_mpz_hamdist"
  external scan0 : t -> int -> int = "ml_mpz_scan0"
  external scan1 : t -> int -> int = "ml_mpz_scan1"
  external is_probab_prime : t -> int -> bool = "ml_mpz_probab_prime_p"
  external is_perfect_square : t -> bool = "ml_mpz_perfect_square_p"
  external gcd : t -> t -> t = "ml_mpz_gcd"
  external gcdext : t -> t -> t * t * t = "ml_mpz_gcdext"
  external invert : t -> t -> t = "ml_mpz_invert"
  external jacobi : t -> t -> int = "ml_mpz_jacobi"
  external legendre : t -> t -> int = "ml_mpz_legendre"
  val setbit: t -> int -> t
  val clrbit: t -> int -> t

  module Infixes : sig
    external ( +! ) : t -> t -> t = "ml_mpz_add"
    external ( -! ) : t -> t -> t = "ml_mpz_sub"
    external ( *! ) : t -> t -> t = "ml_mpz_mul"    
    external ( %! ) : t -> t -> t = "ml_mpz_fdiv_r"
    val ( <!  ) : t -> t -> bool
    val ( <=! ) : t -> t -> bool
    val ( =!  ) : t -> t -> bool
    val ( >=! ) : t -> t -> bool
    val ( >!  ) : t -> t -> bool
    val ( <>! ) : t -> t -> bool
  end
end

module Q : sig
  type t
  external copy : t -> t = "ml_mpq_copy"
  external from_ints : int -> int -> t = "ml_mpq_from_ints"
  external from_z : Z.t -> t = "ml_mpq_from_z"
  external float_from : t -> float = "ml_float_from_mpq"
  external add : t -> t -> t = "ml_mpq_add"
  external sub : t -> t -> t = "ml_mpq_sub"
  external div : t -> t -> t = "ml_mpq_div"
  external mul : t -> t -> t = "ml_mpq_mul"
  external neg : t -> t = "ml_mpq_neg"
  external inv : t -> t = "ml_mpq_inv"
  external get_num : t -> Z.t = "ml_mpq_get_num"
  external get_den : t -> Z.t = "ml_mpq_get_den"
  external cmp : t -> t -> int = "ml_mpq_cmp"
  external sgn : t -> int = "ml_mpq_sgn"
  external equal : t -> t -> bool = "ml_mpq_equal"
  val from_zs : Z.t -> Z.t -> t

  module Infixes : sig
    external ( +/ ) : t -> t -> t = "ml_mpq_add"
    external ( -/ ) : t -> t -> t = "ml_mpq_sub"
    external ( */ ) : t -> t -> t = "ml_mpq_mul"    
    external ( // ) : t -> t -> t = "ml_mpq_div"
    val ( </  ) : t -> t -> bool
    val ( <=/ ) : t -> t -> bool
    external ( =/ ) : t -> t -> bool = "ml_mpq_equal"
    val ( >=/ ) : t -> t -> bool
    val ( >/  ) : t -> t -> bool
    val ( <>/ ) : t -> t -> bool
  end
end

module Q2 : sig
  type t = Q.t
  external copy : t -> t -> t = "ml_mpq2_copy"
  external from_ints : t -> int -> int -> t = "ml_mpq2_from_ints"
  external from_z : t -> Z.t -> t = "ml_mpq2_from_z"
  external add : t -> t -> t -> t = "ml_mpq2_add"
  external sub : t -> t -> t -> t = "ml_mpq2_sub"
  external div : t -> t -> t -> t = "ml_mpq2_div"
  external mul : t -> t -> t -> t = "ml_mpq2_mul"
  external neg : t -> t -> t = "ml_mpq2_neg"
  external inv : t -> t -> t = "ml_mpq2_inv"
  external get_num : Z.t -> t -> Z.t = "ml_mpq2_get_num"
  external get_den : Z.t -> t -> Z.t = "ml_mpq2_get_den"
  val from_zs : t -> Z.t -> Z.t -> t
end

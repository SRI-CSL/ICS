type rounding_mode = GMP_RNDN | GMP_RNDZ | GMP_RNDU | GMP_RNDD
module RNG :
  sig
    type randstate_t
    and randalg_t = GMP_RAND_ALG_LC of int
    val randinit : randalg_t -> randstate_t
    val default : randstate_t
  end
module Z2 :
  sig
    type t
    external from_int : dest:t -> int -> unit = "_mlgmp_z2_from_int"
    external from_string_base : dest:t -> base:int -> string -> unit
      = "_mlgmp_z2_from_string_base"
    external from_float : dest:t -> float -> unit = "_mlgmp_z2_from_float"
    external create : unit -> t = "_mlgmp_z_create"
    external copy : dest:t -> from:t -> unit = "_mlgmp_z_copy"
    external add : dest:t -> t -> t -> unit = "_mlgmp_z2_add"
    external sub : dest:t -> t -> t -> unit = "_mlgmp_z2_sub"
    external mul : dest:t -> t -> t -> unit = "_mlgmp_z2_mul"
    external tdiv_q : dest:t -> t -> t -> unit = "_mlgmp_z2_tdiv_q"
    external tdiv_r : dest:t -> t -> t -> unit = "_mlgmp_z2_tdiv_r"
    external cdiv_q : dest:t -> t -> t -> unit = "_mlgmp_z2_cdiv_q"
    external cdiv_r : dest:t -> t -> t -> unit = "_mlgmp_z2_cdiv_r"
    external fdiv_q : dest:t -> t -> t -> unit = "_mlgmp_z2_fdiv_q"
    external fdiv_r : dest:t -> t -> t -> unit = "_mlgmp_z2_fdiv_r"
    external divexact : dest:t -> t -> t -> unit = "_mlgmp_z2_divexact"
    external neg : dest:t -> t -> unit = "_mlgmp_z2_neg"
    external abs : dest:t -> t -> unit = "_mlgmp_z2_abs"
  end
module Z :
  sig
    type t = Z2.t
    external from_int : int -> t = "_mlgmp_z_from_int"
    external of_int : int -> t = "_mlgmp_z_from_int"
    external from_string_base : base:int -> string -> t
      = "_mlgmp_z_from_string_base"
    external from_float : float -> t = "_mlgmp_z_from_float"
    external of_float : int -> t = "_mlgmp_z_from_float"
    external to_string_base : base:int -> t -> string
      = "_mlgmp_z_to_string_base"
    external to_int : t -> int = "_mlgmp_z_to_int"
    external to_float : t -> float = "_mlgmp_z_to_float"
    external int_from : t -> int = "_mlgmp_z_to_int"
    external float_from : t -> float = "_mlgmp_z_to_float"
    external add : t -> t -> t = "_mlgmp_z_add"
    external sub : t -> t -> t = "_mlgmp_z_sub"
    external mul : t -> t -> t = "_mlgmp_z_mul"
    external add_ui : t -> int -> t = "_mlgmp_z_add_ui"
    external sub_ui : t -> int -> t = "_mlgmp_z_sub_ui"
    external mul_ui : t -> int -> t = "_mlgmp_z_mul_ui"
    external neg : t -> t = "_mlgmp_z_neg"
    external abs : t -> t = "_mlgmp_z_abs"
    external tdiv_qr : t -> t -> t * t = "_mlgmp_z_tdiv_qr"
    external tdiv_q : t -> t -> t = "_mlgmp_z_tdiv_q"
    external tdiv_r : t -> t -> t = "_mlgmp_z_tdiv_r"
    external cdiv_qr : t -> t -> t * t = "_mlgmp_z_cdiv_qr"
    external cdiv_q : t -> t -> t = "_mlgmp_z_cdiv_q"
    external cdiv_r : t -> t -> t = "_mlgmp_z_cdiv_r"
    external fdiv_qr : t -> t -> t * t = "_mlgmp_z_fdiv_qr"
    external fdiv_q : t -> t -> t = "_mlgmp_z_fdiv_q"
    external fdiv_r : t -> t -> t = "_mlgmp_z_fdiv_r"
    external dmod : t -> t -> t = "_mlgmp_z_mod"
    external dmod_ui : t -> int -> t = "_mlgmp_z_mod_ui"
    external euclidean_division : t -> t -> t * t = "_mlgmp_z_fdiv_qr"
    external modulo : t -> t -> t = "_mlgmp_z_mod"
    external tdiv_qr_ui : t -> int -> t * t = "_mlgmp_z_tdiv_qr_ui"
    external tdiv_q_ui : t -> int -> t = "_mlgmp_z_tdiv_q_ui"
    external tdiv_r_ui : t -> int -> t = "_mlgmp_z_tdiv_r_ui"
    external tdiv_ui : t -> int -> int = "_mlgmp_z_tdiv_ui"
    external cdiv_qr_ui : t -> int -> t * t = "_mlgmp_z_cdiv_qr_ui"
    external cdiv_q_ui : t -> int -> t = "_mlgmp_z_cdiv_q_ui"
    external cdiv_r_ui : t -> int -> t = "_mlgmp_z_cdiv_r_ui"
    external cdiv_ui : t -> int -> int = "_mlgmp_z_cdiv_ui"
    external fdiv_qr_ui : t -> int -> t * t = "_mlgmp_z_fdiv_qr_ui"
    external fdiv_q_ui : t -> int -> t = "_mlgmp_z_fdiv_q_ui"
    external fdiv_r_ui : t -> int -> t = "_mlgmp_z_fdiv_r_ui"
    external fdiv_ui : t -> int -> int = "_mlgmp_z_fdiv_ui"
    external divexact : t -> t -> t = "_mlgmp_z_divexact"
    external mul_2exp : t -> int -> t = "_mlgmp_z_mul_2exp"
    external mul2exp : t -> int -> t = "_mlgmp_z_mul_2exp"
    external tdiv_q_2exp : t -> int -> t = "_mlgmp_z_tdiv_q_2exp"
    external tdiv_r_2exp : t -> int -> t = "_mlgmp_z_tdiv_r_2exp"
    external fdiv_q_2exp : t -> int -> t = "_mlgmp_z_fdiv_q_2exp"
    external fdiv_r_2exp : t -> int -> t = "_mlgmp_z_fdiv_r_2exp"
    external cdiv_q_2exp : t -> int -> t = "_mlgmp_z_cdiv_q_2exp"
    external cdiv_r_2exp : t -> int -> t = "_mlgmp_z_cdiv_r_2exp"
    external powm : t -> t -> t -> t = "_mlgmp_z_powm"
    external powm_ui : t -> int -> t -> t = "_mlgmp_z_powm_ui"
    external pow_ui : t -> int -> t = "_mlgmp_z_pow_ui"
    external ui_pow_ui : int -> int -> t = "_mlgmp_z_ui_pow_ui"
    external pow_ui_ui : int -> int -> t = "_mlgmp_z_ui_pow_ui"
    external sqrt : t -> t = "_mlgmp_z_sqrt"
    external sqrtrem : t -> t * t = "_mlgmp_z_sqrtrem"
    external root : t -> int -> t = "_mlgmp_z_root"
    external perfect_power_p : t -> bool = "_mlgmp_z_perfect_power_p"
    external perfect_square_p : t -> bool = "_mlgmp_z_perfect_square_p"
    external is_perfect_power : t -> bool = "_mlgmp_z_perfect_power_p"
    external is_perfect_square : t -> bool = "_mlgmp_z_perfect_square_p"
    external probab_prime_p : t -> int -> bool = "_mlgmp_z_probab_prime_p"
    external is_probab_prime : t -> int -> bool = "_mlgmp_z_probab_prime_p"
    external nextprime : t -> t = "_mlgmp_z_nextprime"
    external gcd : t -> t -> t = "_mlgmp_z_gcd"
    external gcd_ui : t -> t -> t = "_mlgmp_z_gcd_ui"
    external lcm : t -> t -> t = "_mlgmp_z_lcm"
    external gcdext : t -> t -> t * t * t = "_mlgmp_z_gcdext"
    external inverse : t -> t -> t option = "_mlgmp_z_invert"
    external legendre : t -> int = "_mlgmp_z_legendre"
    external jacobi : t -> int = "_mlgmp_z_jacobi"
    external kronecker_si : t -> int -> int = "_mlgmp_z_kronecker_si"
    external si_kronecker : int -> t -> int = "_mlgmp_z_si_kronecker"
    external remove : t -> t -> t * int = "_mlgmp_z_remove"
    external fac_ui : int -> t = "_mlgmp_z_fac_ui"
    external fib_ui : int -> t = "_mlgmp_z_fib_ui"
    external bin_ui : n:t -> k:int -> t = "_mlgmp_z_bin_ui"
    external bin_uiui : n:int -> k:int -> t = "_mlgmp_z_bin_uiui"
    external cmp : t -> t -> int = "_mlgmp_z_compare"
    external cmp_si : t -> int -> int = "_mlgmp_z_compare_si"
    external compare : t -> t -> int = "_mlgmp_z_compare"
    external compare_si : t -> int -> int = "_mlgmp_z_compare_si"
    external compare_int : t -> int -> int = "_mlgmp_z_compare_si"
    external sgn : t -> int = "_mlgmp_z_sgn"
    external band : t -> t -> t = "_mlgmp_z_and"
    external bior : t -> t -> t = "_mlgmp_z_ior"
    external bxor : t -> t -> t = "_mlgmp_z_xor"
    external bcom : t -> t = "_mlgmp_z_com"
    external popcount : t -> int = "_mlgmp_z_popcount"
    external hamdist : t -> t -> int = "_mlgmp_z_hamdist"
    external scan0 : t -> int -> int = "_mlgmp_z_scan0"
    external scan1 : t -> int -> int = "_mlgmp_z_scan1"
    external urandomb : state:RNG.randstate_t -> nbits:int -> t
      = "_mlgmp_z_urandomb"
    external urandomm : state:RNG.randstate_t -> n:t -> t
      = "_mlgmp_z_urandomm"
    external rrandomb : state:RNG.randstate_t -> nbits:int -> t
      = "_mlgmp_z_rrandomb"
    val zero : t
    val one : t
    val is_prime : ?prec:int -> t -> bool
    val equal : t -> t -> bool
    val equal_int : t -> int -> bool
    val is_zero : t -> bool
    val to_string : t -> string
    val from_string : string -> t
    val string_from : t -> string
    val output : out_channel -> t -> unit
    val sprintf : unit -> t -> string

    val print : Format.formatter -> t -> unit
    val succ : t -> t
    val pred : t -> t
    val min : t -> t -> t
    val max : t -> t -> t

    module Infixes :
      sig
        external ( +! ) : t -> t -> t = "_mlgmp_z_add"
        external ( -! ) : t -> t -> t = "_mlgmp_z_sub"
        external ( *! ) : t -> t -> t = "_mlgmp_z_mul"
        external ( /! ) : t -> t -> t = "_mlgmp_z_fdiv_q"
        external ( %! ) : t -> t -> t = "_mlgmp_z_fdiv_r"
        val ( <! ) : t -> t -> bool
        val ( <=! ) : t -> t -> bool
        val ( =! ) : t -> t -> bool
        val ( >=! ) : t -> t -> bool
        val ( >! ) : t -> t -> bool
        val ( <>! ) : t -> t -> bool
      end
  end
module Q :
  sig
    type t
    external create : unit -> t = "_mlgmp_q_create"
    external from_z : Z.t -> t = "_mlgmp_q_from_z"
    external from_si : int -> int -> t = "_mlgmp_q_from_si"
    external from_ints : int -> int -> t = "_mlgmp_q_from_si"
    val from_int : int -> t
    external from_float : float -> t = "_mlgmp_q_from_float"
    external float_from : t -> float = "_mlgmp_q_to_float"
    external to_float : t -> float = "_mlgmp_q_to_float"
    external add : t -> t -> t = "_mlgmp_q_add"
    external sub : t -> t -> t = "_mlgmp_q_sub"
    external mul : t -> t -> t = "_mlgmp_q_mul"
    external div : t -> t -> t = "_mlgmp_q_div"
    external neg : t -> t = "_mlgmp_q_neg"
    external inv : t -> t = "_mlgmp_q_inv"
    external get_num : t -> Z.t = "_mlgmp_q_get_num"
    external get_den : t -> Z.t = "_mlgmp_q_get_den"
    external cmp : t -> t -> int = "_mlgmp_q_cmp"
    external compare : t -> t -> int = "_mlgmp_q_cmp"
    external cmp_ui : t -> int -> int -> int = "_mlgmp_q_cmp_ui"
    external sgn : t -> int = "_mlgmp_q_sgn"
    val zero : t
    val is_zero : t -> bool
    val from_zs : Z.t -> Z.t -> t
    val equal : t -> t -> bool
    val output : out_channel -> t -> unit
    val to_string : t -> string
    val sprintf : unit -> t -> string
    module Infixes :
      sig
        external ( +/ ) : t -> t -> t = "_mlgmp_q_add"
        external ( -/ ) : t -> t -> t = "_mlgmp_q_sub"
        external ( */ ) : t -> t -> t = "_mlgmp_q_mul"
        external ( // ) : t -> t -> t = "_mlgmp_q_div"
        val ( <! ) : t -> t -> bool
        val ( <=! ) : t -> t -> bool
        val ( =! ) : t -> t -> bool
        val ( >=! ) : t -> t -> bool
        val ( >! ) : t -> t -> bool
        val ( <>! ) : t -> t -> bool
      end
  end

exception Unimplemented of string
external get_gmp_runtime_version : unit -> string
  = "_mlgmp_get_runtime_version"
external get_gmp_compile_version : unit -> int * int * int
  = "_mlgmp_get_compile_version"

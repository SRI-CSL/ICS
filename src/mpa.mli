
(*s Multi-precision arithmetic. 
   This module abstract the necessary functions of any multi-precision
   package (Ocaml's bignums, GNU MP, etc.) *)

(*s Multi-precision integers. *)

module Z : sig

  type t

  val mult : t -> t -> t
  val divexact : t -> t -> t

  val equal : t -> t -> bool
  val compare : t -> t -> int
  val lt : t -> t -> bool
  val le : t -> t -> bool
  val gt : t -> t -> bool
  val ge : t -> t -> bool

  val of_int : int -> t
  val gcd : t -> t -> t
  val lcm : t -> t -> t
  val pow : int -> int -> t

  val pp : t -> unit
end

(*s Multi-precision rationals. *)

module Q : sig

  type t

  val zero : t
  val one : t
  val is_zero : t -> bool
  val is_one : t -> bool
  val is_negone : t -> bool
  val of_int : int -> t
  val of_ints : int -> int -> t

  val add : t -> t -> t
  val sub : t -> t -> t
  val minus : t -> t
  val mult : t -> t -> t
  val div : t -> t -> t
  val inv : t -> t

  val floor : t -> Z.t
  val ceil  : t -> Z.t

  val compare : t -> t -> int
  val equal : t -> t -> bool
  val lt : t -> t -> bool
  val le : t -> t -> bool
  val gt : t -> t -> bool
  val ge : t -> t -> bool

  type cmp = Equal | Greater | Less

  val cmp : t -> t -> cmp

  val denominator : t -> Z.t
      
  val is_integer : t -> bool
  val to_z : t -> Z.t
  val of_z : Z.t -> t
      
  val hash : t -> int

  val to_string : t -> string
  val of_string : string -> t

  val pp : t -> unit
end





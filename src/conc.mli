
(*s Concatenation normal forms are representations for bitvectors
    of the form [b1 ++ ... ++ bn] where [++] denotes
    concatenation and the [bi] are basic bitvectors.
    Basic bitvectors are
       \begin{enumerate}
       \item constant bitvectors [const(c)],
       \item extractions [sub(x,n,i,j)] of the [i]-th through the [j]-th bit
             of a term [x], or
       \item bitwise operations [ite(b1,b2,b3)], where [b1] is either a
             constant with not all bits zeros or ones, or an extraction;
             [b2] and [b3] are arbitrary basic bitvectors.
       \end{enumerate}
  *)

module type Var = sig
  type tnode
  type t = tnode Hashcons.hashed
  val fresh : unit -> t
end

module Make(X : Var) : sig

  type t

  val hom : (Bitv.t -> 'a)
               -> (X.t -> int -> int -> int -> 'a)
                  -> (int -> 'a -> 'a -> 'a -> 'a) -> t -> 'a list

  val length : t -> int

  val eps : t
  val zero : int -> t
  val one : int -> t
  val const : Bitv.t -> t
  
  val inj : int -> X.t -> t

  val sub : t -> int -> int -> t
    
  val (++) : t -> t -> t 
  val conc : t list -> t
  
  val ite : t -> t -> t -> t

      (*
  val bw_neg : t -> t
  val bw_conj : t -> t -> t
  val bw_disj : t -> t -> t
  val bw_xor : t -> t -> t
  val bw_imp : t -> t -> t
  val bw_iff : t -> t -> t

  val is_bw_neg : t -> bool
  val is_bw_conj : t -> bool
  val is_bw_disj : t -> bool
  val is_bw_xor : t -> bool
  val is_bw_imp : t -> bool
  val is_bw_iff : t -> bool

  val d_bw_neg : t -> t option
  val d_bw_conj : t -> (t * t) option
  val d_bw_disj : t -> (t * t) option
  val d_bw_xor : t -> (t * t) option
  val d_bw_imp : t -> (t * t) option
  val d_bw_iff : t -> (t * t) option
	*)
  
  val pp : (Format.formatter -> Term.t -> unit) -> Format.formatter -> t -> unit
  
  val solve : t -> t -> (X.t * t) list

end








(*i*)
open Mpa
(*i*)

val euclid : Q.t -> Q.t -> (Q.t * Q.t * Q.t)
    
val solve1 : Q.t list -> Q.t -> (Q.t * Q.t list) option
 
module type Ops =
  sig
    type t
    val num : Q.t -> t
    val fresh : unit -> t
    val ( + ) : t -> t -> t
    val ( - ) : t -> t -> t
    val ( * ) : t -> t -> t
  end

module type S =
  sig
    type t
    val solve: Q.t list -> Q.t -> (Q.t * t list) option
  end

module Make(R: Ops) : (S with type t = R.t)


type t = {
  ctxt : Term.t list;      (* current context. *)
  a : Arith.t;             (* Arithmetic *)
  t : Tuple.t;             (* Tuples. *)
  b : Bool.t;              (* Boolean part. *)
  u : Cc.t                 (* congruence closure data structure. *)
}

val empty : unit -> t

val copy : t -> t

val ext : t -> Term.t -> Term.ts

val cnstrnt : t -> Term.t -> Cnstrnt.t

val norm : Subst.t -> Term.t -> Term.t

val can : t -> Term.t -> Term.t

val simplify : t -> Term.t -> Term.t

val process : t -> Term.t -> t

type status = 
  | Valid 
  | Inconsistent 
  | Satisfiable of t list

val check : t -> Term.t -> status

val inconsistent : t -> bool

val groebner : t -> t

val witness : t -> Term.ts -> Subst.t list

val solutions : t -> Term.ts -> Subst.t list

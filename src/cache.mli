
val cache : int -> (Term.t -> 'a) -> (Term.t -> 'a)

val cache2 : int -> (Term.t * Term.t -> 'a) -> (Term.t * Term.t -> 'a)  
  
val cachel : int -> (Term.t list -> 'a) -> (Term.t list -> 'a)


(* Simplified version of caml types. *)



type t = {
  dom : ground list; 
  cod : ground 
}

and ground = 
  | Int 
  | Bool 
  | Unit 
  | String 
  | Value

val pp : Format.formatter -> t -> unit

val to_string : t -> string


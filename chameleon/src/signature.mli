
(* Simplified version of caml types. *)

module Dom 
  | Int 
  | Bool 
  | Unit 
  | String 
  | Value

type mlarity = {
  args : mltype list; 
  result : mltype 
}


(* Simplified version of caml types. *)

type mltype = Int | Bool | Unit | String | Abstract
type mlarity = { args : mltype list; result : mltype }

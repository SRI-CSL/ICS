
(* Simplified version of caml types. *)

type ground = 
  | Int 
  | Bool 
  | Unit 
  | String 
  | Value

let ground_to_string = function
  | Int -> "int"
  | Bool -> "bool"
  | Unit -> "unit"
  | String -> "string"
  | Value -> "value"

let ground_pp fmt t = 
  Format.fprintf fmt "%s" (ground_to_string t)


type t = {
  dom : ground list; 
  cod
 : ground
}

let pp fmt = function 
  | {dom = []; cod = c} ->
      Format.fprintf fmt "unit -> ";
      ground_pp fmt c
  | {dom = dl; cod = c} -> 
      List.iter (fun d -> ground_pp fmt d; Format.fprintf fmt " -> ") dl;
      ground_pp fmt c
    
let to_string x = 
  pp Format.str_formatter x;
  Format.flush_str_formatter ()

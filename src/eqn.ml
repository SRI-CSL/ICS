
(*i*)
open Hashcons
(*i*)

type t = Term.t * Term.t

let rec mem ((a,b) as e) = 
  function
    | [] -> false
    | (x,y) :: el ->
	((x === a && y === b) || (x === b && y === a)) && mem e el
  
let cons e el =
  if mem e el then el else e :: el

let rec (@@) el1 el2 =
  match el1 with
    | [] -> el2
    | e :: l -> cons e (l @@ el2)

let remove (a,b) =
  let rec rem =
    function 
      | [] -> []
      | ((x,y) as e) :: el ->
	  if (x === a && y === b) || (x === b && y === a) then
	    el
	  else 
	    e :: rem el
  in
  rem

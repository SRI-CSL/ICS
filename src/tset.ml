
(*i*)
open Term
(*i*)

type t = term_node Ptset.t
	   
let empty = Ptset.empty
	      
let add = Ptset.add

let singleton t = add t empty
	    
let is_empty = Ptset.is_empty
		 
let remove = Ptset.remove
	       
let mem = Ptset.mem
      
let iter = Ptset.iter
	     
let iter2 f s = iter (fun x -> iter (fun y -> f x y) s) s
		  
let fold = Ptset.fold

let union = Ptset.union

let filter p s =
  fold (fun x acc -> if p x then add x acc else acc) s empty

let inter s1 s2 = filter (fun x -> mem x s2) s1
			     
let to_list s = fold (fun x acc -> x :: acc) s []

	
exception SFound of term
		  
let choose p s =
  try
    iter (fun a -> if p a then raise (SFound a)) s;
    raise Not_found
  with
      SFound a -> a

exception Found

let exists p s =
  try
    iter (fun a -> if p a then raise Found) s;
    false
  with
      Found -> true

let sub s1 s2 =
  not (exists (fun x -> not (mem x s2)) s1)

let destructure s =
  let x = choose (fun _ -> true) s in
  x, remove x s
  






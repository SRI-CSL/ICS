
(*i*)
open Term
(*i*)

exception MFound of term * term

type 'a t = (term_node,'a) Ptmap.t
	      
let empty = Ptmap.empty
let add = Ptmap.add
let mem = Ptmap.mem
let find = Ptmap.find
let remove = Ptmap.remove
let map = Ptmap.map
let iter = Ptmap.iter
let fold = Ptmap.fold
	     
let choose p m =
  try
    iter (fun a b -> if p a b then raise (MFound (a,b))) m;
    raise Not_found
  with
      MFound (a,b) -> (a,b)


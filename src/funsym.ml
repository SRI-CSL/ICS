
(*i*)
open Term
open Hashcons
(*i*)

type tnode =
  | Uninterp of Term.t
  | Equal
  | Mult
  | Div

type t = tnode hashed 

(*s Hashconsing funsym constructors *)
      
module Hash = Hashcons.Make(
  struct 
    type t = tnode
    let equal t1 t2 =
      match t1, t2 with
	| Uninterp x1, Uninterp x2 -> x1 === x2
	| Equal, Equal -> true
	| Mult, Mult -> true
	| Div, Div -> true
	| _ -> false
    let hash = function
      | Uninterp x -> Hashtbl.hash x
      | Equal -> 3
      | Mult -> 5
      | Div -> 7
  end)

let hc : tnode -> t =
  let ht = Hash.create 107 in
  Tools.add_at_exit (fun () -> print_string "Funsyms: "; Hash.stat ht);
  Tools.add_at_reset (fun () -> Hash.clear ht);
  Hash.hashcons ht

let uninterp t = hc (Uninterp t)
let equal () = hc Equal
let mult () = hc Mult
let div () = hc Div

(*s Compute the function symbol of an uninterpreted term *)

let of_term t =
  match t.node with
    | Term.App(t,_) -> Some (uninterp t)
    | Term.Bool(Term.Equal _) -> Some (equal ())
    | Term.Arith(Term.Mult _) -> Some (mult ())
    | Term.Arith(Term.Div _) -> Some (div ())
    | _ -> None

let pp fmt f =
  match f.node with
  | Uninterp(t) ->
      Format.fprintf fmt "@[Uninterp("; Pretty.term fmt t; Format.fprintf fmt ")@]"
  | Equal ->
      Format.fprintf fmt "Equal"
  | Mult ->
      Format.fprintf fmt "Mult"
  | Div ->
      Format.fprintf fmt "Div"

(*s Maps with funsyms as domains *)

type funsym = t

module Map = struct
  type 'a t = (tnode,'a) Ptmap.t
  let empty = Ptmap.empty
  let add = Ptmap.add
  let mem = Ptmap.mem
  let find = Ptmap.find
  let remove = Ptmap.remove
  let map = Ptmap.map
  let iter = Ptmap.iter
  let fold = Ptmap.fold   
end

module Set = struct
  type elt = funsym 
  type t = tnode Ptset.t  
  let empty = Ptset.empty
  let add = Ptset.add
  let singleton t = add t empty
  let is_empty = Ptset.is_empty
  let remove = Ptset.remove
  let mem = Ptset.mem
  let cardinal = Ptset.cardinal
  let union = Ptset.union
  let iter = Ptset.iter
  let fold = Ptset.fold
end

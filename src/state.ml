
(*i*)
open Hashcons
open Term
open Subst
(*i*)

(*s We have the following databases:
    \begin{description}
    \item[find]: returns find for any variable/uninterp term (vterm).
    \item[use]: maps vterms to rhs terms in find that contain them interpreted.s
    \item[ext]: maps rhs to the set of lhs with rhs as find.
    \item[uninterp]: maps uninterp function symbols to
                     uninterpreted terms in the domain.
    \end{description}
 *)

type t = {
  mutable ctxt: Term.eqn list;
  mutable find: Subst.t;
  mutable inv: (Term.Set.t * Cnstrnt.t) Term.Map.t;    (* extension and constraint for each find. *)
  mutable use: Term.Set.t Term.Map.t;
  mutable uninterp: Term.Set.t Funsym.Map.t
}  

let empty = {
  ctxt = [];
  find = Subst.empty;
  inv = Term.Map.empty;
  use = Term.Map.empty;
  uninterp = Funsym.Map.empty
}

let copy s = {
  ctxt = s.ctxt;
  find = s.find;
  inv = s.inv;
  use = s.use;
  uninterp = s.uninterp
}

let to_subst s = s.find

(*s Accessors. *)

let ctxt_of s = s.ctxt
let find_of s = s.find 
let ext_of s = Term.Map.fold (fun x (l,_) -> Term.Map.add x l) s.inv Term.Map.empty
let cnstrnt_of s = Term.Map.fold (fun x (_,c) -> Term.Map.add x c) s.inv Term.Map.empty
let use_of s = s.use
let uninterp_of s = s.uninterp

		      
(*s Pretty-printing *)

let pp_ctxt fmt s =
  Pretty.list Pretty.eqn fmt s.ctxt
    
let pp_find fmt s =
  Subst.pp fmt s.find
		      
let pp_ext fmt s =
  Pretty.tmap Pretty.tset fmt (ext_of s)

		     
let pp_use fmt s =
  Pretty.tmap Pretty.tset fmt s.use
		 
let pp_uninterp fmt s =
  Funsym.Map.iter (fun f ts ->
		 Funsym.pp fmt f;
		 Format.fprintf fmt " |-> ";
		 Pretty.tset fmt ts;
		 Format.fprintf fmt " \n")
    s.uninterp

 
(*s Use structure *)

let mem s =
  Subst.mem s.find

let apply s a =
  Subst.apply s.find a
    
let find s a =
  Subst.find s.find a

let inv s a =
  assert (find s a === a);
  try
    Term.Map.find a s.inv
  with
      Not_found -> (Term.Set.empty,Cnstrnt.full)
    
let ext s a =
 (* assert (find s a === a); *)
  try
    let (el,_) = Term.Map.find a s.inv in
    el
  with
      Not_found -> Term.Set.empty

let cnstrnt s a =
  let from_ctxt x =
    let (_,c) = Term.Map.find (find s x) s.inv in c
  in
  Cnstrnt.cnstrnt from_ctxt a  
      
let use s a =
  try
    Term.Map.find a s.use
  with
      Not_found -> Term.Set.empty

let uninterp s f =
  try
    Funsym.Map.find f s.uninterp
  with
      Not_found -> Term.Set.empty

let dom s =
  Subst.fold (fun (x,_) acc -> x :: acc) s.find []

    
(*s Updating structures *)

let add_ctxt s e =
  s.ctxt <- e :: s.ctxt
 
let add_cnstrnt s c a =
  assert(a === find s a);
  let ext =
    try
      fst(Term.Map.find a s.inv)
    with
	Not_found -> Term.Set.empty
  in
  s.inv <- Term.Map.add a (ext,c) s.inv

let rec add_eqn s c (a,b) =
  update_old s c a;
  s.inv <- Term.Map.add b (Term.Set.add a (ext s b), c) s.inv;
  s.find <- Subst.add a b s.find;
  add_use s b;
  add_funsym s a

and update_old s c a =
  try
    let a' = Subst.apply s.find a in
    let ea' = ext s a' in
    let ea'' = Term.Set.remove a ea' in
    if Term.Set.is_empty ea'' then                     (* Not a find anymore. Delete from db. *)
      begin
	del_use s a';
	s.inv <- Term.Map.remove a' s.inv;
	del_funsym s a
      end
    else                                              (* Update extension and constraint. *)
      begin
	s.inv <- Term.Map.add a' (ea'',c) s.inv
      end
  with
      Not_found -> ()

and add_use s a =
  Term.iter_uninterpreted
    (fun t ->
       s.use <- Term.Map.add t (Term.Set.add a (use s t)) s.use)
    a

and del_use s a' =
  Term.iter_uninterpreted
    (fun t ->
       let uset' = Term.Set.remove a' (use s t) in
       if Term.Set.is_empty uset' then
	 s.use <- Term.Map.remove t s.use
       else
	 s.use <- Term.Map.add t uset' s.use)
    a'

and add_funsym s a =
  match Funsym.of_term a with
    | Some(f) ->
	s.uninterp <- Funsym.Map.add f (Term.Set.add a (uninterp s f)) s.uninterp
    | None -> ()

and del_funsym s a =
  match Funsym.of_term a with
    | Some(f) ->
	s.uninterp <- Funsym.Map.add f (Term.Set.remove a (uninterp s f)) s.uninterp
    | None -> ()





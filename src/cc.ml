
(*i*)
open Term
open Hashcons
(*i*)

type t = {
  mutable find : Subst.t;
  mutable use : Term.ts Term.Map.t;
  mutable cnstrnt : Cnstrnt.t Term.Map.t; 
  mutable diseqs : Term.ts Term.Map.t
}       

let empty () = {
  find = Subst.empty;
  use = Term.Map.empty;
  cnstrnt = Term.Map.empty;
  diseqs = Term.Map.empty
}

let copy s = {
  find = s.find;
  use = s.use;
  cnstrnt = s.cnstrnt;
  diseqs = s.diseqs
}                                                                            

let subst_of s = s.find
let use_of s = s.use
let cnstrnt_of s = s.cnstrnt
let diseqs_of s = s.diseqs

let apply s = Subst.apply s.find

let mem s = Subst.mem s.find

let rec find s a =
  try
    let a' = apply s a in
    if a === a' then a else find s a'
  with
      Not_found -> a

let use s a =
  try
    Term.Map.find a s.use
  with
      Not_found -> Term.Set.empty

let cnstrnt s a =
  try
    Term.Map.find a s.cnstrnt
  with
      Not_found -> Cnstrnt.top

let diseqs s a =
  try
    Term.Map.find a s.diseqs
  with
      Not_found -> Term.Set.empty

let sgn s a =
  match a.node with
    | App(f,l) ->
	App.sigma f (mapl (find s) l)
    | Var _ ->
	a
let eql s a =                 (* tests if term [a] equals the find of one of the terms. *)
  Term.Set.exists
    (fun x -> find s x === a)

let is_diseq s (a,b) =     (* tests if terms [a] and [b] are known to be disequal in state [s]. *)
  let a' = find s a in
  let b' = find s b in
  eql s a' (diseqs s b') || eql s b' (diseqs s a')


let cmp a b =                 (*s Force applications to be smaller than variables and constants. *)
  match a.node, b.node with   (*s Also, [cmp] makes sure that e.g. [f(f(f(x))) = f(x)] is added in this order. *)
    | (Var _ | App(_,[])), App(_,_::_) -> 1
    | App(_,_::_),  (Var _ | App(_,[])) -> -1
    | _ -> Term.cmp b a

let union s (a,b) =
  let update (x,y) =
    let c = Cnstrnt.inter (cnstrnt s x) (cnstrnt s y) in
    let dx = diseqs s x in
    let dy = diseqs s y in
    if eql s x dy || eql s y dx then
      raise Exc.Inconsistent
    else 
      let d = Term.Set.union dx dy in
      match Cnstrnt.analyze c with
	| Cnstrnt.Full -> 
	    s.find <- Subst.add (x,y) s.find;
	    if not(Term.Set.is_empty d) then
              s.diseqs <- Term.Map.update y d (Term.Map.remove x s.diseqs);
	| Cnstrnt.Empty ->
	    raise Exc.Inconsistent
	| _ -> 
	    s.cnstrnt <- Term.Map.update y c (Term.Map.remove x s.cnstrnt);
	    if not(Term.Set.is_empty d) then
	      s.diseqs <- Term.Map.update y d (Term.Map.remove x s.diseqs);
	    s.find <- Subst.add (x,y) s.find 
	
  in
  if cmp a b <= 0 then 
    update (a,b)
  else 
    update (b,a)

let universe s a =
  Term.Set.exists            
    (fun x ->             
       a === sgn s x) 
    (use s a)

let is_congruent s a b =
  let eq x y =
    find s x === find s y
  in
  match a.node, b.node with
    | App(f1,l1), App(f2,l2) 
	when f1 === f2 ->
	  (try 
	     List.for_all2 eq l1 l2
	   with 
	       Invalid_argument _ -> false)
    | _ ->
	false

let modify = ref true

let add_use s a b =                (* add [a] to the use of [b] *)
  if !modify then
    let useb = use s b in
    let useb' = Term.Set.add a useb in
    if not(useb == useb') then 
      s.use <- Term.Map.add b useb' s.use

      
let merge s e =
  let eqs = ref [] in              (* newly generated equalities. *)
  let rec loop ((a,b) as e) =
    if not(a === b) then
      begin
	union s e;
	eqs := e :: !eqs;
	Term.Set.iter 
	  (fun u ->
	     Term.Set.iter 
	       (fun v ->
		  begin
		    if is_congruent s u v then
		      let u' = find s u in
		      let v' = find s v in
		      Trace.call 6 "Cc" (u',v') Pretty.eqn;
		      loop (u', v');
		    add_use s u b
		  end)
	       (use s b))
	  (use s a)
      end
  in
  loop e;
  !eqs

let rec canon s a =
  match a.node with
    | Var _ ->
	canonsig s a
    | App(f,l) ->
	canonsig s (App.sigma (canonsym s f) (mapl (canon s) l))

and canonsig s a =
  match a.node with
    | Var _ | App(_,[]) ->
	find s a
    | App(f, (x1 :: _ as l)) ->
	try
	  find s (Term.Set.choose                 (* Term [a] has already been processed, so *)
		    (fun u ->                     (* just return its find. *)
		       a === sgn s u) 
		    (use s x1))
	with
	    Not_found ->
	      begin
		List.iter (add_use s a) l;
		a
	      end

and canonsym s f =                                 (*s Canonize 'function symbols'. *)
  match f.node with
    | Uninterp(x,d,p) ->
	mk_uninterp_sym d p (canon s x)
    | _ ->
	f

let extend s a =
  Trace.call 9 "Ext(e)" a Pretty.term;
  modify := true;
  match a.node with
    | App(_, l) ->
	List.iter (add_use s a) l
    | _ -> ()
	
let add_eqn s ((a,b) as e) =
  Trace.call 4 "Add(e)" e Pretty.eqn;
  modify := true;
  merge s (canon s a, canon s b)

let add_eqns s el =
  if el <> [] then Trace.call 5 "Add(e)" el (Pretty.list Pretty.eqn);
  List.fold_left (fun acc e -> add_eqn s e @ acc) [] el

let find s a =
  modify := false;
  canon s a

let cnstrnt s a =      (* get constraint of canonical term. *)
  try
    Term.Map.find (find s a) s.cnstrnt
  with
      Not_found -> Cnstrnt.top

let add_cnstrnt s (c,a) =
  Trace.call 7 "Add(c)" a Pretty.term;
  let a' = find s a in
  let c' = cnstrnt s a' in
  let d = Cnstrnt.inter c c' in
  match Cnstrnt.analyze d with
    | Cnstrnt.Empty ->
	raise Exc.Inconsistent
    | Cnstrnt.Full ->
	[]
    | Cnstrnt.Singleton(q) ->
	[(a,Arith.num q)]
    | _ ->
	s.cnstrnt <- Term.Map.update a' d s.cnstrnt;
        []

let add_cnstrnts s =
  List.fold_left (fun acc x -> add_cnstrnt s x @ acc) []

let add_diseq s (a,b) =
  let update x y =        (* add [y] to the disequalities of [x]. *)
    let dx = diseqs s x in
    if not(Term.Set.exists (fun z -> find s z === y) dx) then  (* don't add two terms with the same find. *)
      let dx' = Term.Set.add y dx in
      if not(dx == dx') then
	s.diseqs <- Term.Map.update x dx' s.diseqs
  in      
  let a' = find s a in
  let b' = find s b in
  if a' === b' then
    raise Exc.Inconsistent
  else 
    begin  
      update a' b';      (* install [b'] into the disequalities of [a'] and vice versa. *)
      update b' a'
    end
  
let add_diseqs s =
  List.iter (add_diseq s)


(*s Set of equivalent terms by recursing over the set of bindings in [s]. *)

let ext s a =
  let a' = find s a in
  Subst.fold
    (fun (x,y) acc ->
       let x' = find s x in
       let y' = find s y in
       match x' === a', y' === a' with
	 | true, true ->
	     Term.Set.add x (Term.Set.add y acc)
	 | true, false -> 
	     Term.Set.add x acc
	 | false, true ->
	     Term.Set.add y acc
	 | false, false ->
	     acc)
    (subst_of s)
    Term.Set.empty

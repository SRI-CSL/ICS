
(*i*)
open Term
open Hashcons
open Binrel
open Eqn
(*i*)

(*s Decision procedure state. *)

type t = {
  ctxt: Term.t list;      (* current context. *)
  a : Arith.t;            (* Arithmetic *)
  t : Tuple.t;            (* Tuples. *)
  b : Bool.t;             (* Boolean part. *)
  u: Cc.t                 (* congruence closure data structure; including constraints and disequalities. *)
}

let empty () = {
  ctxt = [];
  a = Arith.empty ();
  t = Tuple.empty ();
  b = Bool.empty ();
  u = Cc.empty ()
}

let copy s = {
  ctxt = s.ctxt;
  a = Arith.copy s.a;
  t = Tuple.copy s.t;
  b = Bool.copy s.b;
  u = Cc.copy s.u
}

(*s Purify. *)

let abst slack th s a =
  assert(not(is_var a));
  let update (x,a) =              (* insert [x = a] into the theory database [th]. *)
    match th with
      | ArithTh ->
	  if slack then
	    (match Arith.qsolve (x,a) with    (*s Solve for largest variables. This *)
	       | Some(y,z) ->                 (*s ensures that newly introduced slack *)
		   Arith.extend s.a (y,z)     (* variables move to the lhs, since slack *)
	       | None -> ())                  (* vars are smaller than external vars. *)
	  else 
	    Arith.extend s.a (x,a)
      | TupleTh -> 
	  Tuple.extend s.t (x,a)
      | BooleanTh -> 
	  Bool.extend s.b (x,a)
      | EqTh -> 
	  failwith "Process.abst: attempt to purify an uninterpreted term"
  in
  try
    name_of a
  with
      Not_found ->
	let x = mk_rename_var "u" a in
	update (x,a);
	x

(*s [purify s a] returns a purified term, i.e. a term with function symbols from
  one theory only. For example, [f(x+1)] is purified to [f(u)] with the side effect
  that the fresh variable [u] is introduced in the arithmetic state as [u |-> x + 1].
  In addition, the domain of the congruence closure state is extended to contain all
  uninterpreted subterms. *)

let purify s a =              (* if [k] is None, then [a] is not abstracted. *)
  Trace.call 9 "Purify" a Pretty.term;
  let abst = abst false in
  let rec pure k b =            (* purify term [b] in the context of theory [k] *)
    match b.node with
      | Var _ | App(_,[]) ->
	  b
      | App(f,l) ->
	  (match f.node with
	     | Interp(Arith _) ->
		 let k' = Some(ArithTh) in
		 let b' = mk_app f (mapl (pure k') l) in
		 if k = k' || k = None then b' else abst ArithTh s b'
	     | Interp(Tuple _) ->
		 let k' = Some(TupleTh) in
		 let b' = mk_app f (mapl (pure k') l) in
		 if k = k' || k = None then b' else abst TupleTh s b'
	     | Pred _ | Builtin _ ->
		 let k' = Some(EqTh) in
		 mk_app f (mapl (pure k') l)   (* purify e.g. [x+1] in [x+1 <> y]. *)
	     | Uninterp(x,d,p) ->
		 let f' = mk_uninterp_sym d p (pure k x) in
		 let k' = Some(EqTh) in
		 let a' = mk_app f' (mapl (pure k') l) in
                 Cc.extend s.u a';            (* extend congruence closure domain. *)
		 a'
	     | Interp(Bool(Ite)) ->
		 let k' = Some(BooleanTh) in
                 let b' = mk_app f (mapl (pure k') l) in
		 if k = k' || k = None then b' else abst BooleanTh s b'
	     | _ ->
		 failwith "Process.purify: invalid argument")
  in
  let b = pure None a in
  Trace.exit 9 "Purify" b Pretty.term;
  b

and slackify s a =
  if Arith.is_arith a then
    abst true ArithTh s a
  else 
    a
  

  

(*s Canonizer Always returns the canonical variable for a term when it exists.
  \begin{tabular}{lll}
      [can s x]  & = & [v s x]      & , the canonical variable equivalent to [x]. \\
      [can s f(a)] & = & [can_var(sigma th (f(can_th th s a)))] &,                
  \end{tabular}
  where [th] is the theory of [f], [can_th th s a] is just [find_th s (can s a)],
  and [can_var s a] returns the canonical variable equivalent to [a], if there is one,
  and [a], otherwise. *)

let defreshify = ref false

let rec can s a =
  Trace.call 4 "Can" a Pretty.term;
  defreshify := false;
  let b = can_term s a in 
  Trace.exit 4 "Can" b Pretty.term;
  b

and can_external s a =
  Trace.call 4 "Norm" a Pretty.term;
  defreshify := true;
  let b = can_term s a in 
  Trace.exit 4 "Norm" b Pretty.term;
  b

and can_term s a =
  match a.node with
    | App(f,l) ->
	(match f.node, l with
	   | Interp(Bool(Ite)), [x;y;z] ->
	       can_ite s (x,y,z)
	   | Interp(op), _ -> 
	       let find_th =
		 match op with
		   | Arith _ -> Arith.find s.a
		   | Tuple _ -> Tuple.find s.t  
		   | Bool _ -> Bool.find s.b
	       in
	       let l' = mapl (fun x -> find_th (can_term s x)) l in
	       let a' = App.sigma f l' in
	       can_var s a' 
	   | Pred(p), _ ->
	       can_atom s p l
	   | _ ->
	       Cc.find s.u a)
    | _ ->
	if !defreshify && is_rename_var a then           (* canonize definition of rename variables. *)
	  let (_,b) = d_rename_var a in
	  can_term s b
	else 
	  Cc.find s.u a

and can_var s a =
  match a.node with
    | App({node=Interp(f)},_) ->
	let inv_th x = 
	  try
	    match f with
	      | Arith _ -> Arith.inv s.a x
	      | Bool _ -> Bool.inv s.b x
	      | Tuple _ -> Tuple.inv s.t x
	  with
	      Not_found -> x
	in
	Cc.find s.u (inv_th a)
    | _ ->
	Cc.find s.u a

and can_atom s p l =
  match p, l with
    | Cnstrnt(c), [x]  ->
	can_cnstrnt s c (can_term s x)
    | Equal, [x;y] ->
	can_equal s (can_term s x, can_term s y)
    | _ -> 
	failwith "Can.atom: fatal error"

and can_equal s (a,b) =
  let a' = can_term s a in
  let b' = can_term s b in
  if a' === b' then 
    Bool.tt()
  else 
    Atom.equal (a',b')
  
and can_diseq s (a,b) = 
  let a' = can_term s a in
  let b' = can_term s b in
  if a' === b' then
    Bool.ff()
  else if Cc.is_diseq s.u (a',b') then
    Bool.tt()
  else 
    Atom.diseq (a',b')

and can_cnstrnt s c a =
  let a' = can_term s a in
  let c' = cnstrnt s a' in
  match Cnstrnt.cmp c c' with 
    | (Same | Super) -> Bool.tt()
    | Disjoint -> Bool.ff()
    | Sub -> Atom.cnstrnt c a'
    | Overlap -> Atom.cnstrnt (Cnstrnt.inter c c') a'

and can_ite s (a,b,c) =
  let a' = can s a in
  try
    let _ = process s a' in
    Bool.ite (a', can s b, can s c)
  with
    | Exc.Valid -> can s b
    | Exc.Inconsistent -> can s c


(*s Compute the best known constraint for a given term. *)

and cnstrnt s a =
  let ctxt = Cc.cnstrnt s.u in
  let c1 = Cnstrnt.of_term ctxt a in
  let c2 = Arith.cnstrnt ctxt s.a a in
  Cnstrnt.inter c1 c2

(*s Processing an atom. *)

and process s a =
  Trace.call 1 "Process" a Pretty.term;
  if is_tt a then 
    raise Exc.Valid
  else if is_ff a then 
    raise Exc.Inconsistent
  else if is_equal a then
    process_eqn s (d_equal a)
  else if is_cnstrnt a then
    process_cnstrnt s (d_cnstrnt a)
  else 
    process_eqn s (a, Bool.tt());


(*s Processing of an equality. Assumes that the arguments 
    [a] and [b] are canonized. If [s |- a = b] is detected, 
    then [eqn s (a,b)] throws the exception [Exc.Valid], and
    if [s |- a <> b] is established, then [Exc.Inconsistent] is
    raised. Otherwise, an extended state [s1] for the context
    [s, a = b] is returned. *)

and process_eqn s (a,b) =
  let s = copy s in
  let ((a,b) as e) = (purify s a, purify s b) in
  if Arith.is_linarith a || Arith.is_linarith b then   
    let (el1,cl1) = Arith.process (cnstrnt s) s.a e in
    let el2 = Cc.add_cnstrnts s.u cl1 in
    propagate s (el1 @@ el2)
  else if Tuple.is_tuple a || Tuple.is_tuple b then
    propagate s (Tuple.process s.t e)
  else if Bool.is_bool a ||  Bool.is_bool b then
    let (el,dl,cl) = Bool.add_eqn s.b e in
    let el1 = Cc.add_cnstrnts s.u cl in
    Cc.add_diseqs s.u dl;
    propagate s (el @@ el1)
  else 
    propagate s [e];
  s

and process_cnstrnt s (c,a) =
   let s = copy s in
   let a = purify s a in
   let b = slackify s a in
   let el = Cc.add_cnstrnt s.u (c,b) in
   propagate s el;
   s

and propagate1 s el = 
  Trace.call 1 "Propagate" el (Pretty.list Pretty.eqn);
  let el1 = Cc.add_eqns s.u el in
  let (el2,cl2) = Arith.propagate (cnstrnt s) s.a el1 in
  let el3 = Cc.add_cnstrnts s.u cl2 in
  let el4 = Tuple.propagate s.t el1 in
  let (el5,dl5,cl5) = Bool.add_eqns s.b el1 in
  let el6 = Cc.add_cnstrnts s.u cl5 in
  let _ =  Cc.add_diseqs s.u dl5 in
  let el7 = el1 @@ el2 @@ el3 @@ el4 @@ el5 @@ el6 in
  Trace.exit 1 "Propagate" el7 (Pretty.list Pretty.eqn);
  el7
  
and propagate s el =
  let el1 = propagate1 s el in
  if el1 = [] then () else propagate s el1


(*s Check status of a formula. *)

type status = 
  | Valid 
  | Inconsistent 
  | Satisfiable of t list

let rec check s a =
  if is_ite a then
    check_ite s (d_ite a)
  else 
    let a' = can s a in
    if is_tt a' then Valid
    else if is_ff a' then Inconsistent
    else Satisfiable([s])

and check_ite s (a,b,c) =
  let a' = can s a in
  if is_tt a' then 
    check s b
  else if is_ff a' then
    check s c
  else 
    try
      let s' = process s a' in
      (try 
	 let s'' = process s (Bool.neg a') in
	 match check s' b, check s'' c with
	   | Valid, Valid -> Valid
	   | Inconsistent, Inconsistent -> Inconsistent
	   | Satisfiable(sl1), Satisfiable(sl2) -> Satisfiable(sl1 @ sl2)
	   | _, Satisfiable(sl2) -> Satisfiable(sl2)
	   | Satisfiable(sl1), _ -> Satisfiable(sl1)
	   | Inconsistent, Valid -> Satisfiable([s'])
	   | Valid, Inconsistent -> Satisfiable([s'])
       with
	 | Exc.Valid -> check s c
	 | Exc.Inconsistent -> check s b)
    with
      | Exc.Valid -> check s b
      | Exc.Inconsistent -> check s c


(*s Check for inconsistencies. *)

let inconsistent s =
  check s (Bool.term_of s.b) = Inconsistent

(*s Groebner completion. *)

let groebner s =
  let s' = copy s in
  let ctxt = cnstrnt s' in
  let (el,cl) = Arith.groebner ctxt s'.a in
  let el1 = Cc.add_cnstrnts s'.u cl in
  propagate s' el1;
  s'

(*s Set of equivalent terms (incomplete.) *)

let ext s a =
  Term.Set.union 
    (Cc.ext s.u a)
    (Arith.ext s.a a)


(*s Normalization. *)

let rec norm rho a =
  match a.node with
    | App(f,l) ->
	App.sigma f (mapl (norm rho) l)
    | Var(_,Rename(x)) ->
	norm rho x
    | _ ->
	(try 
	   let b = Subst.apply rho a in
	   assert(not(a===b));
	   norm rho b
	 with
	     Not_found -> a)


(*s Computation of witnesses. *)

let solutions s xs =
  let solution1 x =             (* Set of all terms in equivalence class of [x] *)
    Term.Set.filter            (* which do not contain any of the [xs]. *)
      (fun y -> 
	 not(Term.Set.exists (fun z -> is_subterm z y) xs))
      (ext s x)
  in
  Term.Set.fold 
    (fun x acc -> 
       let ys = solution1 x in
       if Term.Set.is_empty ys then
	 []
       else 
	 (x,ys) :: acc)
    xs []
    

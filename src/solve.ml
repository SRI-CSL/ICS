
(*i*)
open Term
open Hashcons
(*i*)

let is_solvable a =
  match a.node with
    | Var _
    | App _
    | Update _
    | Bool(Equal _)
    | Arith(Mult _) 
    | Arith(Div _)
    | Bv(BvToNat _)
    | Set(Finite _)
    | Set(Cnstrnt _) ->
	true
    | _ ->
	false

let arith_solve x s (a,b) =
  let add_cnstrnt =
    State.add_cnstrnt s Interval.int
  in
  if State.is_int s a
    && State.is_int s b
  then
    let (kl,rho) = Arith.zsolve (a,b) in
    List.iter add_cnstrnt kl;
    rho
  else
    Arith.qsolve x (a,b)

let solve x s e =
  let rec solvel rho el =
    match el with
    | [] -> rho
    | (a,b) :: el -> 
	Trace.call 5 "Solve(rec)" (a,b) Pretty.eqn;
	let rho' = solve1 rho (a,b) el in
	Trace.exit 5 "Solve(rec)" rho' Subst.pp;
	rho'

  and solve_equal rho (a1,a2) b el =              (*s Solve equations of the form [(a1 = a2) = b] *)
    match b.node with
      | Bool(True) ->
	  solvel rho ((a1,a2) :: el)
      | Bool(False) ->
	  solve_diseq rho (a1,a2) el
      | Bool(Equal(b1,b2)) ->
	  let c1 = State.cnstrnt s b1 in
	  let c2 = State.cnstrnt s b2 in
	  if Interval.is_disjoint c1 c2 then
	    raise Exc.Inconsistent;
	  let rho' = Subst.add (Bool.equal(a1,a2), b) rho in
	  solvel rho' el
      | Bool(Ite _) ->
	  let a = Bool.equal(a1,a2) in
	  if Term.occurs_interpreted a b then
	    let x = Bool.iff a b in
	    let l = Bool.solve x in
	    solvel rho (l @ el)
	  else
	    solvel (Subst.add (a,b) rho) el
      | _ ->
	  solvel (Subst.add (b, Bool.equal(a1,a2)) rho) el

  and solve_diseq rho (a,b) el =                (* Solve disequalities [a <> b]. *)
    match a.node, b.node with
      | Arith(Num q1), Arith(Num q2) ->
	  if Mpa.Q.equal q1 q2 then
	    raise Exc.Inconsistent
	  else
	    solvel rho el
      | Arith(Num q1), _ ->
	  State.add_cnstrnt s (Interval.diseq q1) b;
	  solvel rho el
      | _, Arith(Num q2) ->
	  State.add_cnstrnt s (Interval.diseq q2) a;
	  solvel rho el
      | _ ->
	  solvel (Subst.add (Bool.equal(a,b), Bool.ff()) rho) el

  and bool_solve rho (a,b) el =
    match a with
      | True ->
	  true_solve rho b el
      | False ->
	  false_solve rho b el
      | Equal(x,y) ->
	  solve_equal rho (x,y) b el
      | Ite _ ->
	  let x = Bool.iff (hc(Bool(a))) b in
	  let l = Bool.solve x in
	  solvel rho (l @ el)
      | _ ->
	  assert false

  and true_solve rho b el =      (*s Solve equations of the form [true = b] *)  
    match b.node with
      | Bool(True) -> 
	  solvel rho el
      | Bool(False) ->
	  raise Exc.Inconsistent
      | Bool(Equal(x,y)) ->
	  solvel rho ((x,y) :: el)
      | Bool(Ite _) ->
	  solvel rho (Bool.solve b @ el)
      | App({node=Set(Cnstrnt(c))}, [x]) ->
	  cnstrnt_solve rho c x el
      | _ ->
	  solvel (Subst.add (b,Bool.tt()) rho) el

  and false_solve rho b el =  (*s Solve equations of the form [false = b] *)
    match b.node with
      | Bool(False) ->
	  solvel rho el
      | Bool(True) ->
	  raise Exc.Inconsistent
      | Bool(Equal(x,y)) ->
	  solve_diseq rho (x,y) el
      | App({node=Set(Cnstrnt(c))}, [x]) ->
	  cnstrnt_solve rho (Interval.compl c) x el
      | Bool(Ite _) ->
	  let x = Bool.neg b in
	  solvel rho (Bool.solve x @ el)
      | _ ->
	  solvel (Subst.add (b,Bool.ff()) rho) el 
	
  and cnstrnt_solve rho c x el =               (* solve equalities of the form [(x in c) = true] *)
    let d = State.cnstrnt s x in
    match Interval.cmp d c with
      | Binrel.Sub | Binrel.Same -> (*i [x in c] holds always. i*)
	  solvel rho el
      | Binrel.Disjoint ->          (*i [x in c] never holds. i*)
	  raise Exc.Inconsistent
      | Binrel.Super
      | Binrel.Overlap ->
	  let cd = Interval.inter c d in
	  if Interval.is_singleton cd then   (*i infer new equality i*)
	    let n = Interval.value_of cd in
	    solvel rho ((x,Arith.num n) :: el)
	  else if Arith.is_arith x then
	    let k = Var.fresh "z" (Some(x)) in
	    State.add_cnstrnt s cd k;
	    solvel rho ((x,k) :: el)         (*i Order [(x,k)] important. i*)
	  else
	    begin
	      State.add_cnstrnt s c x;
	      solvel rho el
	    end

  and cond_solve rho (x,y,z) b el =
    let a = Bool.ite(x, Bool.equal(y,b), Bool.equal(z,b)) in
    solvel rho ((a, Bool.tt()) :: el)
    
  and solve1 rho (a,b) el =
	let a' = Subst.norm rho a and b' = Subst.norm rho b in
	if a' === b' then
	  solvel rho el
	else if is_ground a' && is_ground b' then
	  raise Exc.Inconsistent
	else
	  let ca' = State.cnstrnt s a' in
	  let cb' = State.cnstrnt s b' in
	  let c = Interval.inter ca' cb' in
	  if Interval.is_bot c
	  then     
	    raise Exc.Inconsistent
	  else if
	    Interval.is_singleton c
	    && not(Arith.is_num a')
	    && not(Arith.is_num b')
	  then 
	    let n = Arith.num(Interval.value_of c) in
	    solvel rho ((a',n) :: (b',n) :: el)
	  else 
	    (match a'.node, b'.node with
	       | Bool(Equal(x,y)), Bool(True) ->
		   solvel rho ((x,y) :: el)
	       | Bool(True), Bool(Equal(x,y)) ->
		   solvel rho ((x,y) :: el)  
	       | App({node=Set(Cnstrnt c)},[x]), Bool(True) ->
		   cnstrnt_solve rho c x el
	       | App({node=Set(Cnstrnt c)},[x]), Bool(False) ->
		   cnstrnt_solve rho (Interval.compl c) x el 
	       | _ ->
		   if is_solvable a' &&
		     not(Term.occurs_interpreted a' b')
		   then
		     solvel (Subst.add (a',b') rho) el
		   else (match a'.node,b'.node with
			   | Bool x, _ ->
			       bool_solve rho (x,b) el
			   | _, Bool y ->
			       bool_solve rho (y,a) el
			   | Update (u,i,s), Update (v,j,t)
			       when u === v && i === j ->
				 if u === v && i === j
				 then
				   solvel rho ((s,t) :: el)
				 else
				   solvel (Subst.add (a',b') rho) el
			   | Cond(x,y,z), _ ->
			       cond_solve rho (x,y,z) b' el
			   | _, Cond(x,y,z) ->
			       cond_solve rho (x,y,z) b' el
			   | Arith(Num _ | Multq _ | Add _), _ ->
			       solvel rho (arith_solve x s (a,b) @ el)
			   | _, Arith(Num _ | Multq _ | Add _) ->
			       solvel rho (arith_solve x s (b,a) @ el)
			   | Set _, _ ->
			       solvel rho (Sets.solve 0 (a,b) @ el)
			   | _, Set _ ->
			       solvel rho (Sets.solve 0 (b,a) @ el)
			   | Tuple _, _ ->
			       solvel rho (Tuple.solve (a,b) @ el)
			   | _, Tuple _ ->
			       solvel rho (Tuple.solve (b,a) @ el)
			   | Bv _, _ ->
			       solvel rho (Tuple.solve (a,b) @ el)
			   | _, Bv _ ->
			       solvel rho (Bv.solve (b,a) @ el)
			   | _ ->
			       failwith "Incompleteness in solver; to be fixed..."))
		
  in
  solvel Subst.empty [e]


let solve x s ((a,b) as e) =
  Trace.call 3 "Solve" e Pretty.eqn;
  try
    let rho = solve x s e in
    Trace.exit 3 "Solve" rho Subst.pp;
    rho
  with
      Exc.Inconsistent ->
	Trace.exc 3 "Solve" e Pretty.eqn;
	raise Exc.Inconsistent


(*s Compute a solution for [a] given [s]. *)
    
let rec solution s a =
  if State.mem s a then
    Some(State.apply s a)
  else
    try
      let p x =                                 (*s choose lhs [b] such that [a] occurs interpreted *)
	not(Term.Set.is_empty(State.ext s x))   (*s in [b] and the extension of [b] is nonempty. *)
      in
      let b = Term.Set.choose p (State.use s a) in   
      let x = Term.Set.choose (fun x -> true) (State.ext s b) in
      Some(unwind a (x,b))
    with
	Not_found ->
	  None

and  unwind x (a,b) =
  if a === x then
    b
  else
    match b.node with
      | Arith(Multq _ | Add _) ->
	  (match Arith.qsolve (Some x) (a,b) with
	     | [a',b'] ->
		 unwind x (a',b')
	     | _ ->
		 raise Not_found)
      | Bool(Ite _ ) ->
	  let l = Bool.solve (Bool.iff a b) in
	  let b' = List.assq a l in
	  unwind x (a,b')
      | Tuple _ ->
	  let l = Tuple.solve (a,b) in
	  let b' = List.assq a l in
	  unwind x (a,b')
      | Set _ ->
	  let l = Sets.solve 0 (a,b) in
	  let b' = List.assq a l in
	  unwind x (a,b')
      | Bv _ ->
	  let l = Bv.solve (a,b) in
	  let b' = List.assq a l in
	  unwind x (a,b')
      | _ ->
	  raise Not_found
	
	
 










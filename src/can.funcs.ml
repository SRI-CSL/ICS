

(*

  let replace map s rhos =
    let lookup y = 
      try
	let b, tau = apply s y in
	  rhos := Jst.dep2 tau !rhos; b
      with
	  Not_found -> y
    in
      map lookup
	

	
  (** {i Reduce} of [a] in configuration [S]
    - [S^-1(x) = x].   
    - [S^-1(a) = x] if [x = a] in [s].
    - [S^-1(f(a1, ...,an)) = S^-1(f(S^-1(a1), ..., S^-1(an)))] 
    if there is [ai =/= S^-1(ai)].
    - [S^-1(a)] = a otherwise. *)
  let reduce (map, sigma) s rhos =
    let rec red a =
      if Term.is_var a then a else
	try
	  let (x, rho) = inv s a in
	    rhos := Jst.dep2 rho !rhos; x
	with
	  Not_found -> 
	    let al = Term.args_of a in
	    let al' = Term.mapl red al in
	      if al == al' then a else 
		red (sigma (Term.sym_of a) al')
    in
      red


module Update(Args: ARGS) = struct

  let extend (x, b, rho) s = 
    assert(not(in_dom s x));
    Map.set x (Cod.singleton (b, rho)) s.find;
    Term.iter (fun y -> Dep.add x y s.dep) b
      
  let add (x, a, rho) s =
    assert(Term.is_var x);
    assert(is_flat a);  
    try
      let z, tau = inv s a in          (* [tau |- z = a]. *)
	assert(Term.is_var z);
	V.Infsys.process_equal (x, z, tau)
    with
	Not_found -> 
	  let cod' =
	    try
	      let cod = Map.find x s.find in
		Cod.add (a, rho) cod
	    with
		Not_found -> Cod.singleton (a, rho)
	  in
	    Map.set x cod' s.find;
	    Term.iter (fun y -> Dep.add x y s.dep) a
	      
  let remove (x, a, rho) s =
    try
      let cod = Map.find x s.find in
	if Cod.mem (a, rho) cod then
	  let cod' = Cod.remove (a, rho) cod in
	    if Cod.is_empty cod' then
	      begin
		Map.remove x s.find;
		Term.iter (fun y -> Dep.remove x y s.dep) a
	      end 
	    else
	      begin
		Map.set x cod' s.find;
		failwith "to do"
	      end 
	else 
	  ()
    with
	Not_found -> ()


  let rec instantiate ((x, y, _) as e) s = 
    collapse e s;
    fuse e s

  and fuse ((x, y, rho) as e) s =                  (* [rho |- x = y]. *)
  assert(Term.is_var x);
    assert(Term.is_var y);
    let inst_right z = 
      assert(in_dom s z);
      assert(Term.is_var x && Term.is_var y); 
      let lookup z = if x == z then y else z in
	try
	  let es = Map.find z s.find in
	  let es' = ref es in
	  Cod.iter
	    (fun (e, tau) ->                       (* [tau |- z = e]. *)
	       assert(is_flat e);
	       if Term.subterm x e then
		 let e' = Args.map lookup e in     (* ==> [rho |- e = e']. *)
		   assert(is_flat e');
		   let sigma = Jst.dep2 rho tau in
		     es' := Cod.add (e', sigma) (Cod.remove (e, tau) !es');
		     try
		       let (z', ups) = inv s e' in  (* [ups |- z' = e']. *)
			 if not(Term.eq z z') then
			   deduce (z, z', Jst.dep2 sigma ups)
		     with
			 Not_found -> ())
	    es;
	    Map.set z !es' s.find;
	    Dep.remove z x s.dep;
	    if Cod.exists (fun (e', _) -> (Term.subterm y e')) !es' then  
	      Dep.add z y s.dep         
	with
	    Not_found -> invalid_arg "U.fuse: not a domain variable"
    in
      if not(Term.eq x y) then
	Dep.Set.iter inst_right (dep s x)
	  
  and deduce e =
    V.Infsys.process_equal e
      
  (** For [x = a], [y = b] replace [x = b] with [y = b]. *)
  and collapse ((x, y, rho) as e) s =  (* [rho |- x = y]. *) 
    try
      let as' = Map.find x s.find in
	(try
	  let bs' = Map.find y s.find in
	  let abs' = ref bs' in
	    Cod.iter
	      (fun ((a, tau) as e) ->    (* [tau |- x = a] *)
		 let e' = (a, Jst.dep2 rho tau) in
		   abs' := Cod.add e' !abs';
		   Term.iter (fun z -> Dep.replace x y z s.dep) a)
	      as';
	    Map.remove x s.find;
	    Map.set y !abs' s.find
	with
	    Not_found -> 
	      Map.set y as' s.find;
	      Cod.iter
		(fun ((a, _) as e) ->
		   Term.iter (fun z -> Dep.replace x y z s.dep) a)
		(failwith "to do"))
    with
	Not_found -> ()
	      
  (** Flatten a pure term [a] and introduce variables as
    necessary for naming subterms. The result is a variable [z]
    equal to [a] in the extended context. *)
  let varify s rhos =
    let rec var_of_term a =
      if Term.is_var a then a else
	try
	  let x, rho = inv s a in
	    assert(Term.is_var x);
	    rhos := Jst.dep2 rho !rhos;
	    x
	with
	    Not_found -> 
	      let al = Term.args_of a in
	      let bl = Term.mapl var_of_term al in
	      assert(List.for_all Term.is_var bl);
		let v = Term.mk_fresh_var "v" in
		let b = if al == bl then a else Args.can (Term.sym_of a) bl in
		  extend (v, b, Jst.dep0) s;
		  v
    in
      var_of_term
      *)

  *)

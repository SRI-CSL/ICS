(*
 * The contents of this file are subject to the ICS(TM) Community Research
 * License Version 2.0 (the ``License''); you may not use this file except in
 * compliance with the License. You may obtain a copy of the License at
 * http://www.icansolve.com/license.html.  Software distributed under the
 * License is distributed on an ``AS IS'' basis, WITHOUT WARRANTY OF ANY
 * KIND, either express or implied. See the License for the specific language
 * governing rights and limitations under the License.  The Licensed Software
 * is Copyright (c) SRI International 2003, 2004.  All rights reserved.
 * ``ICS'' is a trademark of SRI International, a California nonprofit public
 * benefit corporation.
 *)

open Combine

module Status = struct

  type t = 
    | Sat of Term.Model.t
    | Unsat of Judgement.unsat   
    | Unknown
	
  let pp fmt = function
    | Unknown -> 
	Format.fprintf fmt "ok"
    | Sat(mdl) ->  
	Format.fprintf fmt "sat(";
	Term.Model.pp fmt mdl;
	Format.fprintf fmt ")@;"
    | Unsat(bot) -> 
	Format.fprintf fmt "unsat(";
	Judgement.pp fmt bot;
	Format.fprintf fmt ")@;"

end 

type t = {
  ctxt : Atom.t list;         (* Logical context. *)
  config : Config.t;          (* Inference system configuration. *)
  upper : int;                (* Upper bound on fresh variable index. *)
  mutable status: Status.t
}

let empty = {
  ctxt = [];
  config = Config.empty ();
  upper = 0;
  status = Status.Unknown
}

let is_empty s = (s.ctxt = [])

let eq s1 s2 =
  s1.ctxt == s2.ctxt
  
let ctxt s = s.ctxt

let config s = s.config
let status s = s.status

let pp fmt s =
  Format.fprintf fmt "@[<ctxt=}";
  Pretty.set Atom.pp fmt (ctxt s);
  Format.fprintf fmt ";@,status=";
  Status.pp fmt s.status;
  Format.fprintf fmt ">@]@;"


let initialize s = 
  Infsys.initialize s.config;
  Term.k := s.upper

let finalize ctxt' = { 
  ctxt = ctxt';
  config = Infsys.finalize ();
  upper = !Term.k;
  status = Status.Unknown 
}

module Axioms = struct

 class virtual top = (object(self)
   method virtual concl : Atom.t
   method hyps = Judgement.mk_empty
   method name = "axiom"
   method assumptions acc = Atom.Set.add self#concl acc
   method pp fmt = 
     Format.fprintf fmt "@[%s |- " self#name; 
     Atom.pp fmt self#concl;
     Format.fprintf fmt "@]@;"
   method validate = true
 end)

 class equal (t1: Term.t) (t2: Term.t) = (object
   inherit top
   method lhs = t1
   method rhs = t2
   method concl = Atom.Equal(t1, t2)
  end : Judgement.equal)

 let mk_equal t1 t2 = new equal t1 t2

 class diseq (t1: Term.t) (t2: Term.t) = (object
   inherit top
   method lhs = t1
   method rhs = t2
   method concl = Atom.Diseq(t1, t2)
  end : Judgement.diseq)

 let mk_diseq t1 t2 = new diseq t1 t2

 class nonneg (t: Term.t) = (object
   inherit top
   method arg = t
   method concl = Atom.Nonneg(t)
  end : Judgement.nonneg)

 let mk_nonneg t = new nonneg t

 class pos (t: Term.t) = (object
   inherit top
   method arg = t
   method concl = Atom.Pos(t)
  end : Judgement.pos)

 let mk_pos t = new pos t

 class cnstrnt (t: Term.t) (c: Cnstrnt.t) = (object
   inherit top
   method arg = t
   method cnstrnt = c
   method concl = Atom.Cnstrnt(t, c)
  end : Judgement.cnstrnt)

 let mk_cnstrnt t c = new cnstrnt t c

  class unsat  = (object
   inherit top
   method concl = Atom.mk_false
  end : Judgement.unsat)

 let mk_unsat = new unsat
		  
end

let add_equal s t1 t2 =  
  let can = Config.Can.term s.config in
  let t1' = can t1 and t2' = can t2 in
    if Term.eq t1' t2' then 
      let e1 = Config.Can.justify s.config t1
      and e2 = Config.Can.justify s.config t2 in
      let e:>Judgement.atom = Judgement.mk_join e1 e2 in
	raise(Judgement.Valid(e))
    else
      let ax = Axioms.mk_equal t1' t2' in
      let e = ax in (* to do *)
	initialize s;
	Infsys.process_equal e;
	finalize (Atom.mk_equal t1 t2 :: s.ctxt)

let add_diseq s t1 t2 =  
  let t1' = Config.Can.term s.config t1
  and t2' = Config.Can.term s.config t2 in
    if Config.Diseq.test s.config t1 t2 then 
      let d:>Judgement.atom = Config.Diseq.justify s.config t1 t2 in
	raise(Judgement.Valid(d))
    else
      let d = Axioms.mk_diseq t1 t2 in
	initialize s;
	Infsys.process_diseq d;
	finalize (Atom.mk_diseq t1 t2 :: s.ctxt)

let add_cnstrnt s t c =  
  let t' = Config.Can.term s.config t in
    if Config.Cnstrnt.test s.config t' c then 
      let cc:>Judgement.atom = Config.Cnstrnt.justify s.config t c in
	raise(Judgement.Valid(cc))
    else
      let cc = Axioms.mk_cnstrnt t c in
	initialize s;
	Infsys.process_cnstrnt cc;
	finalize (Atom.mk_cnstrnt t c :: s.ctxt)

let add_nonneg s t =  
  let t' = Config.Can.term s.config t in
    if Config.Nonneg.test s.config t' then 
      let nn:>Judgement.atom = Config.Nonneg.justify s.config t in
	raise(Judgement.Valid(nn))
    else
      let nn = Axioms.mk_nonneg t in
	initialize s;
	Infsys.process_nonneg nn;
	finalize (Atom.mk_nonneg t :: s.ctxt)

let add_pos s t =  
  let t' = Config.Can.term s.config t in
    if Config.Pos.test s.config t' then 
      let pp:>Judgement.atom = Config.Pos.justify s.config t in
	raise(Judgement.Valid(pp))
    else
      let pp = Axioms.mk_pos t in
	initialize s;
	Infsys.process_pos pp;
	finalize (Atom.mk_pos t :: s.ctxt)

let add s a = 
  match a with
    | Atom.TT -> s
    | Atom.FF -> raise(Judgement.Unsat(Axioms.mk_unsat))
    | Atom.Equal(t1, t2) -> add_equal s t1 t2
    | Atom.Diseq(t1, t2) -> add_diseq s t1 t2
    | Atom.Nonneg(t) -> add_nonneg s t
    | Atom.Pos(t) -> add_pos s t
    | Atom.Cnstrnt(t, c) -> add_cnstrnt s t c


let addl s al =
  let ctxt' = ref s.ctxt in
  let rec processl = function
    | [] -> ()
    | a :: al ->                                      (* [p |- a => b]. *)
	failwith "addl: to do"
  in
    initialize s;
    processl al;
    finalize !ctxt'

let resolve s =
  match s.status with
    | Status.Sat _ -> ()
    | Status.Unsat _ -> ()
    | Status.Unknown -> 
	try
	  Infsys.initialize s.config;
	  Term.k := s.upper;
	  let mdl = Infsys.model () in
	    s.status <- Status.Sat(mdl)
	with
	    Judgement.Unsat(bot) -> 
	      s.status <- Status.Unsat(bot)
    
let is_inconsistent s atml =
  try
    let _ = addl s atml in
      None
  with
      Judgement.Unsat(bot) -> Some(bot)
	
let is_valid s a =
  try
    let _ = add s a in
      None
  with
    | Judgement.Valid(j) -> Some(j)
    | Judgement.Unsat _ -> None

let validates s atm =
  resolve s;
  match s.status with
    | Status.Unsat _ -> true
    | Status.Unknown -> raise Exc.Incomplete
    | Status.Sat(mdl) -> not(Atom.is_false (Atom.eval mdl atm))

let eval s atm =
  resolve s;
  match s.status with
    | Status.Unsat _ -> Atom.mk_true
    | Status.Unknown -> raise Exc.Incomplete
    | Status.Sat(mdl) -> Atom.eval mdl atm
	
let rec model s =
  resolve s;
  match s.status with
    | Status.Sat(mdl) -> externalize s mdl
    | Status.Unsat(rho) -> raise(Judgement.Unsat(rho))
    | Status.Unknown -> raise Exc.Incomplete

and externalize s (i, alpha) =
  failwith "externalize: to do"
(*
  let dom = dom s.ctxt in
  let v = s.config.Config.v in
  let find x = fst (V.find v x) in
  let can = Term.map find in
  let alpha' =
    Term.Vset.fold
      (fun x acc -> 
	 try
	   let vs = Term.Assign.apply alpha (can x) in
	     failwith "to do"
	 with
	     Not_found -> acc)
      dom Term.Assign.empty
  in
  let i' = failwith "to do" in
    (i', alpha')  
*)
	 
and dom atms =
  let vars = Term.Set.empty () in
    List.iter
      (fun atm -> 
	 Term.Set.union (Atom.vars_of atm) vars)
      atms;
    vars

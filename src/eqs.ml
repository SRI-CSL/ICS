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

(** The theory-specific equality sets are represented using a map [th |-> ei]
  with [th] the theory specificier of type {!Th.t} and [ei] the theory-specific
  solution set. The type of [ei] depends on the name [th], so we use the artificial
  type [top] to encode such dependent maps. *)
module Eqs = struct

  let theories = ref Theory.Set.empty 

  type eqs

  type t = eqs Theory.Map.t

  let empty () = Theory.Map.empty

  module Methods = struct
    type 'a t = 'a Theory.Map.t ref

    let is_empty: (eqs -> bool) t = ref Theory.Map.empty
    let pp = ref Theory.Map.empty
    let inv: (eqs -> Jst.Eqtrans.t) t = ref Theory.Map.empty
    let apply: (eqs -> Jst.Eqtrans.t) t = ref Theory.Map.empty
    let dep: (eqs -> Term.t -> Dep.Set.t) t = ref Theory.Map.empty
    let mem: (Term.t -> eqs -> bool) t = ref Theory.Map.empty
    let model: (eqs -> Term.Model.t) t = ref Theory.Map.empty
  end
			
  let is_empty i e = 
    try
      let ei = Obj.magic (Theory.Map.find i e) in
      let is_empty_method = Theory.Map.find i !Methods.is_empty in
	 is_empty_method ei
    with
	Not_found -> false

  let pp i fmt e =
    try
      let ei = Obj.magic (Theory.Map.find i e) in
      let pp_method = Theory.Map.find i !Methods.pp in
	pp_method fmt ei
    with
	Not_found -> ()

  let inv i e =
    let ei = Obj.magic (Theory.Map.find i e) in
    let inv_method = Theory.Map.find i !Methods.inv in
      inv_method ei
 
  let apply i e (x: Term.t) =
    let ei = Obj.magic (Theory.Map.find i e) in
    let apply_method = Theory.Map.find i !Methods.apply in
      apply_method ei x
	  
  let find i e x =
    try apply i e x with Not_found -> Jst.Eqtrans.id x

  let dep i e x =
    try
      let ei = Obj.magic (Theory.Map.find i e) in
      let dep_method = Theory.Map.find i !Methods.dep in
	dep_method ei x
    with
	Not_found -> Dep.Set.empty()

  let mem i x e =
    try
      let ei = Obj.magic (Theory.Map.find i e) in
      let mem_method = Theory.Map.find i !Methods.mem in
	mem_method x ei
    with
	Not_found -> false

  let model i e =
    try
      let ei = Obj.magic (Theory.Map.find i e) in
      let model_method = Theory.Map.find i !Methods.model in
	model_method ei
    with
	Not_found -> Term.Model.empty

  let theories () = !theories

end

module type INFSYS = Infsys.IS
module type EQS = Infsys.EQS

module Infsys = struct

  let theories = ref Theory.Set.empty

  module Eqs = struct
    type t = Eqs.t
    and eqs = Eqs.eqs
    let empty = Theory.Map.empty
    let is_empty e = (Theory.Map.empty == e)
    let pp fmt = failwith "to do"
    let apply e = failwith "to do"
    let inv e = failwith "to do"
    let dep e = failwith "to do"
    let mem e = failwith "to do"
    let model e = failwith "to do"
    let iter e = failwith "to do"
    let fold e = failwith "to do"
    let occurs e = failwith "to do" 
  end


  module Methods = struct
    let reset = ref Theory.Map.empty 
    let current = ref Theory.Map.empty
    let initialize = ref Theory.Map.empty
    let is_unchanged = ref Theory.Map.empty
    let finalize = ref Theory.Map.empty
    let abstract = ref Theory.Map.empty
    let process_equal = ref Theory.Map.empty
    let process_diseq = ref Theory.Map.empty
    let process_ineq = ref Theory.Map.empty
    let propagate_equal = ref []
    let propagate_diseq = ref []
    let propagate_cnstrnt = ref []
    let propagate_nonneg = ref []
    let normalize = ref Theory.Map.empty
  end 
			      
  module Component = struct

    let reset i = 
      Theory.Map.find i !Methods.reset ()

    let initialize e i = 
      try 
	let init = Theory.Map.find i !Methods.initialize in
	let ei = Theory.Map.find i e in
	  init ei
      with 
	  Not_found -> reset i

    let current i =  
      Theory.Map.find i !Methods.current ()

    let is_unchanged i = 
      Theory.Map.find i !Methods.is_unchanged () 

    let finalize i = 
      Theory.Map.find i !Methods.finalize ()

    let normalize i = 
      Theory.Map.find i !Methods.normalize ()
    
  end

  let reset () =
    Theory.Set.iter Component.reset !theories
	
  let initialize e =
    Theory.Set.iter (Component.initialize e) !theories
	
  let current () =
    let add i =  
      let ei: Eqs.eqs = Obj.magic (Component.current i) in
	Theory.Map.add i ei
    in
      Theory.Set.fold add !theories Theory.Map.empty
	
  let is_unchanged () = 
    Theory.Set.for_all Component.is_unchanged !theories
      
  let finalize () = 
    let add i =  
      let ei: Eqs.eqs = Obj.magic (Component.finalize i) in
	Theory.Map.add i ei
    in
      Theory.Set.fold add !theories Theory.Map.empty
	
  let abstract i a fct =
    assert(Term.is_pure i a);
    G.put fct;
    Theory.Map.find i !Methods.abstract a
      
  let process_equal i e =
    try
      Theory.Map.find i !Methods.process_equal e
    with
	Not_found -> invalid_arg ("No equality processor for " ^ (Theory.to_string i))
	
  let process_diseq i d =
    try
      Theory.Map.find i !Methods.process_diseq d
    with
	Not_found -> invalid_arg ("No disequality processor for " ^ (Theory.to_string i))

  let process_ineq i n =
    try
      Theory.Map.find i !Methods.process_ineq n
    with
	Not_found -> invalid_arg ("No inequality processor for " ^ (Theory.to_string i))
	  
  let propagate_equal x =
    let propagate_x (th, f) = f x in
      List.iter propagate_x !Methods.propagate_equal
      
  let propagate_diseq d = 
    let propagate_d f = f d in
      List.iter propagate_d !Methods.propagate_diseq
	
  let propagate_cnstrnt x = 
    let propagate_x f = f x in
      List.iter propagate_x !Methods.propagate_cnstrnt
	
  let propagate_nonneg x = 
    let propagate_x f = f x in
      List.iter propagate_x !Methods.propagate_nonneg
	
  let normalize () =
      Theory.Set.iter Component.normalize !theories

end 


module type TH = sig val th: Theory.t end


module Register(T: TH)(I: INFSYS) = struct

  let th = T.th

  let _ = 
    if Theory.Set.mem th !Infsys.theories then
      invalid_arg (Format.sprintf "Theory %s already registered." (Theory.to_string T.th))
    else 
      Infsys.theories := Theory.Set.add T.th !Infsys.theories

  let current () =  
    let (ei: Eqs.eqs) = Obj.magic (I.current()) in 
      ei

  let initialize (ei: Eqs.eqs) = 
    I.initialize (Obj.magic ei)

  let finalize () =  
    let (ei: Eqs.eqs) = Obj.magic (I.finalize()) in 
      ei

  open Infsys

  let _ = 
    Methods.reset := Theory.Map.add th I.reset !Methods.reset;
    Methods.current := Theory.Map.add th current !Methods.current;
  (*  Methods.initialize := Theory.Map.add th initialize !Methods.initialize; *)
    Methods.is_unchanged := Theory.Map.add th I.is_unchanged !Methods.is_unchanged;
    Methods.finalize := Theory.Map.add th finalize !Methods.finalize;
    Methods.abstract := Theory.Map.add th I.abstract !Methods.abstract;
    Methods.normalize := Theory.Map.add th I.normalize !Methods.normalize;
    Methods.process_equal := Theory.Map.add th I.process_equal !Methods.process_equal;
    Methods.process_diseq := Theory.Map.add th I.process_diseq !Methods.process_diseq;
    Methods.process_ineq := Theory.Map.add th I.process_ineq !Methods.process_ineq;
  (*  Methods.propagate_equal := Theory.Map.add th I.propagate_equal !Methods.propagate_equal;
    Methods.propagate_diseq := Theory.Map.add th I.propagate_diseq !Methods.propagate_diseq; 
    Methods.propagate_cnstrnt := Theory.Map.add th I.propagate_cnstrnt !Methods.propagate_cnstrnt;
    Methods.propagate_nonneg := Theory.Map.add th I.propagate_nonneg !Methods.propagate_nonneg
  *)

(*
  let _ = 
    if Version.debug() <= 2 then () else ()
      Format.eprintf "\nRegistering: %s" (Theory.to_string th)

*)
end

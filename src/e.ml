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

type eqtrans = Term.t -> Term.t * Judgement.equal

module type CONFIG = sig
  type t
  val empty : unit -> t
  val is_empty : t -> bool
  val pp : Format.formatter -> t -> unit
  val apply : t -> eqtrans
  val inv : t -> eqtrans
  val dep : t -> Term.t -> Dep.Set.t
  val occ : Term.t -> t -> bool
  val model : t -> Term.Model.t
end

module type INFSYS = sig
  type eqs
  val current : unit -> eqs
  val reset : unit -> unit
  val initialize : eqs -> unit
  val is_unchanged : unit -> bool
  val finalize : unit -> eqs
  val abstract : Term.t -> Judgement.atom -> unit
  val process_equal : (Judgement.equal -> unit) option
  val process_diseq : (Judgement.diseq -> unit) option 
  val process_nonneg : (Judgement.nonneg -> unit) option
  val process_pos : (Judgement.pos -> unit) option
  val propagate_equal : (Term.t -> unit) option
  val propagate_diseq : (Judgement.diseq -> unit) option 
  val propagate_cnstrnt : (Term.t -> unit) option 
  val propagate_nonneg : (Term.t -> unit) option
  val branch : unit -> Judgement.disjunction option
  val normalize : unit -> unit
end


module type COMPONENT = sig
  val th : Theory.t
  module Eqs : CONFIG
  module Infsys : (INFSYS with type eqs = Eqs.t)
end


module Eqs = struct

  type t = eqs Theory.Map.t

  and eqs

  let empty () = Theory.Map.empty

  let is_empty e = (Theory.Map.empty == e)

  module Component = struct

    let out = Theory.Map.find

    let call (i: Theory.t) ms = Theory.Hash.find ms i

    module Methods = struct
      module Table = Theory.Hash
      let is_empty: (eqs -> bool) Table.t = Table.create 11
      let pp : (Format.formatter -> eqs -> unit) Table.t = Table.create 11
      let apply: (eqs -> eqtrans) Table.t = Table.create 11
      let inv : (eqs -> eqtrans) Table.t = Table.create 11
      let dep : (eqs -> Term.t -> Dep.Set.t) Table.t = Table.create 11
      let occ : (Term.t -> eqs -> bool) Table.t = Table.create 11
      let model : (eqs -> Term.Model.t) Table.t = Table.create 11
      let call (i: Theory.t) ms = Theory.Hash.find ms i
    end

    module M = Methods

    let add i (ei: eqs) (e: t) =
      if call i M.is_empty ei then e else 
	Theory.Map.add i ei e

    let is_empty i e = call i M.is_empty (out i e)
    let pp i fmt e = call i M.pp fmt (out i e)
    let dep i e = call i M.dep (out i e)
    let occ i x e = call i M.occ x (out i e)
    let model i e = call i M.model (out i e)

    module Apply = struct
      module Methods = struct
	let get = Theory.Hash.create 11
	let justify = Theory.Hash.create 11
      end 
      let get i = Theory.Hash.find Methods.get i
      let justify i = Theory.Hash.find Methods.justify i
    end 
    module Inv = struct
      module Methods = struct
	let get = Theory.Hash.create 11
	let justify = Theory.Hash.create 11
      end 
      let get i = call i Methods.get
      let justify i = call i Methods.justify
    end 
    module Replace = struct
      module Methods = struct
	let get = Theory.Hash.create 11
	let justify = Theory.Hash.create 11
      end 
      let get i = call i Methods.get
      let justify i = call i Methods.justify
    end 
    module Diseq = struct
      module Methods = struct
	let test = Theory.Hash.create 11
	let justify = Theory.Hash.create 11
      end 
      let test i = call i Methods.test
      let justify i = call i Methods.justify
    end
    module Equal = struct
      module Methods = struct
	let test = Theory.Hash.create 11
	let justify = Theory.Hash.create 11
      end 
      let test i = call i Methods.test
      let justify i = call i Methods.justify
    end
    module Nonneg = struct
      module Methods = struct
	let test = Theory.Hash.create 11
	let justify = Theory.Hash.create 11
      end 
      let test i = call i Methods.test
      let justify i = call i Methods.justify
    end
    module Pos = struct
      module Methods = struct
	let test = Theory.Hash.create 11
	let justify = Theory.Hash.create 11
      end 
      let test i = call i Methods.test
      let justify i = call i Methods.justify
    end
    
  end

  let iter = Theory.Map.iter

  let pp fmt =
    iter
      (fun i ei ->
	 Format.fprintf fmt "@[%s:" (Theory.to_string i);
	 Component.call i Component.Methods.pp fmt ei;
	 Format.fprintf fmt "@]@.")

  let model e =
    Theory.Map.fold
      (fun i ei -> 
	 Term.Model.combine 
	    (Component.call i Component.Methods.model ei))
      e Term.Model.empty

end

(** Abstract interface of an {i inference system} for equality theories.
  Such an inference system operates on configurations [(g, e, v)] with
  - [g] the global inputs (see module {!G}),
  - [e] a set of equalities of type {!Infsys.CONFIG}, and
  - [v] a set of variable equalities, disequalities, and other constraints (see module {!V}). *)
module Infsys = struct

  module Methods = struct
    module Table = Theory.Hash
    let reset: (unit -> unit) Table.t = Table.create 11
    let current : (unit -> Eqs.eqs) Table.t = Table.create 11
    let initialize = Table.create 11
    let is_unchanged: (unit -> bool) Table.t = Table.create 11
    let finalize = Table.create 11
    let abstract: (Term.t -> Judgement.atom -> unit) Table.t = Table.create 11
    let process_equal: (Judgement.equal -> unit) Table.t = Table.create 11
    let process_diseq: (Judgement.diseq -> unit) Table.t  = Table.create 11
    let process_nonneg: (Judgement.nonneg -> unit) Table.t = Table.create 11
    let process_pos: (Judgement.pos -> unit) Table.t = Table.create 11
    let propagate_equal: (Term.t -> unit) Table.t = Table.create 11
    let propagate_diseq: (Judgement.diseq -> unit) Table.t = Table.create 11
    let propagate_cnstrnt: (Term.t -> unit) Table.t = Table.create 11
    let propagate_nonneg: (Term.t -> unit) Table.t  = Table.create 11
    let branch: (unit -> Judgement.disjunction option) Table.t = Table.create 11
    let normalize: (unit -> unit) Table.t = Table.create 11
    let call (i: Theory.t) m = Table.find m i
    let call_all m x = Table.iter (fun _ f -> f x) m
  end
    
  let reset () =
    Methods.call_all Methods.reset ()

  let initialize (e: Eqs.t) =
    Eqs.iter 
      (fun i -> 
	 Methods.call i Methods.initialize)
      e
    
  let finalize () =
    Methods.Table.fold
      (fun i fin acc ->
	 let ei = fin () in
	   Eqs.Component.add i ei acc)
      Methods.finalize
      (Eqs.empty())

  let current () =
    Methods.Table.fold
      (fun i fin acc ->
	 let ei = fin () in
	   Eqs.Component.add i ei acc)
      Methods.current
      (Eqs.empty())

  exception Changed

  let is_unchanged () =
    try
      Methods.Table.iter
	(fun _ test -> 
	   if not(test ()) then raise Changed)
	Methods.is_unchanged;
      true
    with
	Changed -> false
	   
  let abstract i = Methods.call i Methods.abstract
  let process_equal i = Methods.call i Methods.process_equal
  let process_diseq i = Methods.call i Methods.process_diseq
  let process_nonneg i = Methods.call i Methods.process_nonneg
  let process_pos i = Methods.call i Methods.process_pos

  let propagate_equal = Methods.call_all Methods.propagate_equal
  let propagate_diseq = Methods.call_all Methods.propagate_diseq
  let propagate_cnstrnt = Methods.call_all Methods.propagate_cnstrnt
  let propagate_nonneg = Methods.call_all Methods.propagate_nonneg

  exception Branch
			    
  let branch () = None

     (*
    try
      Methods.Table.iter
	(fun _ br -> 
	   try br(); raise Branch with Not_found -> ())
	Methods.branch;
      raise Not_found
    with
	Branch -> ()
     *)

  let normalize () = 
    Methods.call_all Methods.normalize ()

end


let theories = ref Theory.Set.empty

let registered () = !theories

let register th = 
  let name = Theory.to_string th in
  if Theory.Set.mem th !theories then
    invalid_arg (Format.sprintf "Theory %s already registered" name)
  else
    begin
      theories := Theory.Set.add th !theories;
      if Version.debug() >= 1 then
	Format.eprintf "\nRegistering %s@;" name
    end


module Trace(C: COMPONENT): COMPONENT = struct
  let th = C.th
  module E = C.Eqs
  module I = C.Infsys
  let msg str = Format.sprintf "%s.%s" (Theory.to_string th) str
  module Eqs = struct
    type t = E.t
    let empty = E.empty
    let pp = E.pp
    let is_empty = E.is_empty
    let apply s = E.apply s
    let inv s = E.inv s
    let dep s = E.dep s
    let occ = E.occ
    let model = E.model
  end 
  module Infsys = struct
    type eqs = Eqs.t
    let current = I.current
    let reset = Trace.func 8 (msg "reset") Pretty.unit Pretty.unit I.reset
    let initialize = Trace.func 8 (msg "initialize") Eqs.pp Pretty.unit I.initialize
    let is_unchanged = Trace.func 8 (msg "is_unchanged") Pretty.unit Pretty.bool I.is_unchanged
    let finalize = Trace.func 8 (msg "finalize") Pretty.unit Eqs.pp I.finalize
    let abstract = I.abstract
    let process_equal = match I.process_equal with
      | None -> None
      | Some(proc) -> Some(Trace.proc 3 (msg "process") (fun fmt e -> e#pp fmt) proc)
    let process_diseq = match I.process_diseq with
      | None -> None
      | Some(proc) -> Some(Trace.proc 3 (msg "process") (fun fmt d -> d#pp fmt) proc)
    let process_nonneg = match I.process_nonneg with
      | None -> None
      | Some(proc) -> Some(Trace.proc 3 (msg "process") (fun fmt c -> c#pp fmt) proc)
    let process_pos = match I.process_pos with
      | None -> None
      | Some(proc) -> Some(Trace.proc 3 (msg "process") (fun fmt c -> c#pp fmt) proc)
    let propagate_equal = match I.propagate_equal with
      | None -> None
      | Some(proc) -> Some(Trace.proc 3 (msg "propagate") Term.pp proc)
    let propagate_diseq = match I.propagate_diseq with
      | None -> None
      | Some(proc) -> Some(Trace.proc 3 (msg "propagate") (fun fmt d -> d#pp fmt) proc)
    let propagate_cnstrnt = match I.propagate_cnstrnt with
      | None -> None
      | Some(proc) -> Some(Trace.proc 3 (msg "propagate") Term.pp proc)
    let propagate_nonneg =  match I.propagate_nonneg with
      | None -> None
      | Some(proc) -> Some(Trace.proc 3 (msg "propagate") Term.pp proc)
    let branch = I.branch
    let normalize = 
      Trace.proc 3 (msg "normalize") Pretty.unit I.normalize
  end
end



module Register(C: COMPONENT) = struct

  module I = Trace(C)

  let _ =
    if Theory.Set.mem I.th !theories then
      invalid_arg (Format.sprintf "Theory %s already registered" (Theory.to_string I.th))
    else
      theories := Theory.Set.add I.th !theories

  let out (e: Eqs.eqs) = 
    let (ei: I.Eqs.t) = Obj.magic e in
      ei
			   
  let inj (ei: I.Eqs.t) = 
    let (e: Eqs.eqs) = Obj.magic ei in
      e

 let _ = 
   let module Methods = Eqs.Component.Methods in
   let _ = Methods.Table.add Methods.is_empty I.th (fun e -> I.Eqs.is_empty (out e)) in
   let _ = Methods.Table.add Methods.pp I.th (fun fmt e -> I.Eqs.pp fmt (out e)) in
   let _ = Methods.Table.add Methods.apply I.th (fun e -> I.Eqs.apply (out e)) in
   let _ = Methods.Table.add Methods.inv I.th (fun e -> I.Eqs.inv (out e)) in
   let _ = Methods.Table.add Methods.dep I.th (fun e -> I.Eqs.dep (out e)) in
   let _ = Methods.Table.add Methods.occ I.th (fun x e -> I.Eqs.occ x (out e)) in
   let _ = Methods.Table.add Methods.model I.th (fun e -> I.Eqs.model (out e)) in
     ()

 let _ = 
   let module Methods = Infsys.Methods in
   let add m = Methods.Table.add m I.th in
   let _ = add Methods.reset I.Infsys.reset in
   let _ = add Methods.current (fun () -> inj (I.Infsys.current ())) in
   let _ = add Methods.initialize (fun e -> I.Infsys.initialize (out e)) in
   let _ = add Methods.is_unchanged I.Infsys.is_unchanged in
   let _ = add Methods.finalize (fun () -> inj (I.Infsys.finalize ())) in
   let _ = add Methods.abstract I.Infsys.abstract in
   let _ = match I.Infsys.process_equal with 
     | None -> ()
     | Some(process_equal) -> add Methods.process_equal process_equal 
   in
   let _ = match I.Infsys.process_diseq with 
     | None -> ()
     | Some(process_diseq) -> add Methods.process_diseq process_diseq 
   in
   let _ = match I.Infsys.process_nonneg with 
     | None -> ()
     | Some(process_nonneg) -> add Methods.process_nonneg process_nonneg
   in
   let _ = match I.Infsys.process_pos with 
     | None -> ()
     | Some(process_pos) -> add Methods.process_pos process_pos
   in
   let _ = match I.Infsys.propagate_equal with 
     | None -> ()
     | Some(propagate_equal) -> add Methods.propagate_equal propagate_equal 
   in
   let _ = match I.Infsys.propagate_diseq with 
     | None -> ()
     | Some(propagate_diseq) -> add Methods.propagate_diseq propagate_diseq 
   in
   let _ = match I.Infsys.propagate_cnstrnt with 
     | None -> ()
     | Some(propagate_cnstrnt) -> add Methods.propagate_cnstrnt propagate_cnstrnt 
   in
   let _ = match I.Infsys.propagate_nonneg with 
     | None -> ()
     | Some(propagate_nonneg) -> add Methods.propagate_nonneg propagate_nonneg 
   in
   let _ = add Methods.branch I.Infsys.branch in
   let _ = add Methods.normalize I.Infsys.normalize in
     ()

 let _ =
   if Version.debug() >= 1 then
     Format.eprintf "\nRegistering %s@;" (Theory.to_string I.th)

end


module Config = Eqs


(** {6 Equality Components} *)


module type EQUAL = sig
  val th : Theory.t
  module Config : sig
    type t
    val empty : unit -> t
    val is_empty : t -> bool
    val pp : Format.formatter -> t -> unit
    val dep : t -> Term.t -> Dep.Set.t
    val occ : t -> Term.t -> bool
    val model : t -> Term.Model.t
    module Apply : sig
      val get : t -> Term.t -> Term.t
      val justify : t -> Term.t -> Judgement.equal
    end 
    module Inv : sig
      val get : t -> Term.t -> Term.t
      val justify : t -> Term.t -> Judgement.equal
    end 
    module Replace : sig
      val get : t -> Term.t -> Term.t
      val justify : t -> Term.t -> Judgement.equal
    end 
    module Diseq : sig
      val test : t -> Term.t -> Term.t -> bool
      val justify : t -> Term.t -> Term.t -> Judgement.diseq
    end
    module Equal : sig
      val test : t -> Term.t -> Term.t -> bool
      val justify : t -> Term.t -> Term.t -> Judgement.equal
    end
  end
  module Infsys : sig
    val current : unit -> Config.t
    val reset : unit -> unit
    val initialize : Config.t -> unit
    val is_unchanged : unit -> bool
    val finalize : unit -> Config.t
    val abstract : Term.t -> Judgement.atom -> unit
    val process_equal : (Judgement.equal -> unit)
    val process_diseq : (Judgement.diseq -> unit)
    val propagate_equal : (Term.t -> unit)
    val propagate_diseq : (Judgement.diseq -> unit) 
    val branch : unit -> Judgement.disjunction option
    val normalize : unit -> unit
  end
end


module Equal(E: EQUAL) = struct

  let _ = register E.th

  let out (e: Eqs.eqs) = 
    let (ei: E.Config.t) = Obj.magic e in
      ei
			   
  let inj (ei: E.Config.t) = 
    let (e: Eqs.eqs) = Obj.magic ei in
      e

  open Eqs.Component.Methods

  let register m f = Table.add m E.th f

   let _ = register is_empty (fun e -> E.Config.is_empty (out e))
   let _ = register pp (fun fmt e -> E.Config.pp fmt (out e))
(*
   let _ = register apply (fun e -> E.Config.Apply.get (out e))
   let _ = register inv (fun e -> E.Config.Inv.get (out e))
*)
   let _ = register dep (fun e -> E.Config.dep (out e))
   let _ = register occ (fun x e -> E.Config.occ (out e) x)
   let _ = register model (fun e -> E.Config.model (out e))

   open Infsys.Methods

   let register m f = Table.add m E.th

   let _ = register reset E.Infsys.reset
   let _ = register current (fun () -> inj (E.Infsys.current ()))
   let _ = register initialize (fun e -> E.Infsys.initialize (out e))
   let _ = register is_unchanged E.Infsys.is_unchanged
   let _ = register finalize (fun () -> inj (E.Infsys.finalize ()))
   let _ = register abstract E.Infsys.abstract
   let _ = register process_equal E.Infsys.process_equal 
   let _ = register process_diseq E.Infsys.process_diseq
   let _ = register propagate_equal E.Infsys.propagate_equal
   let _ = register propagate_diseq E.Infsys.propagate_diseq

end


(** {6 Arithmetic Components} *)


module type ARITH = sig
  val th : Theory.t
  module Config : sig
    type t
    val empty : unit -> t
    val is_empty : t -> bool
    val pp : Format.formatter -> t -> unit
    val apply : t -> eqtrans
    val inv : t -> eqtrans
    val dep : t -> Term.t -> Dep.Set.t
    val occ : Term.t -> t -> bool
    val model : t -> Term.Model.t
  end
  module Infsys : sig
    type eqs
    val current : unit -> Config.t
    val reset : unit -> unit
    val initialize : Config.t -> unit
    val is_unchanged : unit -> bool
    val finalize : unit -> Config.t
    val abstract : Term.t -> Judgement.atom -> unit
    val process_equal : (Judgement.equal -> unit)
    val process_diseq : (Judgement.diseq -> unit)
    val process_nonneg : (Judgement.nonneg -> unit)
    val process_pos : (Judgement.pos -> unit)
    val propagate_equal : (Term.t -> unit)
    val propagate_diseq : (Judgement.diseq -> unit) 
    val propagate_cnstrnt : (Term.t -> unit)
    val propagate_nonneg : (Term.t -> unit)
    val branch : unit -> Judgement.disjunction option
    val normalize : unit -> unit
  end
end

module Arith(A: ARITH) = struct

  let _ = register A.th

  let out (e: Eqs.eqs) = 
    let (ei: A.Config.t) = Obj.magic e in
      ei
			   
  let inj (ei: A.Config.t) = 
    let (e: Eqs.eqs) = Obj.magic ei in
      e

  open Eqs.Component.Methods

  let register m f = Table.add m A.th f

   let _ = register is_empty (fun e -> A.Config.is_empty (out e))
   let _ = register pp (fun fmt e -> A.Config.pp fmt (out e))
   let _ = register apply (fun e -> A.Config.apply (out e))
   let _ = register inv (fun e -> A.Config.inv (out e))
   let _ = register dep (fun e -> A.Config.dep (out e))
   let _ = register occ (fun x e -> A.Config.occ x (out e))
   let _ = register model (fun e -> A.Config.model (out e))

   open Infsys.Methods

   let register m f = Table.add m A.th

   let _ = register reset A.Infsys.reset
   let _ = register current (fun () -> inj (A.Infsys.current ()))
   let _ = register initialize (fun e -> A.Infsys.initialize (out e))
   let _ = register is_unchanged A.Infsys.is_unchanged
   let _ = register finalize (fun () -> inj (A.Infsys.finalize ()))
   let _ = register abstract A.Infsys.abstract
   let _ = register process_equal A.Infsys.process_equal 
   let _ = register process_diseq A.Infsys.process_diseq
   let _ = register process_nonneg A.Infsys.process_nonneg
   let _ = register process_pos A.Infsys.process_pos
   let _ = register propagate_equal A.Infsys.propagate_equal
   let _ = register propagate_diseq A.Infsys.propagate_diseq
   let _ = register propagate_cnstrnt A.Infsys.propagate_cnstrnt

end

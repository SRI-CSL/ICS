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


module P = Partition


module Config = struct

  type 'e t = G.t * 'e * P.t

  let empty empty_e = (G.empty, empty_e, P.empty)

  let is_empty is_empty_e (g, e, p) =
    G.is_empty g &&
    is_empty_e e &&
    P.is_empty p

  (** Printing a configuration. *)
  let pp pp_e fmt (g, e, p) = 
    Format.fprintf fmt "@[(G:";
    G.pp fmt g; 
    Format.fprintf fmt ",@, E:";
    pp_e fmt e; 
    Format.fprintf fmt ",@, V:"; 
    (let v = Partition.v_of p in
       if V.is_empty v then
	 Format.fprintf fmt "{}"
       else 
       V.pp fmt v);
    Format.fprintf fmt ",@, D:"; 
    (let d = Partition.d_of p in
       if D.is_empty d then
	 Format.fprintf fmt "{}"
       else 
	 D.pp fmt d);
    Format.fprintf fmt ")@]@."

  let eq eq_e (g1, e1, p1) (g2, e2, p2) =
    G.eq g1 g2 &&
    eq_e e1 e2 &&
    P.eq p1 p2

end

let g = ref G.empty
let p = ref Partition.empty

module type EQ = sig
  type e     
  val current : unit -> e
  val initialize : e -> unit
  val finalize : unit -> e
  val abstract : Term.t -> unit
  val merge : Fact.Equal.t -> unit
  val propagate : Fact.Equal.t -> unit
  val dismerge : Fact.Diseq.t -> unit
  val propagate_diseq : Fact.Diseq.t -> unit
  val branch : unit -> unit
  val normalize : unit -> unit
end


module Empty: (EQ with type e = unit) = struct
  type e = unit
  let current () = ()
  let initialize () = ()
  let finalize () = ()
  let abstract _ = invalid_arg "Infsys.Empty.abstract"
  let merge _ = invalid_arg "Infsys.Empty.merge"
  let propagate _ = ()
  let dismerge _ = ()
  let propagate_diseq _ = ()
  let branch () = ()
  let normalize () = ()
end 


module type ARITH = sig
  type e
  val current : unit -> e
  val initialize : e -> unit
  val finalize : unit -> e
  val abstract : Term.t -> unit
  val merge : Fact.Equal.t -> unit
  val propagate : Fact.Equal.t -> unit
  val dismerge : Fact.Diseq.t -> unit
  val propagate_diseq : Fact.Diseq.t -> unit
  val branch : unit -> unit
  val normalize : unit -> unit
  val nonneg : Fact.Nonneg.t -> unit
  val pos : Fact.Pos.t -> unit
end


module type LEVEL = sig 
  type t
  val level : Trace.level
  val eq : t -> t -> bool
  val diff : t -> t -> t
  val pp : t Pretty.printer
end

let difference = ref false

let tracing_disabled = ref false

(** Tracing an equality inference system. *)
module Trace(I: EQ)(L: (LEVEL with type t = I.e)) = struct

  type e = I.e
 
  let current = I.current
  let initialize = I.initialize
  let finalize = I.finalize

  let pp () = 
    Config.pp L.pp Format.err_formatter (!g, current(), !p)

  let call name p a = 
    if Trace.is_active L.level then
      begin
	incr(Trace.indent);
	Format.eprintf "\nCall(%d): %s(%s)@." !Trace.indent name (Pretty.to_string p a)
      end 

  let exit name = 
    if Trace.is_active L.level then
      begin
	Format.eprintf "\nExit(%d): %s ==> " !Trace.indent name;
	Trace.indent := !Trace.indent - 1;
	pp ();
	Format.eprintf "@."
      end 

  let fail name exc = 
    (if Trace.is_active L.level then
       begin
	 Trace.indent := !Trace.indent - 1;
	 Format.eprintf "\nFail %s: %s@." name (Printexc.to_string exc)
       end);
    raise exc

  type 'a rule =  'a -> unit
 
  let rule1 name p rl a =
    try
      call name p a;
      rl a;
      exit name
    with
	exc -> fail name exc

  let rule0 name rl c = 
    try
      call name Pretty.unit ();
      rl ();
      exit name
    with
	exc -> fail name exc

  let abstract = 
   if !tracing_disabled then I.abstract else
     rule1 "abstract" Term.pp I.abstract

  let merge = 
    if !tracing_disabled then I.merge else
      rule1 "merge" Fact.Equal.pp I.merge

  let dismerge  = 
    if !tracing_disabled then I.dismerge else
      rule1 "dismerge" Fact.Diseq.pp I.dismerge

  let propagate e = 
    if !tracing_disabled then I.propagate e else
      rule1 "propagate" Fact.Equal.pp I.propagate e

  let propagate_diseq d = 
    if !tracing_disabled then I.propagate_diseq d else
    rule1 "propagate" Fact.Diseq.pp I.propagate_diseq d

  let branch = I.branch

  let normalize s = 
    if !tracing_disabled then I.normalize s else
    rule0 "normalize" I.normalize s

end 


(** Tracing an equality inference system. *)
module TraceArith(I: ARITH)(L: (LEVEL with type t = I.e)) = struct

  type e = I.e

  let current = I.current
  let initialize = I.initialize
  let finalize = I.finalize

  module Tr = Trace(I)(L)

  let abstract = Tr.abstract
  let merge = Tr.merge
  let dismerge = Tr.dismerge
  let propagate = Tr.propagate
  let propagate_diseq = Tr.propagate_diseq
  let branch = Tr.branch
  let normalize = Tr.normalize

  let nonneg =  
    if !tracing_disabled then I.nonneg else
      Tr.rule1 "nonneg" Fact.Nonneg.pp I.nonneg

  let pos =  
    if !tracing_disabled then I.pos else
      Tr.rule1 "pos" Fact.Pos.pp I.pos

end 

(*
 * The contents of this file are subject to the ICS(TM) Community Research
 * License Version 2.0 (the ``License''); you may not use this file except in
 * compliance with the License. You may obtain a copy of the License at
 * http://www.icansolve.com/license.html.  Software distributed under the
 * License is distributed on an ``AS IS'' basis, WITHOUT WARRANTY OF ANY
 * KIND, either express or implied. See the License for the specific language
 * governing rights and limitations under the License.  The Licensed Software
 * is Copyright (c) SRI International 2003, 2004.  All rights reserved.
 * ``ICS'' is a Jtrademark of SRI International, a California nonprofit public
 * benefit corporation.
 *)

(** {6 Interfaces} *)

class type atom = object
  method concl : Atom.t
  method name : string
  method hyps : atoms
  method pp : Format.formatter -> unit
  method assumptions : Atom.Set.t -> unit
  method validate : bool
end

and atoms = object
  method to_list : atom list
  method assumptions : Atom.Set.t -> unit
  method pp : Format.formatter -> unit
  method validate : bool
end

class type equal = object
  inherit atom
  method lhs : Term.t
  method rhs : Term.t
end

class type diseq = object
  inherit atom
  method lhs : Term.t
  method rhs : Term.t
end

class type cnstrnt = object
  inherit atom
  method arg : Term.t
  method cnstrnt : Cnstrnt.t
end

class type unsat = object
  inherit atom
end

class type valid = object
  inherit atom
end

class type nonneg = object
  inherit atom
  method arg : Term.t
end

class type pos = object
  inherit atom
  method arg : Term.t
end


exception Unsat of unsat
exception Valid of atom


(** {6 Heterogeneous sets of judgements} *)

class virtual topatoms = object
  method virtual to_list : atom list
  method virtual assumptions : Atom.Set.t -> unit
  method virtual pp : Format.formatter -> unit
  method virtual validate : bool
end 

class empty = (object
  inherit topatoms
  method to_list = []
  method assumptions acc = ()
  method pp fmt = Format.fprintf fmt "[]"
  method validate = true
end : atoms)

class ['a] add (a: 'a) atms = (object 
  inherit topatoms
  method to_list = (a:>atom) :: atms#to_list
  method assumptions acc = a#assumptions acc; atms#assumptions acc
  method pp fmt = 
    Format.fprintf fmt "@[[";
    (a:>atom)#pp fmt; 
    Format.fprintf fmt ", ";
    atms#pp fmt;
    Format.fprintf fmt "]@]"
  method validate = a#validate && atms#validate
end : atoms)

class union (as1: atoms) (as2: atoms) = 
  (object 
     inherit topatoms
     method to_list = as1#to_list @ as2#to_list
     method assumptions acc = as1#assumptions acc; as2#assumptions acc
     method pp fmt = as1#pp fmt; Format.fprintf fmt "@ ++@ "; as2#pp fmt
     method validate = as1#validate && as2#validate
   end : atoms)

let mk_empty = new empty
let mk_add a = new add a
let mk_add_equal (e: equal) = new add e
let mk_add_diseq (d: diseq) = new add d
let mk_singleton a = new add a mk_empty
let mk_union as1 as2 = new union as1 as2


(** {6 Operators} *)

let of_list al =
  List.fold_right 
    (fun a -> mk_add (a:>atom)) al mk_empty

let of_equal_list = of_list
let of_nonneg_list = of_list 
let of_cnstrnt_list = of_list 

let pp fmt a = a#pp fmt

let to_atom j = (j:>atom)


let validates p a = 
  Atom.eq p#concl a

let validates_equal e s t =
  Term.eq e#lhs s && Term.eq e#rhs t

(** Set of homogeneous atoms *)
module Set = Sets.Make(
  struct
    type t = atom
    let compare r1 r2 = 
      Pervasives.compare r1#concl r2#concl
    let pp fmt a = a#pp fmt
  end)		


(** {6 Virtual Judgements} *)

let footprint = ref (Version.debug() >= 8)

module Top = struct

  class virtual atom = object (self)
    method virtual concl : Atom.t
    method virtual name : string
    method virtual hyps : atoms
    method pp fmt =
      Format.fprintf fmt "@[";
      self#hyps#pp fmt; Format.fprintf fmt " |- "; Atom.pp fmt self#concl;
      Format.fprintf fmt "@]@;"
    method assumptions = self#hyps#assumptions
    method validate = true
    initializer 
      if !footprint then 
	self#pp Format.err_formatter
  end 

  class virtual unsat = object(self)
    inherit atom
    method concl = Atom.FF
  end
    
  class virtual equal = object(self)
    inherit atom
    method virtual lhs : Term.t
    method virtual rhs : Term.t
    method concl = Atom.Equal(self#lhs, self#rhs)
  end
    
  class virtual diseq = object(self)
    inherit atom
    method virtual lhs : Term.t
    method virtual rhs : Term.t
    method concl = Atom.Diseq(self#lhs, self#rhs)
  end
    
  class virtual cnstrnt = object(self)
    inherit atom
    method virtual arg : Term.t
    method virtual cnstrnt : Cnstrnt.t
    method concl = Atom.Cnstrnt(self#arg, self#cnstrnt)
  end

  class virtual nonneg = object(self)
    inherit atom
    method virtual arg : Term.t
    method concl = Atom.Nonneg(self#arg)
  end

  class virtual pos = object(self)
    inherit atom
    method virtual arg : Term.t
    method concl = Atom.Pos(self#arg)
  end

end

(** {6 Axioms} *)

class axiom (a: Atom.t) =
  (object
     inherit Top.atom
     method concl = a
     method hyps = mk_empty
     method name = "axiom"
     method assumptions acc = Atom.Set.add a acc
   end : atom)

let mk_axiom a = new axiom a


(** {6 Equality judgements} *)

class refl (a: Term.t) = (object 
  inherit Top.equal
  method lhs = a
  method rhs = a
  method hyps = mk_empty
  method name =  "refl"
end : equal)

class sym (e: equal) = (object
  inherit Top.equal
  method lhs = e#rhs
  method rhs = e#lhs
  method name = "sym"
  method hyps = mk_singleton e
end : equal)

class trans (e1: equal) (e2: equal) = (object (self) 
  inherit Top.equal
  method lhs = e1#lhs
  method rhs = e2#rhs
  method hyps = mk_add e1 (mk_singleton e2)
  method name = "trans"
  method validate = Term.eq e1#rhs e2#lhs
end : equal)

class join (e1: equal) (e2: equal) = (object (self) 
  inherit Top.equal
  method lhs = e1#lhs
  method rhs = e2#lhs
  method hyps = mk_add e1 (mk_singleton e2)
  method name = "join"
  method validate = Term.eq e1#lhs e2#lhs
end : equal)

class alias (x: Term.t) (t: Term.t) = 
  (object
     inherit Top.equal
     method lhs = x
     method rhs = t
     method name = "alias"
     method hyps = mk_empty
   end : equal)

let mk_alias x t = new alias x t
   
let mk_refl = 
  let module M = Memoize.Make(
    struct
      type d = Term.t
      type r = equal
      let eq = Term.eq
      let hash = Term.hash
      let f t = new refl t
    end)
  in
    M.memoize

let is_refl e = (e#lhs == e#rhs)

let mk_sym e = 
  if is_refl e then e else new sym e

let mk_trans e1 e2 = 
  if is_refl e1 then e2 else 
    if is_refl e2 then e1 else
      new trans e1 e2

let mk_join e1 e2 = 
  if is_refl e1 then e2 else 
    if is_refl e2 then e1 else
      new join e1 e2

let mk_trans3 e1 e2 e3 =
  mk_trans e1 (mk_trans e2 e3)

let mk_alias x t = new alias x t

(** {6 Trivially true judgement} *)

class triv = object
  inherit Top.atom
  method concl = Atom.mk_false
  method name = "triv"
  method hyps = mk_empty
end

let mk_triv = new triv

(** {6 Unsatisfiability judgements} *)

class bot (d: diseq) = (object
  inherit Top.unsat
  method hyps = mk_singleton d
  method name = "bot"
  method validate = Term.eq d#lhs d#rhs
end : unsat)

let mk_bot d = new bot d

class contra (e: equal) (d: diseq) = (object
  inherit Top.unsat
  method hyps = mk_add e (mk_singleton d)
  method name = "contra"
  method validate = Term.eq e#rhs d#rhs && Term.eq e#lhs d#lhs
end : unsat)

let mk_contra e d = 
  if is_refl e then mk_bot d else
    new contra e d

(** {6 Constraint judgmements} *)

class inter (c1: cnstrnt) (c2: cnstrnt) = 
  (object (self)
     inherit Top.cnstrnt
     method arg = c1#arg
     method cnstrnt = Cnstrnt.inter c1#cnstrnt c2#cnstrnt
     method hyps = mk_add c1 (mk_singleton c2)
     method name = "inter"
     method validate = Term.eq c1#arg c2#arg
   end: cnstrnt)

let mk_inter c1 c2 = new inter c1 c2

(** {6 Disequality judgmements} *)

class disjoint (c1: cnstrnt) (c2: cnstrnt) = (object(self)
  inherit Top.diseq
  method lhs = c1#arg
  method rhs = c2#arg
  method name = "disjoint"
  method hyps = mk_add c1 (mk_singleton c2)
  method validate = Term.eq c1#arg c2#arg && Cnstrnt.disjoint c1#cnstrnt c2#cnstrnt
end : diseq)

let mk_disjoint c1 c2 = new disjoint c1 c2

class func i (d: diseq) = (object
  inherit Top.diseq
  method lhs = Term.Args.get (Term.args_of d#lhs) i
  method rhs = Term.Args.get (Term.args_of d#rhs) i
  method hyps = mk_singleton d
  method name = Format.sprintf "func[%d]" i
end : diseq)

let mk_func i d = new func i d



(** {6 Replace} *)

class replace_in_atom (e: equal) (a: atom) c = (object (self)
  inherit Top.atom
  method concl = c
  method hyps = mk_add e (mk_singleton a)
  method name = "replace"
  method assumptions acc = e#assumptions acc; a#assumptions acc
  method validate = true
end: atom)

class replace_in_equal (el: equal list) (e: equal) = (object (self)
  inherit Top.equal
  method lhs = e#lhs
  method rhs = e#rhs
  method hyps = of_list el
  method name = "replace"
  method assumptions acc = List.iter (fun p -> p#assumptions acc) el
  method validate = true
end: equal)

class replace_in_cnstrnt (p1: equal) (p2: cnstrnt) = 
  (object (self)
     inherit Top.cnstrnt
     method arg = p1#rhs
     method cnstrnt = p2#cnstrnt
     method hyps = mk_add p1 (mk_singleton p2)
     method name = "replace"
     method assumptions acc = p1#assumptions acc; p2#assumptions acc
     method validate = Term.eq p1#rhs p2#arg
   end: cnstrnt)

class replace_in_diseq (e1: equal) (e2: equal) (d3: diseq) = 
  (object (self)
     inherit Top.diseq
     method lhs = e1#lhs
     method rhs = e2#rhs
     method hyps = of_list [e1; e2; d3]
     method name = "replace"
     method assumptions acc =  e1#assumptions acc; e2#assumptions acc; d3#assumptions acc
     method validate = Term.eq e1#rhs d3#rhs && Term.eq e2#rhs d3#rhs
   end: diseq)

class replace_in_nonneg (e: equal) (n: nonneg) = 
  (object (self)
     inherit Top.nonneg
     method arg = e#lhs
     method name = "replace"
     method hyps = mk_add e (mk_singleton n)
     method assumptions acc =  e#assumptions acc; n#assumptions acc
     method validate = Term.eq e#lhs n#arg
   end: nonneg)

class replace_in_pos (e: equal) (p: pos) = 
  (object (self)
     inherit Top.pos
     method arg = e#lhs
     method name = "replace"
     method hyps = mk_add e (mk_singleton p)
     method assumptions acc = e#assumptions acc; p#assumptions acc
     method validate = Term.eq e#lhs p#arg
   end: nonneg)

let mk_replace_in_atom e a =
  if is_refl e then a else
    let concl' = Atom.replace e#lhs e#rhs a#concl in
      new replace_in_atom e a concl'

let mk_replace_in_equal el e = new replace_in_equal el e

let mk_replace_in_cnstrnt e1 c2 = 
  if is_refl e1 then c2 else
    new replace_in_cnstrnt e1 c2

let mk_replace_in_nonneg e n =
  if is_refl e then n else
    new replace_in_nonneg e n

let mk_replace_in_pos e p = 
  if is_refl e then p else
    new replace_in_pos e p

let mk_replace_in_diseq e1 e2 d3 =
  if is_refl e1 && is_refl e2 then d3 else
    new replace_in_diseq e1 e2 d3


(** {6 Transformations} *)

let mk_transform_diseq f d =
  let e1 = f d#lhs and e2 = f d#rhs in
    if is_refl e1 && is_refl e2 then d else
      mk_replace_in_diseq e1 e2 d
	
let mk_transform_equal f e =
  let e1 = f e#lhs and e2 = f e#rhs in
    if is_refl e1 && is_refl e2 then e else
      mk_replace_in_equal [e1; e2] e
    
let mk_transform_rhs_equal f e =
  let e' = f e#rhs in
    if is_refl e' then e else
      mk_replace_in_equal [mk_refl e#lhs; e'] e

let mk_transform_nonneg f n =
  let e = f n#arg in
    if is_refl e then n else
      mk_replace_in_nonneg e n

let mk_transform_pos f p =
  let e = f p#arg in
    if is_refl e then p else
      mk_replace_in_pos e p

let mk_transform_cnstrnt f c =
  let e = f c#arg in
    if is_refl e then c else
      mk_replace_in_cnstrnt e c


(** {6 Projections} *)

let to_unsat j = 
  assert(Atom.is_false j#concl);
  failwith "to_unsat: to do"
  
let to_equal j = failwith "to_unsat: to do"
let to_diseq j = failwith "to_unsat: to do"
let to_nonneg j = failwith "to_unsat: to do"
let to_pos j = failwith "to_unsat: to do"
let to_cnstrnt j = failwith "to_unsat: to do"


(** {6 Disjunction} *)

class type disjunction = object
  method concl : atoms
  method hyps : atoms
  method name : string
  method assumptions : Atom.Set.t -> unit
  method validate : bool
end

class disj (hyps: atoms) (concl: atoms) = (object (self)
  method concl = concl
  method hyps = hyps
  method name = "disj"
  method assumptions = hyps#assumptions
  method validate = true
end : disjunction)

let mk_disj hl c = new disj hl c


(** {6 Homogeneous sets} *)

module Equals = Sets.Make(
  struct
    type t = equal
    let eq e1 e2 = 
      (Term.eq e1#rhs e2#rhs && Term.eq e1#lhs e2#lhs) ||
      (Term.eq e2#rhs e1#lhs && Term.eq e2#lhs e1#rhs)
    let compare e1 e2 =
       if eq e1 e2 then 0 else Pervasives.compare e1 e2
    let pp fmt e = e#pp fmt
end)

let equals_to_atoms es = 
  Equals.fold mk_add es mk_empty


module Diseqs = Sets.Make(
  struct
    type t = diseq
    let eq d1 d2 = 
      (Term.eq d1#rhs d2#rhs && Term.eq d1#lhs d2#lhs) ||
      (Term.eq d2#rhs d1#lhs && Term.eq d2#lhs d1#rhs)
    let compare d1 d2 =
      if eq d1 d2 then 0 else Pervasives.compare d1 d2
    let pp fmt d = d#pp fmt
  end)

module Cnstrnts = Sets.Make(
  struct
    type t = cnstrnt
    let eq c1 c2 = 
      Term.eq c1#arg c2#arg && c1#cnstrnt = c2#cnstrnt
    let compare c1 c2 =
      if eq c1 c2 then 0 else Pervasives.compare c1 c2
    let pp fmt c = c#pp fmt
  end)

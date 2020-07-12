(* 
 * The MIT License (MIT)
 *
 * Copyright (c) 2020 SRI International
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 *)

module Ics = Ics_

module Logic = struct
  type t =
    | AUFLIA
        (** Closed linear formulas over the theory of integer arrays with
            free sort, function and predicate symbols. *)
    | QF_A
        (** Closed quantifier-free formulas over the theory of arrays
            without extensionality. *)
    | QF_AUFLIA
        (** Closed quantifier-free and linear formulas over the theory of
            integer arrays with free sort, function and predicate symbols. *)
    | QF_IDL  (** Difference Logic over the integers. *)
    | QF_LIA  (** Unquantified integer linear arithmetic. *)
    | QF_LRA  (** Unquantified real linear arithmetic. *)
    | QF_RDL  (** Difference Logic over the reals. *)
    | QF_UF
        (** Unquantified formulas built over a signature of uninterpreted
            sort, function and predicate symbols. *)
    | QF_UFIDL
        (** Difference Logic over the integers (in essence) but with
            unintepreted sort, function, and predicate symbols. *)
    | QF_UFLIA
        (** Unquantified integer linear arithmetic with unintepreted sort,
            function, and predicate symbols. *)
    | QF_UFLRA
        (** Unquantified real linear arithmetic with unintepreted sort,
            function, and predicate symbols. *)

  let translation =
    [ (AUFLIA, "AUFLIA")
    ; (QF_A, "QF_A")
    ; (QF_AUFLIA, "QF_AUFLIA")
    ; (QF_IDL, "QF_IDL")
    ; (QF_LIA, "QF_LIA")
    ; (QF_LRA, "QF_LRA")
    ; (QF_RDL, "QF_RDL")
    ; (QF_UF, "QF_UF")
    ; (QF_UFIDL, "QF_UFIDL")
    ; (QF_UFLRA, "QF_UFLRA") ]

  let to_string (l : t) = List.assoc l translation

  let of_sym (s : Name.t) =
    let s = Name.to_string s in
    let rec search = function
      | [] -> raise Not_found
      | (l, s') :: sl' -> if s = s' then l else search sl'
    in
    search translation
end

module Ast = struct
  module Sym = Name
  module Funsym = Name
  module Predsym = Name
  module Sort = Name
  module Numeral = Z

  let intern = Name.of_string
  let extern = Name.to_string

  type sort = Array | Int | Real | Extra of Sort.t
  type fun_kind = {dom: sort list; cod: sort}
  type pred_kind = sort list
  type var = Sym.t
  type funsym = Funsym.t
  type predsym = Predsym.t

  type trm =
    | Var of var
    | Num of Numeral.t
    | Apply of Funsym.t * trm list
    | Cond of fml * trm * trm
    | Add of trm list
    | Mult of trm list
    | Div of trm * trm
    | Sub of trm * trm
    | Select of trm * trm
    | Store of trm * trm * trm

  and fml =
    | True
    | False
    | Fvar of Sym.t
    | Equal of trm * trm
    | Lt of trm * trm
    | Le of trm * trm
    | Gt of trm * trm
    | Ge of trm * trm
    | Predapply of predsym * trm list
    | Distinct of trm list
    | Neg of fml
    | Conj of fml list
    | Disj of fml list
    | Impl of fml list
    | Equiv of fml list
    | Xor of fml list
    | Ite of fml * fml * fml
    | Exists of (var * sort) list * fml
    | Forall of (var * sort) list * fml
    | Let of var * trm * fml
    | Flet of var * fml * fml

  type status = Unsat | Sat | Unknown

  let pp_status fmt status =
    match (status : status) with
    | Unsat -> Format.fprintf fmt "Unsat"
    | Sat -> Format.fprintf fmt "Sat"
    | Unknown -> Format.fprintf fmt "Unknown"

  module Decls = Map.Make (Sym)
  module Sorts = Set.Make (Sort)

  type bench =
    { mutable name: string
    ; mutable logic: Logic.t
    ; mutable funsym_decls: fun_kind Decls.t
    ; mutable predsym_decls: pred_kind Decls.t
    ; mutable sort_decls: Sorts.t
    ; mutable status: status
    ; mutable assumptions: fml list
    ; mutable conclusion: fml }
end

module Fill = struct
  open! Ast

  let empty_bench () =
    { name= ""
    ; logic= QF_UF
    ; funsym_decls= Decls.empty
    ; predsym_decls= Decls.empty
    ; sort_decls= Sorts.empty
    ; status= Unknown
    ; assumptions= []
    ; conclusion= True }

  let current = ref (empty_bench ())
  let reset () = current := empty_bench ()
  let finalize () = !current
  let set_name str = !current.name <- str
  let set_logic l = !current.logic <- l

  let add_assumption fml =
    !current.assumptions <- fml :: !current.assumptions

  let set_conclusion fml = !current.conclusion <- fml
  let set_status st = !current.status <- st

  let add_funsym_decl f (t : fun_kind) =
    assert (not (Decls.mem f !current.funsym_decls)) ;
    !current.funsym_decls <- Decls.add f t !current.funsym_decls

  let add_predsym_decl p t =
    assert (not (Decls.mem p !current.predsym_decls)) ;
    !current.predsym_decls <- Decls.add p t !current.predsym_decls

  let add_sort_decl s =
    assert (not (Sorts.mem s !current.sort_decls)) ;
    !current.sort_decls <- Sorts.add s !current.sort_decls
end

open Ast

let decide incomplete_flag b =
  let let_decls = Hashtbl.create 7 in
  let flet_decls = Hashtbl.create 7 in
  let quant_cache = Hashtbl.create 7 in
  let term_cond = Hashtbl.create 7 in
  let funsym f = Funsym.of_string (extern f) in
  let predsym p = Ics.Predsym.uninterp p in
  let propvar p = Ics.Propvar.of_ident p in
  Ics.reset () ;
  let rec trm2ics t =
    match t with
    | Var x -> (
      try Hashtbl.find let_decls x
      with Not_found -> Ics.var (Ics.Var.of_ident x) )
    | Num n -> Ics.constz n
    | Apply (f, []) -> Ics.apply (funsym f) (Ics.nil ())
    | Apply (f, [t]) -> Ics.apply (funsym f) (trm2ics t)
    | Apply (f, tl) ->
        Ics.apply (funsym f)
          (Ics.tuple (Array.of_list (List.map trm2ics tl)))
    | Cond (c, p, n) ->
        let c' = fml2ics c in
        if Ics.Formula.is_true c' then trm2ics p
        else if Ics.Formula.is_false c' then trm2ics n
        else
          let p' = trm2ics p and n' = trm2ics n in
          if Ics.Term.equal p' n' then p'
          else
            let x = Ics.Var.fresh () in
            let f =
              Ics.ite c' (Ics.eq (Ics.var x) p') (Ics.eq (Ics.var x) n')
            in
            Hashtbl.add term_cond x f ;
            Ics.var x
    | Add tl ->
        let rec addl acc = function
          | [] -> acc
          | [t] -> Ics.add (trm2ics t) acc
          | t :: tl -> addl (Ics.add (trm2ics t) acc) tl
        in
        addl (Ics.constz Z.zero) tl
    | Mult tl ->
        let mult2 t1 t2 =
          try Ics.multq (Ics.d_num t1) t2
          with Not_found -> (
            try Ics.multq (Ics.d_num t2) t1
            with Not_found ->
              Ics.apply (Funsym.of_string "*") (Ics.pair t1 t2) )
        in
        let rec multl acc = function
          | [] -> acc
          | [t] -> mult2 (trm2ics t) acc
          | t :: tl -> multl (mult2 (trm2ics t) acc) tl
        in
        multl (Ics.constz Z.one) tl
    | Div (s, t) -> (
        let t1 = trm2ics s and t2 = trm2ics t in
        try Ics.multq (Q.inv (Ics.d_num t2)) t1
        with Not_found ->
          Ics.apply (Funsym.of_string "div") (Ics.pair t1 t2) )
    | Sub (s, t) -> Ics.sub (trm2ics s) (trm2ics t)
    | Select (s, t) -> Ics.lookup (trm2ics s) (trm2ics t)
    | Store (s, t, r) -> Ics.update (trm2ics s) (trm2ics t) (trm2ics r)
  and fml2ics f =
    match f with
    | True -> Ics.tt
    | False -> Ics.ff
    | Fvar x -> (
      try Hashtbl.find flet_decls x
      with Not_found ->
        failwith ("Flet definition " ^ extern x ^ " not found") )
    | Equal (s, t) -> Ics.eq (trm2ics s) (trm2ics t)
    | Lt (s, t) -> Ics.lt (trm2ics s) (trm2ics t)
    | Le (s, t) -> Ics.le (trm2ics s) (trm2ics t)
    | Gt (s, t) -> Ics.gt (trm2ics s) (trm2ics t)
    | Ge (s, t) -> Ics.ge (trm2ics s) (trm2ics t)
    | Predapply (p, []) -> Ics.posvar (propvar p)
    | Predapply (p, [t]) -> Ics.poslit (predsym p) (trm2ics t)
    | Predapply (p, tl) ->
        Ics.poslit (predsym p)
          (Ics.tuple (Array.of_list (List.map trm2ics tl)))
    | Distinct _ ->
        (* ToDo: handle distinct *)
        Ics.tt
    | Neg p -> Ics.neg (fml2ics p)
    | Conj pl ->
        let rec conj acc = function
          | [] -> acc
          | [p] -> Ics.andthen acc (fml2ics p)
          | p :: pl -> conj (Ics.andthen acc (fml2ics p)) pl
        in
        conj Ics.tt pl
    | Disj pl ->
        let rec conj acc = function
          | [] -> acc
          | [p] -> Ics.orelse acc (fml2ics p)
          | p :: pl -> conj (Ics.orelse acc (fml2ics p)) pl
        in
        conj Ics.ff pl
    | Impl pl -> (
      match pl with
      | [] -> Ics.tt
      | p' :: pl' ->
          let rec implies acc = function
            | [] -> acc
            | [p] -> Ics.implies acc (fml2ics p)
            | p :: pl -> implies (Ics.implies acc (fml2ics p)) pl
          in
          implies (fml2ics p') pl' )
    | Equiv pl ->
        let rec equiv acc = function
          | [] -> acc
          | [p] -> Ics.equiv acc (fml2ics p)
          | p :: pl -> equiv (Ics.equiv acc (fml2ics p)) pl
        in
        equiv Ics.tt pl
    | Xor pl ->
        let rec xor acc = function
          | [] -> acc
          | [p] -> Ics.xor acc (fml2ics p)
          | p :: pl -> xor (Ics.xor acc (fml2ics p)) pl
        in
        xor Ics.tt pl
    | Ite (c, p, n) ->
        let c' = fml2ics c in
        if Ics.Formula.is_true c' then fml2ics p
        else if Ics.Formula.is_false c' then fml2ics n
        else
          let p' = fml2ics p and n' = fml2ics n in
          if n' == p' then p' else Ics.ite c' p' n'
    | Exists (_, _) | Forall (_, _) -> (
      try Hashtbl.find quant_cache f
      with Not_found ->
        let p = Ics.posvar (Ics.Propvar.fresh ()) in
        Hashtbl.add quant_cache f p ;
        p )
    | Let (x, t, p) -> (
        Hashtbl.add let_decls x (trm2ics t) ;
        try
          let p' = fml2ics p in
          Hashtbl.remove let_decls x ;
          p'
        with exc ->
          Hashtbl.remove let_decls x ;
          raise exc )
    | Flet (x, q, p) -> (
        Hashtbl.add flet_decls x (fml2ics q) ;
        try
          let p' = fml2ics p in
          Hashtbl.remove flet_decls x ;
          p'
        with exc ->
          Hashtbl.remove let_decls x ;
          raise exc )
  in
  let rec fmls2ics = function
    | [] -> Ics.tt
    | [p] -> fml2ics p
    | p :: pl ->
        let p' = fml2ics p in
        if Ics.Formula.is_false p' then Ics.ff
        else if Ics.Formula.is_true p' then fmls2ics pl
        else Ics.andthen p' (fmls2ics pl)
  in
  let hyps = fmls2ics b.assumptions in
  let concl = fml2ics b.conclusion in
  let fml = Ics.implies hyps concl in
  (* to do: add constraints. *)
  let status = Ics.process fml in
  match if incomplete_flag then status else Ics.resolve () with
  | Ics.Sat _ -> Ast.Sat
  | Ics.Unsat _ -> Ast.Unsat
  | Ics.Unknown -> Ast.Unknown

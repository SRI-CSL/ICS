
(*i
 * The contents of this file are subject to the ICS(TM) Community Research
 * License Version 1.0 (the ``License''); you may not use this file except in
 * compliance with the License. You may obtain a copy of the License at
 * http://www.icansolve.com/license.html.  Software distributed under the
 * License is distributed on an ``AS IS'' basis, WITHOUT WARRANTY OF ANY
 * KIND, either express or implied. See the License for the specific language
 * governing rights and limitations under the License.  The Licensed Software
 * is Copyright (c) SRI International 2001, 2002.  All rights reserved.
 * ``ICS'' is a trademark of SRI International, a California nonprofit public
 * benefit corporation.
 * 
 * Author: Harald Ruess
 i*)

(*i*)
open Format
open Term
open Hashcons
(*i*)

type 'a printer = Format.formatter -> 'a -> unit

let print_all = ref false
let _ =  Tools.add_at_reset (fun () -> print_all := false)

let get_print_all () = !print_all
let set_print_all b = (print_all := b)

let pr fmt str =
  Format.fprintf fmt str

let number fmt c =
  Number.pp fmt c

let sym fmt f =
  Sym.pp !print_all fmt f

let list pre sep post pp fmt l =
  let rec iter = function
    | [] -> ()
    | [x] -> pp fmt x
    | x :: l -> pp fmt x; pr fmt "%s" sep; iter l
  in
  pr fmt "@[%s" pre; iter l; pr fmt "%s@]" post

let rec term fmt a =
  let f,l = Term.destruct a in
  if l = [] then
    constant fmt f
  else if !print_all then
    prefix fmt (f,l)
  else 
    match Sym.destruct f, l with
      | Sym.Interp(Sym.Nonlin(Sym.Expt(n))), [x] -> 
	  (term fmt x; pr fmt "^%d" n)
      | Sym.Interp(Sym.Arith(Sym.Multq(q))), [x] -> 
	  (Mpa.Q.pp fmt q; pr fmt "*"; term fmt x)
      | Sym.Interp(Sym.Bv(Sym.Sub(n,i,j))), [x] -> 
	  (term fmt x; Format.fprintf fmt "[%d:%d]" i j)
      | Sym.Interp(Sym.Bv(Sym.Conc(n,m))), [x;y] -> 
	  (term fmt x; pr fmt " ++ "; term fmt y)
      | _ when Sym.eq f Sym.mk_mult ->
	  infixl "**" fmt l
      | _ when Sym.eq f Sym.mk_add ->
	  infixl "+" fmt l
      | _ when Sym.eq f Sym.mk_tuple ->
	  list "(" "," ")" term fmt l
      | _ ->
	  prefix fmt (f,l)


and constant fmt f = 
  pr fmt "@["; sym fmt f; pr fmt "@]"

and prefix fmt (f,l) = 
  pr fmt "@["; 
  sym fmt f; 
  list "(" ", " ")" term fmt l; 
  pr fmt "@]"

and infix fmt x opstr y =
  pr fmt "@["; 
  term fmt x; pr fmt "%s" opstr; term fmt y;
  pr fmt "@]"

and infixl opstr fmt l =
  pr fmt "@["; 
  list "" opstr "" term fmt l; 
  pr fmt "@]"

let eqn fmt (a,b) =
  pr fmt "@["; 
  term fmt a; pr fmt " = "; term fmt b; 
  pr fmt "@]"

let diseq fmt (a,b) =
  pr fmt "@["; 
  term fmt a; pr fmt " <> "; term fmt b; 
  pr fmt "@]"

let inn fmt (a,c) =
  pr fmt "@["; 
  term fmt a; pr fmt " in "; Number.pp fmt c; 
  pr fmt "@]"
    
let set fmt s = 
  list "{" ", " "}" term fmt (Term.Set.elements s)

let tset = set
   
let map p fmt m =
  let elems =
    Term.Map.fold (fun a b acc -> (a,b) :: acc) m [] 
  in 
  let assign fmt (x,a) =
    pr fmt "@["; term fmt x; pr fmt " |-> "; p fmt a;   pr  fmt"@]";
  in 
  list "[" ", " "]" assign fmt elems
    
let tmap = map term

let tlist = 
  list "[" ", " "]" term

let list p fmt = list "[" ", " "]" p fmt

let atom fmt p =
  pr fmt "@[";
  (match p with
     | Atom.True -> Format.fprintf fmt "true"
     | Atom.False -> Format.fprintf fmt "false"
     | Atom.Equal(x,y) -> infix fmt x " = " y
     | Atom.Diseq(x,y) -> infix fmt x " <> " y
     | Atom.In(c,x) -> term fmt x; pr fmt " in "; Number.pp fmt c);
  pr fmt "@]"

let atoms fmt ps =
  let l = Atom.Set.elements ps in
  list atom fmt l

let rec prop fmt b = 
  match Prop.destruct b with
    | Prop.True -> 
	Format.fprintf fmt "tt"
    | Prop.False -> 
	Format.fprintf fmt "ff"
    | Prop.Ite(x,t,f) 
	when Prop.is_tt t && Prop.is_ff f ->
	atom fmt x
    | Prop.Ite(x,p,n) -> 
	Format.fprintf fmt "@[if ";
	atom fmt x;
        Format.fprintf fmt " then@ ";
	prop fmt p;
	Format.fprintf fmt " else@ ";
	prop fmt n;
	Format.fprintf fmt " end@]"




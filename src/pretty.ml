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

(** Pretty-printing. *)

open Format

module Mode = struct

  type t = Mixfix | Prefix | Sexpr

  let to_string = function
    | Mixfix -> "mixfix"
    | Prefix -> "prefix"
    | Sexpr -> "sexpr"

  let of_string = function
    | "mixfix" -> Mixfix
    | "prefix" -> Prefix
    | "sexpr" -> Sexpr
    | str -> invalid_arg (sprintf "%s: no such pretty-printing mode" str)

end 

open Mode

let flag = ref Mixfix

type 'a printer = formatter -> 'a -> unit

(** {6 S-expressions} *)
       
(** print S-expression [(op a)]. *)
let sexpr1 pp fmt (op, a) = 
  printf "(%s " op; pp fmt a; printf ")@."

(** print S-expression [(op a1 ... an)]. *)
let sexpr qq fmt (op, al) =
  fprintf fmt "(%s" op;
  List.iter (fun a -> fprintf fmt " "; qq fmt a) al;
  fprintf fmt ")@."

(** print S-expression [(op a1 a2)]. *)
let sexpr2 pp1 pp2 fmt (op, a1, a2) = 
  printf "(%s " op;
  pp1 fmt a1; printf " ";
  pp2 fmt a2; printf ")@."

(** print S-expression [(op a1 a2 a3)]. *)
let sexpr3 pp1 pp2 pp3 fmt (op, a1, a2, a3) = 
  printf "(%s " op;
  pp1 fmt a1; printf " ";
  pp2 fmt a2; printf " ";
  pp3 fmt a3; printf ")@."


(** {6 Basic data types} *)

let unit fmt () =
  fprintf fmt "()"

let string fmt str =
  fprintf fmt "%s" str

let number fmt i =
  fprintf fmt "%d" i

let bool fmt = function
  | true -> fprintf fmt ":true"
  | false -> fprintf fmt ":false"

let three fmt = function
  | Three.Yes -> fprintf fmt ":yes"
  | Three.No -> fprintf fmt ":no"
  | Three.X -> fprintf fmt ":x"

let option pp fmt = 
  function
    | None -> fprintf fmt ":none"
    | Some(x) -> 
	(match !flag with
	   | Sexpr -> sexpr1 pp fmt (":some", x)
	   | _ -> fprintf fmt ":some("; pp fmt x; fprintf fmt ")")

let break = ref true
	
let list (pre, sep, post) pp fmt l =
  let separator = List.length l >= 3 in
  let rec iter l =
    match l with
      | [] -> ()
      | [x] -> 
	  pp fmt x;
      | x :: l -> 
	  pp fmt x; 
	  string fmt sep; 
	  if !break && separator then fprintf fmt "@, " else (); 
	  iter l
  in
    match !flag with
      | Sexpr -> sexpr pp fmt (":list", l)
      | _ -> 
         fprintf fmt "%s" pre; 
	 iter l;
	 fprintf fmt "%s@?" post

let pair pp1 pp2 fmt (a, b) =
  match !flag with
    | Sexpr -> sexpr2 pp1 pp2 fmt (":pair", a, b) 
    | _ -> fprintf fmt "("; pp1 fmt a; string fmt ","; pp2 fmt b; fprintf fmt ")@?"

let triple pp1 pp2 pp3 fmt (a, b, c) =
  match !flag with
    | Sexpr ->  sexpr3 pp1 pp2 pp3 fmt (":triple", a, b, c) 
    | _ -> fprintf fmt "("; pp1 fmt a; string fmt ","; pp2 fmt b; string fmt ","; pp3 fmt c; fprintf fmt ")@?"

let set pp fmt al = 
  match !flag with
    | Sexpr -> sexpr pp fmt (":set", al)
    | _ -> 
	list ("{", ", ", "}") pp fmt al

let assign pp1 pp2 fmt (x, a) =
  match !flag with
    | Sexpr -> sexpr2 pp1 pp2 fmt (":assign", x, a)
    | _ -> fprintf fmt "@["; pp1 fmt x; string fmt " |-> "; pp2 fmt a; fprintf fmt "@]"

let map pp1 pp2 fmt ml =
  match !flag with
    | Sexpr -> sexpr (assign pp1 pp2) fmt (":map", ml)
    | _ -> list ("[", "; ", "]") (assign pp1 pp2) fmt ml


(** {6 Applications} *)

let args pp fmt l = 
  break := false;
  list ("(", ", ", ")") pp fmt l;
  break := true

let apply qq fmt (f, al) =
  match !flag with
    | Sexpr -> sexpr qq fmt (f, al)
    | _ -> (fprintf fmt "%s" f; args qq fmt al)


let infix pp1 op pp2 fmt (a, b) =
  match !flag with
    | Sexpr -> sexpr2 pp1 pp2 fmt (op, a, b)
    | Prefix -> fprintf fmt "%s" op; pair pp1 pp2 fmt (a, b)
    | Mixfix -> fprintf fmt ""; pp1 fmt a; fprintf fmt " %s " op; pp2 fmt b; fprintf fmt ""

let post pp fmt (a, op) =
  match !flag with
    | Sexpr -> sexpr1 pp fmt (op, a)
    | _ -> fprintf fmt "";  pp fmt a; fprintf fmt " %s" op; fprintf fmt ""

let mixfix str1 pp1 str2 pp2 str3 pp3 str4 fmt (a, b, c) =
  let opname str1 str2 str3 str4 =  str1 ^ "__" ^ str2 ^ "__" ^ str3 ^ "__" ^ str4 in
  match !flag with
    | Sexpr -> sexpr3 pp1 pp2 pp3 fmt (opname str1 str2 str3 str4, a, b, c)
    | _ ->
	string fmt str1; pp1 fmt a; 
	string fmt str2; pp2 fmt b; 
	string fmt str3; pp3 fmt c; 
	string fmt str4


let infixl pp op fmt al =
  match !flag with
    | Sexpr -> 
	sexpr pp fmt (op, al)
    | _ -> 
	break := false;
	list ("", op, "") pp fmt al;
	break := true


(** {6 Redirecting Printers} *)

(** Redirecting output to [stdout] *)
let to_stdout pp = pp std_formatter

(** Redirecting output to [stderr] *)
let to_stderr pp = pp err_formatter

(** Redirecting output to a string *)
let to_string pp x = 
  pp str_formatter x;
  flush_str_formatter ()

(** Printing lists in Ocaml syntax. *)
let list pp = list ("[", "; ", "]") pp

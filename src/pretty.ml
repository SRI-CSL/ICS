(*
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
 *)

type 'a printer = Format.formatter -> 'a -> unit

let unit fmt () =
  Format.fprintf fmt "()"

let string fmt str =
  Format.fprintf fmt "%s" str

let number fmt i =
  Format.fprintf fmt "%d" i

let bool fmt = function
  | true ->
      Format.fprintf fmt "true"
  | false ->
      Format.fprintf fmt "false"

let option pp fmt = function
  | None -> 
      Format.fprintf fmt "None"
  | Some(x) -> 
      Format.fprintf fmt "Some(";
      pp fmt x;
      Format.fprintf fmt ")"
  

let final_sep = ref false

let list (pre,sep,post) pp fmt l =
  let rec iter = function
    | [] -> ()
    | [x] -> pp fmt x; if !final_sep then string fmt sep
    | x :: l -> pp fmt x; string fmt sep; Format.fprintf fmt " "; iter l
  in
  Format.fprintf fmt "@[%s" pre; 
  iter l; 
  Format.fprintf fmt "%s@]@?" post

let pair pp1 pp2 fmt (a,b) =
  Format.fprintf fmt "(";
  pp1 fmt a;
  string fmt ",";
  pp2 fmt b;
  Format.fprintf fmt ")@?"

let triple pp1 pp2 pp3 fmt (a,b,c) =
  Format.fprintf fmt "(";
  pp1 fmt a;
  string fmt ",";
  pp2 fmt b;
  string fmt ",";
  pp3 fmt c;
  Format.fprintf fmt ")@?"

let infix pp1 op pp2 fmt (a,b) =
  Format.fprintf fmt ""; 
  pp1 fmt a; 
  Format.fprintf fmt " %s " op; 
  pp2 fmt b;
  Format.fprintf fmt ""

let eqn pp = infix pp "=" pp

let infixl pp op =
  list ("", op, "") pp

let set pp fmt = list ("{", ", ", "}") pp fmt

let assign pp1 pp2 fmt (x,a) =
  Format.fprintf fmt "@[";
  pp1 fmt x; 
  string fmt " |-> "; 
  pp2 fmt a;
  Format.fprintf fmt "@]"

let map pp1 pp2 fmt =
  list ("[", "; ", "]") (assign pp1 pp2) fmt

let tuple pp = list ("(", ", ", ")") pp

let list pp = list ("[", "; ", "]") pp

let solution pp = list (eqn pp)

(** Redirecting output. *)

let to_stdout pp = pp Format.std_formatter

let to_stderr pp = pp Format.err_formatter

let to_string pp x = 
  pp Format.str_formatter x;
  Format.flush_str_formatter ()

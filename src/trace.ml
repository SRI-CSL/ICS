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

type level = int

let level = ref (Version.debug())

let reset () = (level := (Version.debug()))

let set i = (level := i)

let is_active j = j <= (Version.debug())
 
let indent = ref 0

let rec whitespace level n =
  if n >= 0 && is_active level then
    begin
      match n with
	| 0 -> ()
	| n -> (Format.eprintf " "; whitespace level (n - 1))
    end  

let call level op args pp =
  if is_active level then
    begin
      whitespace level !indent;
      indent := !indent + 1;
      Format.eprintf "%d: %s <-- " level op;
      pp Format.err_formatter args;
      Format.eprintf "@."
    end

let exit level op res pp =
  if is_active level then
    begin
      indent := !indent - 1;
      whitespace level !indent;
      Format.eprintf "%d: %s --> " level op;
      pp Format.err_formatter res;
      Format.eprintf "@."
    end

let exit0 level op  = 
 if is_active level then
   exit level op () Pretty.unit

let fail level name exc =
  if is_active level then
    begin
      indent := !indent - 1;
      whitespace level !indent;
      (if is_active level then
	 Format.eprintf "Exit %s: %s@." name (Printexc.to_string exc));
      raise exc
    end

let msg level op args pp =
  if is_active level then
    begin
      Format.eprintf "%d: %s\t" level op;
      pp Format.err_formatter args;
      Format.eprintf "@." 
    end

let func level name pp qq f a =
  if is_active level then
    begin
      try  
	call level name a pp;
	let b = f a in
	  exit level name b qq;
	  b
      with
	| exc -> raise exc (* fail level name exc *)
    end
  else
    f a

let func2 level name pp1 pp2 pp3 f a b =
  func level name (Pretty.pair pp1 pp2) pp3
    (fun (a, b) -> f a b) (a, b)

let proc = 
  let qq fmt () = Format.fprintf fmt "()" in 
    (fun level name pp -> func level name pp qq)

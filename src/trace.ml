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
 *)


type level = string

module Levels = Set.Make(
  struct
    type t = level
    let compare = Pervasives.compare
  end)

let levels = ref Levels.empty
let _ = Tools.add_at_reset (fun () -> levels := Levels.empty)

let reset () = (levels := Levels.empty)

let add l = (levels := Levels.add l !levels)

let remove l = (levels := Levels.remove l !levels)

let get () = Levels.elements !levels

let is_active l = 
  Levels.mem l !levels ||
  Levels.mem "all" !levels


let call level op args pp =
  if is_active level then
    begin
      Format.eprintf "%s: %s <-- " level op;
      pp Format.err_formatter args;
      Format.eprintf "@." 
    end

let exit level op res pp =
  if is_active level then
    begin
       Format.eprintf "%s: %s --> " level op;
       pp Format.err_formatter res;
       Format.eprintf "@."
    end

let msg level op args pp =
  if is_active level then
    begin
      Format.eprintf "%s: %s\t" level op;
      pp Format.err_formatter args;
      Format.eprintf "@." 
    end

let rec whitespace level n =
  if is_active level then
    begin
      match n with
	| 0 -> ()
	| n -> (Format.eprintf "%d " n; whitespace level (n - 1))
    end  

let indent = ref 0

let func level = 
  fun name pp qq f a ->
    try  
      whitespace level !indent;
      indent := !indent + 1;
      call level name a pp;
      let b = f a in
	indent := !indent - 1;
	whitespace level !indent;
	exit level name b qq;
	b
    with
      | exc -> 
	  begin
	    indent := !indent - 1;
	    whitespace level !indent;
	    (if is_active level then
	       Format.eprintf "Exit: %s@." (Printexc.to_string exc));
	    raise exc
	  end

let proc level = 
  let qq fmt () = Format.fprintf fmt "()" in 
    (fun name pp -> func level name pp qq)

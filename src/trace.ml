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

(** Rudimentary tracer. *)

type level = string

let registered = 
  ref [
    "all", "Enable all trace levels"; 
    "rule", "Trace top-level integration methods";
    "v", "Tracing modifications of variable equalities";
    "d", "Tracing modifications of variable diesequalities"
  ]

let _ =
  Th.iter
    (fun i ->
       let name = Th.to_string i in
       let pair = (name, "Tracing modifications in solution set " ^ name) in
	 registered := pair :: !registered)

let is_registered level =
  List.mem_assoc level !registered

let pp fmt () =
  Format.fprintf fmt "\nTrace levels: ";
  List.iter
    (fun (level, explanation) ->
       Format.fprintf fmt "\n%s : %s" level explanation)
    !registered

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
  if !levels == Levels.empty then false else 
    (Levels.mem l !levels ||
     Levels.mem "all" !levels)

let indent = ref 0

let rec whitespace level n =
  if is_active level then
    begin
      match n with
	| 0 -> ()
	| n -> (Format.eprintf "%d " n; whitespace level (n - 1))
    end  

let call level op args pp =
  if is_active level then
    begin
      whitespace level !indent;
      indent := !indent + 1;
      Format.eprintf "%s: %s <-- " level op;
      pp Format.err_formatter args;
      Format.eprintf "@." 
    end

let exit level op res pp =
  if is_active level then
    begin 
      indent := !indent - 1;
      whitespace level !indent;
       Format.eprintf "%s: %s --> " level op;
       pp Format.err_formatter res;
       Format.eprintf "@."
    end

let fail level name exc =
  indent := !indent - 1;
  whitespace level !indent;
  (if is_active level then
     Format.eprintf "Exit %s: %s@." name (Printexc.to_string exc));
  raise exc

let msg level op args pp =
  if is_active level then
    begin
      Format.eprintf "%s: %s\t" level op;
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
	| exc -> fail level name exc
    end
  else
    f a

let func2 level name pp1 pp2 pp3 f a b =
  func level name (Pretty.pair pp1 pp2) pp3
    (fun (a, b) -> f a b) (a, b)

let proc level = 
  let qq fmt () = Format.fprintf fmt "()" in 
    (fun name pp -> func level name pp qq)

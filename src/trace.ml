
(*i
 * ICS - Integrated Canonizer and Solver
 * Copyright (C) 2001-2004 SRI International
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the ICS license as published at www.icansolve.com
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * ICS License for more details.
 i*)

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

let rec whitespace = function
  | 0 -> ()
  | n -> (Format.eprintf "%d " n; whitespace (n - 1))
  

let func level = 
  let indent = ref 0 in
    fun name pp qq f a ->
      try  
	whitespace !indent;
	indent := !indent + 1;
	call level name a pp;
	let b = f a in
	  indent := !indent - 1;
	  whitespace !indent;
	  exit level name b qq;
	  b
      with
	| exc -> 
	    begin
	      indent := !indent - 1;
	      whitespace !indent;
	      (if is_active level then
		 Format.eprintf "Exit: %s@." (Printexc.to_string exc));
	      raise exc
	    end

let proc level = 
  let qq fmt () = Format.fprintf fmt "()" in 
    (fun name pp -> func level name pp qq)

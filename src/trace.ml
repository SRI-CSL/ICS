
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

type 'a pp = Format.formatter -> 'a -> unit   (* pretty-printer type *)

let fmt = Format.std_formatter
	    
let indent = ref 0

let whitespace n =
  let rec loop = function
    | 0 -> ()
    | k -> Format.fprintf fmt " "; loop (k - 1)
  in
  loop n

let init () =
  indent := 0

let call trace_level op args pp =
  if Tools.get_verbose () >= trace_level then
    begin
      whitespace !indent;
      Format.fprintf fmt "%s: " op;
      pp fmt args;
      indent := !indent + 1;
      Format.fprintf fmt "@."
    
    end

let exit trace_level op res pp =
  if Tools.get_verbose () >= trace_level then
    begin
       indent := !indent - 1;
       whitespace !indent;
       Format.fprintf fmt "<-- ";
       pp fmt res;
       Format.fprintf fmt "@."
    end

let ok trace_level op =
  exit trace_level op "ok" (fun fmt s -> Format.fprintf fmt s)
    

let exc trace_level op res pp =
  let str = Tools.pp_to_string (fun fmt e ->
				  Format.fprintf fmt "Exception %s for" op;
				  pp fmt res)
	      res
  in
  if Tools.get_verbose () >= trace_level then
    begin
      whitespace !indent;
      Format.fprintf fmt "%s" str;
      indent := 0;
      Format.fprintf fmt "@.";
    end
    







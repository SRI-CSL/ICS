
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



(*s Verbose Level *)

let verbose_level = ref 0
let _ = Tools.add_at_reset (fun () -> verbose_level := 0)

let set_verbose n = verbose_level := n

let get_verbose () = !verbose_level

let verbose n f x =
  if !verbose_level >= n then begin f x; Format.print_flush () end




type 'a pp = Format.formatter -> 'a -> unit   (* pretty-printer type *)

let fmt = Format.std_formatter
	

let whitespace n =
  let rec loop = function
    | 0 -> ()
    | k -> Format.fprintf fmt " "; loop (k - 1)
  in
  loop n

let init () = ()

let call trace_level op args pp =
  if get_verbose () >= trace_level then
    begin
      Format.fprintf fmt "%s <-- " op;
      pp fmt args;
      Format.fprintf fmt "@." 
    end

let exit trace_level op res pp =
  if get_verbose () >= trace_level then
    begin
       Format.fprintf fmt "%s --> " op;
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
  if get_verbose () >= trace_level then
    begin
      Format.fprintf fmt "%s" str;
      Format.fprintf fmt "@.";
    end

let msg trace_level op args pp =
  if get_verbose () >= trace_level then
    begin
      Format.fprintf fmt "%s:\t" op;
      pp fmt args;
      Format.fprintf fmt "@." 
    end
    









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
 * Author: Jean-Christophe Filliatre, Harald Ruess
 i*)

(*s Functions to run at exit. *)

let at_exit_functions = ref []

let add_at_exit f = 
  at_exit_functions := f :: !at_exit_functions

let do_at_exit () = 
  List.iter (fun f -> f()) (List.rev !at_exit_functions)


(*s Functions to run at reset. *)

let at_reset_functions = ref []

let add_at_reset f = 
  at_reset_functions := f :: !at_reset_functions

let do_at_reset () =
  List.iter (fun f -> f()) (List.rev !at_reset_functions)


(*s Verbose. *)

let verbose_level = ref 0
let _ = add_at_reset (fun () -> verbose_level := 0)

let set_verbose n = verbose_level := n

let get_verbose () = !verbose_level

let verbose n f x =
  if !verbose_level >= n then begin f x; Format.print_flush () end


(*s Timing functions. *)

open Unix

let utime f x =                                                   
  let u = (times()).tms_utime in                                  
  let y = f x in
  let ut = (times()).tms_utime -. u in
  (y,ut)

let timers = ref [0.]

let profile str f =
  let timer = ref 0. in
  let calls = ref 0 in
  add_at_exit
    (fun () -> Format.printf "%s: utime = %f  calls = %d\n@ " str !timer !calls);
  fun x ->
    let start = (Unix.times()).tms_utime in
    let y = f x in
    let finish = (Unix.times()).tms_utime in
    timer := !timer +. (finish -. start);
    calls := !calls + 1;
    y

(*s Print to a string *)

let pp_to_string pp x =
  pp Format.str_formatter x;
  Format.flush_str_formatter ()


(*s Type for comparison. *)

type cmp = Less | Equal | Greater
    

(*s Mapping over list of terms. Avoids unnecessary consing. *)

let rec mapl f l =
  match l with
    | [] -> []
    | a :: l1 ->
	let a' = f a and l1' = mapl f l1 in
	if a' == a && l1 == l1' then l else a' :: l1'


(*s Printing a list. *)        

let ppl (pre,sep,post) pp fmt l =
  let rec iter = function
    | [] -> ()
    | [x] -> pp fmt x
    | x :: l -> pp fmt x; Format.fprintf fmt "%s " sep; iter l
  in
  Format.fprintf fmt "@[%s" pre;
  iter l;
  Format.fprintf fmt "%s@]" post

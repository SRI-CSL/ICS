
(*i*)
open Ics
open Tools
open Format
(*i*)

let verbose n =
  Tools.set_verbose n

(*s Pretty-printers with newlines. *)

let endline () = printf "\n"; print_flush ()

let with_nl pp x = pp x; endline ()
 
let pp_term_nl = with_nl pp_term
let pp_mapsto (x,y) = pp_term x; printf " |-> "; pp_term y
let pp_mapsto_nl = with_nl pp_mapsto

(*s The state of the toplevel is composed of a mapping (type [seqns]) and 
    a list of dis-equalities. *)
		     
let current = ref (empty_state())

let state () = !current

let rollback f x =
  let s = !current in
  try
    f x
  with
      Exc.Inconsistent _ ->
	current := s;
	raise (Exc.Inconsistent "")
	     
(*s Canonizer test (command [sigma]). *)

let sigma t =
  pp_term_nl t; print_flush ()

(*s Solver test (command [solve]). *)
    
let solve e =
  let e' = canon !current (fst e), canon !current (snd e) in
  try
    let s = solve !current e' in
    List.iter pp_mapsto_nl s
  with
      Exc.Inconsistent _ -> printf "F"; endline ()

(*s The command [do_assert] introduces a new atom, which is either an equality
    or a dis-equality. The state is left unchanged is an inconsistency is 
    discovered.  *)

let change_state f =
  match f !current with
    | Dp.Consistent st -> current := st
    | Dp.Valid -> print_string "Redundant"; endline ()
    | Dp.Inconsistent -> raise (Exc.Inconsistent "")
  
let process a = change_state (fun st -> process st a)

let processl l = rollback (List.iter process) l
	  
(*s Other commands. *)
			 
let reset () =
  Tools.do_at_reset ();
  current := (empty_state())

let drop () = failwith "drop"
			 
let compare (t1,t2) =
  let cmp = Ics.compare t1 t2 in
  print_string (if cmp = 0 then "=" else if cmp < 0 then "<" else ">");
  endline ()

let find = function
  | Some(t) -> let t' = find !current t in pp_term_nl t'
  | None -> Ics.pp_find !current

let use = function
  | Some(t) ->
      let ts = use !current t in
      printf "{"; Pretty.list pp_term ts; printf "}";
      endline ()
  | None ->
      Ics.pp_use !current

let polarity t =
  Ics.polarity !current t

let universe = function
  | Some(t) -> 
      if universe !current t then
	printf "true"
      else
	printf "false";
      endline ()
  | None ->
      Ics.pp_universe !current

let can t =
  let t' = canon !current t in pp_term_nl t'
				 
let norm t =
  let t' = norm !current t in pp_term_nl t'
				 
let check t =
 (match Ics.process !current t with
    | Dp.Valid -> printf "T"
    | Dp.Inconsistent ->  printf "F"
    | Dp.Consistent _ -> printf "X");
  endline ()







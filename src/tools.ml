

(*s Functions to run at exit. *)

let at_exit_functions = ref []

let add_at_exit f = at_exit_functions := f :: !at_exit_functions

let do_at_exit () = List.iter (fun f -> f()) (List.rev !at_exit_functions)

(*s Functions to run at reset. *)

let at_reset_functions = ref []

let add_at_reset f = at_reset_functions := f :: !at_reset_functions

let do_at_reset () = List.iter (fun f -> f()) (List.rev !at_reset_functions)

(*s Verbose. *)

let verbose_level = ref 0

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


(*s Comparison function similar to Pervasives.compare but not recursive. *)

let gen_compare f v1 v2 =
  let o1 = Obj.repr v1
  and o2 = Obj.repr v2 in
  if Obj.is_int o1 then
    if Obj.is_int o2 then
      (Obj.magic v1 : int) - (Obj.magic v2 : int)
    else 
      -1
  else if Obj.is_int o2 then
    1
  else
    let tag1 = Obj.tag o1 
    and tag2 = Obj.tag o2 in
    if tag1 != tag2 then
      tag1 - tag2
    else
      f (v1,v2)

(*s Various comparisons. *)

let compare_string s1 s2 =
  let c = String.length s1 - String.length s2 in
  if c != 0 then c else Pervasives.compare s1 s2

let compare_list compare_elt l1 l2 =
  let rec comp c = function
    | [], [] -> c
    | [], _  -> -1
    | _,  [] -> 1
    | t1::l1, t2::l2 -> 
	if c != 0 then comp c (l1,l2) else comp (compare_elt t1 t2) (l1,l2)
  in
  comp 0 (l1,l2)

let compare_array compare_elt a1 a2 =
  let n1 = Array.length a1
  and n2 = Array.length a2 in
  let rec compare_rec i =
    if i = n1 then 
      0 
    else 
      let c = compare_elt a1.(i) a2.(i) in
      if c != 0 then c else compare_rec (succ i)
  in
  if n1 != n2 then n1 - n2 else compare_rec 0

(*s Lexicographic comparison. *)

let lexico2 fu u1 u2 fv v1 v2 =
  let c = fu u1 u2 in
  if c != 0 then c else fv v1 v2

let rec lexico = function
  | [] -> 0
  | (f,x,y) :: l -> let c = f x y in if c != 0 then c else lexico l

let lexico3 f1 u1 v1 f2 u2 v2 f3 u3 v3 =
  let c = f1 u1 v1 in
  if c != 0 then c else
    let d = f2 u2 v2 in
    if d != 0 then d else f3 u3 v3

let lexico4 f1 u1 v1 f2 u2 v2 f3 u3 v3 f4 u4 v4 =
  let c = f1 u1 v1 in
  if c != 0 then c else
    let d = f2 u2 v2 in
    if d != 0 then d else
      let e = f3 u3 v3 in
      if e != 0 then e else f4 u4 v4

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

let verbose n f x =
  if !verbose_level >= n then begin f x; Format.print_flush () end

(*s Timing functions. *)

open Unix

let utime f x =                                                   
  let u = (times()).tms_utime in                                  
  let y = f x in
  let ut = (times()).tms_utime -. u in
  (y,ut)

let timers = ref []

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



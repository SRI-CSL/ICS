
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
 * Author: Harald Ruess, N. Shankar
i*)

(*i*)
open Term
open Three
(*i*)

let is_cod a =
  not(is_var a) &&
  List.for_all is_var (args_of a)

(*s If [modulo_array_theory] is true, then [mk_select] and [mk_update]
 are treated in an interpreted way. *)

let interpreted_array_theory = ref false


(*s Equality contexts *)

module U = Subst.Make(
  struct
    let name = "u"
    let fold f a =
      assert(is_cod a);
      List.fold_right f (args_of a)
    let map f a =
      assert(is_cod a);
      App.lazy_sigma a (sym_of a) (Term.mapl f (args_of a))
  end)


type t = { 
  v : V.t;             (* Variable partitioning. *)
  u : U.t              (* Equalities for uninterpreted terms. *)
}

(*s Accessors. *)

let partition s = V.partition s.v

let diseqs s = V.diseqs s.v

let solution s = U.solution s.u


(*s Pretty-printing. *)

let pp fmt s = 
  V.pp fmt s.v; 
  if not(s.u == U.empty) then
    begin
      Pretty.string fmt "\nu:";
      U.pp fmt s.u
    end

(*s [d_select s a] succeeds when [a] is equal to 
 the pattern [select(x, y)] in [s]. *)

let d_select a =
  if is_var a then raise Not_found  else
    match destruct a with
      | f, [x;y] when Sym.eq f Sym.mk_select -> (x,y)
      | _ -> raise Not_found

(*s [d_update s a] succeeds when [a] is equal to 
 the pattern [update(x, y,z)] in [s]. *)

let d_update a =
  if is_var a then raise Not_found else
    match destruct a with
      | f, [x;y;z] when Sym.eq f Sym.mk_update -> (x,y,z)
      | _ -> raise Not_found


(*s Find and inverse find. *)

let apply s = U.apply s.u
let find s = U.find s.u
let use s = U.use s.u

(*s Variable equality and disequality. *)

let veq s = V.eq s.v

let vdeq s = V.deq s.v

(*s Variable equality modulo [select(update(a,i,x), i) = x]. *)

let rec vequal s x y =
  veq s x y ||
  (!interpreted_array_theory &&
   (select_update_eq s x y ||
    select_update_eq s y x))

(*s [x] is also equivalent to [y] if [x' |-> select(upd,j)],
 [upd |-> update(_, i, y')], [x = x'], [y = y'], and [i = j]. *)

and select_update_eq s x y =
  Set.exists
    (fun x' ->
       (vequal s x x') &&
       (try
	  let (upd, j) = d_select (apply s x') in
	  let (a, i, y') = d_update (apply s upd) in 
	  vequal s y y' &&     (* Note: should use some [upd'] equiv to [upd]. *)
	  vequal s i j
	with
	    Not_found -> false))
    (use s y)


(*s Canonical representative of a variable. *)

let v s = V.find s.v

let v' s x =
  let (v', y) = V.find' s.v x in
  ({s with v = v'}, y)


(*s [congruent s a b] holds if [a] is of the form [f(x1,...,xn)]
 and [b] is of the form [f(y1,...,yn)] and the respective variables
 [xi] and [yi] are variables in the same equivalence class. *)


let congruent s a b =
  assert(not(is_var a) && not(is_var b));
  Sym.eq (sym_of a) (sym_of b) &&
  try 
    List.for_all2 (vequal s) (args_of a) (args_of b) 
  with 
      Invalid_argument _ -> false


(*s [a] and [b] are also congruent modulo [s] if [a]
 is of the form [select(update(x, i, _),j)], [b] is 
 of the form [select(x',j')] and [x = x'], [i <> j], [j = j']. *)

let select_update_diseq s a b =
  try
    let (upd, j) = d_select (find s a) in
    let (x, i, _) = d_update upd in
    let (x',j') = d_select (find s b) in
    veq s x x' && vdeq s i j && veq s j j'
  with
      Not_found -> false

let congruent_modulo s a b =
  congruent s a b ||
  (!interpreted_array_theory &&    (* arrays only interpreted when switch *)
   (select_update_diseq s a b ||   (* is on. *)
    select_update_diseq s b a))


(*s Choose an element in set [s] which satisfies [p] *)

exception Found of Term.t

let choose p xs =
  try
    Set.iter (fun x -> if p x then raise (Found x)) xs;
    raise Not_found
  with   
     Found(x) -> x


(*s Inverse lookup. *)

let rec inv s a = 
  if is_var a then 
    v s a
  else 
    if args_of a = [] then
      v s (U.inv s.u a)
    else 
      v s (choose
	     (fun y ->
		try congruent_modulo s a (apply s y) with Not_found -> false)
	     (use s (List.hd (args_of a))))


(*s Canonization *)

let rec can s a =
  if is_var a then
    v s a
  else  
    let f, l = Term.destruct a in
    let b = mk_app f (Term.mapl (can s) l) in
    try v s (inv s b) with Not_found -> b



(*s Check if two terms are known to be equal. *)

let is_equal s a b =
  V.is_equal s.v (can s a) (can s b)


(*s The empty context. *)

let empty = {
  v = V.empty;
  u = U.empty;
}

(*s Merging two equivalence classes. Make sure that whenever 
 [x |-> y] in [v], then [use s x] is a subset of [use s y]. *)

let rec union e s =
  Trace.msg "u" "Union" e Veq.pp;
  let v' = V.merge e s.v in
  let u' = adduse e s.u in
  {s with v = v'; u = u'}

and adduse e u = 
  let (x, y) = Veq.destruct e in
  let ux = U.use u x in
  let uy = U.use u y in
  let uxy = Set.union ux uy in
  if uy == uxy then u else U.adduse y uxy u

    


(*s Fresh labels *)

let labels = ref Set.empty
let _ = Tools.add_at_reset (fun () -> labels := Set.empty)

let is_label x = Set.mem x !labels


(*s Adding an entry [c |-> f(c1,...,cn)] for [c] fresh. *)

let extend a s =
  Trace.call "u" "Extend" a Term.pp;
  let (c, u') = U.extend a s.u in
  labels := Set.add c !labels;
  (c, {s with u = u'})

(*s Removing a binding [x |-> b] in [s]. *)

let restrict x s = 
  Trace.msg "u" "Restrict" x Term.pp;
  {s with u = U.restrict x s.u}


(*s Adding a disequality. *)

let diseq (x, y) s =
  assert(is_var x && is_var y);
  match is_equal s x y with
    | Yes -> raise Exc.Inconsistent
    | No -> (s, Veqs.empty)
    | X -> ({s with v = V.diseq (x, y) s.v}, Veqs.empty)


(*s Merging of equalities [(a,b)] and congruence-close. *)

	

let rec merge e s =
  Trace.call "u" "Merge" e Veq.pp;
  let (s', veqs') = merge1 e (s, Veqs.empty) in
  let veqs'' = Veqs.remove e veqs' in
  Trace.exit "u" "Merge" veqs'' Veqs.pp;
  (s', veqs'')

and merge1 e (s, veqs) = 
  let (x, y) = Veq.destruct e in
  match V.is_equal s.v x y with
    | Yes -> (s, veqs)
    | No -> raise Exc.Inconsistent
    | _ ->
	Set.fold
	  (fun x1 (s1, veqs1) ->
	     Set.fold
	       (fun x2 (s2, veqs2) ->
		  if congruent_modulo s2 (find s2 x1) (find s2 x2) then 
		    let e' = Veq.make (v s2 x1) (v s2 x2) in
		    merge1 e' (s2, veqs2)
		  else 
		    (s2, veqs2))
	       (use s1 y)
	       (s1, veqs1))
	  (use s x)
	  (union e s, Veqs.add e veqs)
  


(*s Compression by instantiating variables on both the lhs and rhs of [u]
 with their canonical representatives w.r.t. to [v] *)

let compress s = 
  Trace.msg "u" "Compress" "disabled" Pretty.string;
  s




(*s Suggested splits on ['a[x:=y][z]'], when 
 [x = z] is not already known. *)

let rec split s =
  U.fold (fun _ -> select_update_splits s) s.u []

and select_update_splits s =
  Term.fold
    (fun x acc ->
       try
	 let (upd, j) = d_select (apply s x) in
	 let (a, i, y) = d_update (apply s upd) in
	 match is_equal s x y with
	   | X -> Atom.mk_equal x y :: acc
	   | Yes | No -> acc
       with
	   Not_found -> acc)

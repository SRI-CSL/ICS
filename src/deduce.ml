
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
 * Author: Harald Ruess
 i*)

(*i*)
open Term
(*i*)


(*s Propagate constraints for [b in i] for each variable [x] in [b].
  Suppose [b] is of the form [pre + q * x + post'], then
  [x in 1/q * (i - (j + k))] is derived, where [pre in j] and
  [post' in k]. Following should be optimized. *)


let propagate b i (v, c) =
  let rec loop j post c = 
    match post with
      | [] -> c
      | m :: post' ->
	  try
	    let (q, x) = Arith.mono_of m in
	    let k = C.of_term (v, c) (Arith.mk_addl post') in
	    let j' = Cnstrnt.add (Cnstrnt.multq q (C.of_term (v, c) x)) j in
            let i' = Cnstrnt.multq (Mpa.Q.inv q) (Cnstrnt.subtract i (Cnstrnt.add j k)) in
	    let c' = C.add (Fact.mk_cnstrnt x i' None) c in
	    loop j' post' c'
	  with
	      Not_found -> c  (* should not happen. *)
  in
  loop Cnstrnt.mk_zero (Arith.monomials b) c
  

let extend (x, b) (v, c) =
  try
    let j = C.of_term (v, c) b in
      C.add (Fact.mk_cnstrnt x j None) c
  with
      Not_found -> c


(*s Deduce new constraints from an equality of the form [x = b],
 where [x] is a variable. *)


let rec deduce eqn ((v, c, e) as s) =
  let (x, b, _) = Fact.d_equal eqn in
  try
    let i = C.apply c (V.find v x) in                                  
      match partition (v, c) b with
	| Some(_, j), None ->
	    (match Cnstrnt.cmp i j with
	       | Binrel.Disjoint -> 
		   raise Exc.Inconsistent
	       | Binrel.Same -> 
		   s
	       | Binrel.Sub -> 
		   let c' = propagate b i (v, c) in
		     (v, c', e)
	       | Binrel.Super -> 
		   let c' =  propagate b j (v, c) in
		   let c'' = C.add (Fact.mk_cnstrnt x j None) c' in
		     (v, c'', e)
	       | Binrel.Overlap(ij) -> 
		   let c' = propagate b ij (v, c) in
		   let c'' = C.add (Fact.mk_cnstrnt x ij None) c' in
		     (v, c'', e)
	       | Binrel.Singleton(q) ->
		   let k = Cnstrnt.mk_singleton q in
		   let c' = propagate b k (v, c) in
		   let c'' = C.add (Fact.mk_cnstrnt x k None) c' in
		     (v, c'', e))
	| Some(_, j), Some(b'')  ->  
	    let k = Cnstrnt.subtract i j in
	    let (x', e') = 
	      if is_var b'' then 
		(b'', e) 
	      else                        (* Add [x' = b''] to [e], where *)
		Solution.name (b'', e)    (* [x'] might be a fresh variable. *)
	    in
	    let c' = C.add (Fact.mk_cnstrnt x' k None) c in
	      (v, c', e')
	| _ -> 
	    (v, c, e)
      with
	  Not_found ->
	    let c' = extend (x, b) (v, c) in
	      (v, c', e)



(*s Split a term into the part with constraints and the unconstraint part.
 Also, return the constraint for the term with a constraint. *)

and partition (v, c) a =
  let cnstrnt = C.of_term (v, c) in
  let (constr, unconstr) = 
    List.fold_right
      (fun x (constr, unconstr) ->
	 try
	   let i = cnstrnt x in
	   let constr' = match constr with
	     | None -> Some([x], i)
	     | Some(yl, j) -> Some(x :: yl, Cnstrnt.add i j)
	   in
	     (constr', unconstr)
	 with
	     Not_found ->
	       let unconstr' = match unconstr with
		 | None -> Some([x])
		 | Some(yl) -> Some(x :: yl)
	       in
		 (constr, unconstr'))
      (Arith.monomials a)
      (None, None)
  in
  let constr' = match constr with
    | Some(xl, i) -> Some(Arith.mk_addl xl, i)
    | None -> None
  and unconstr' = match unconstr with
    | Some(yl) -> Some(Arith.mk_addl yl)
    | None -> None
  in
    (constr', unconstr')

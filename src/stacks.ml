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

(** Datatype of stacks

  @author Harald Ruess
*)


type 'a t = {
  mutable arr : 'a array;
  mutable top : int
}

let length s = s.top + 1

exception Empty

let initial_size = 2

let create () = {
  arr = Array.make initial_size (Obj.magic 0);
  top = -1;
}

let clear s = (s.top <- (-1))

let is_empty s = (s.top == -1)

let rec push x s = 
  if s.top >= Array.length s.arr then
    resize s;
  assert(s.top < Array.length s.arr); 
  s.top <- s.top + 1;
  Array.unsafe_set s.arr s.top x

and resize s =
  let l' = 2 * (Array.length s.arr) in
  let arr' = Array.create l' (Obj.magic 0) in
    for i = 0 to s.top do 
      Array.unsafe_set arr' i (Array.unsafe_get s.arr i) 
    done;
    s.arr <- arr'

let pop s =
  if s.top < 0 then raise Empty else
    let x = Array.unsafe_get s.arr s.top in
      s.top <- s.top - 1;
      x

let iter f s = 
  for i = 0 to s.top do
    f (Array.unsafe_get s.arr i)
  done

let to_list s = 
  let l = ref [] in
    for i = 0 to s.top do
      l := Array.unsafe_get s.arr i :: !l
    done;
    !l

exception Yes

let mem eq a s =
  try
    for i = s.top downto 0 do
      if eq a (Array.unsafe_get s.arr i) then
	raise Yes
    done;
    false
  with
      Yes -> true


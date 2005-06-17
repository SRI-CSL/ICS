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
  mutable top : int;
  mutable size: int
}

let well_formed s = 
  s.top < s.size &&
  s.size = Array.length s.arr

let length s = s.top + 1

exception Empty

let initial_size = 4

let create () = 
  let s = {
    arr = Array.make initial_size (Obj.magic 0);
    top = -1;
    size = initial_size
  }
  in
    assert(well_formed s);
    s

let clear s = 
  assert(well_formed s);
  (s.top <- (-1))

let is_empty s =
  assert(well_formed s);
  (s.top == -1)

let top s = 
  assert(well_formed s);
  if is_empty s then raise Empty else
    Array.get s.arr s.top

let rec push x s = 
  assert(well_formed s);
  if s.top = s.size - 1 then resize s;
  s.top <- s.top + 1; 
  assert(s.top < s.size); 
  Array.set s.arr s.top x

and resize s =
  assert(well_formed s);
  let size' = 2 * s.size in
  let arr' = Array.create size' (Obj.magic 0) in
    assert(s.top < size');
    for i = 0 to s.top do
      Array.set arr' i (Array.get s.arr i)
    done;
    s.arr <- arr';
    s.size <- size';
    assert(well_formed s);
    assert(s.top < s.size - 1)

let pop s =
  assert(well_formed s);
  if s.top < 0 then raise Empty else
    let x = Array.get s.arr s.top in
      s.top <- s.top - 1;
      x

let iter f s = 
  assert(well_formed s);
  for i = 0 to s.top do
    f (Array.get s.arr i)
  done

let to_list s = 
  assert(well_formed s);
  let l = ref [] in
    for i = 0 to s.top do
      l := Array.get s.arr i :: !l
    done;
    !l

let map f s =
  assert(well_formed s);
  for i = 0 to s.top do
    let a = Array.get s.arr i in
    let b = f a in
      if a == b then () else
	Array.set s.arr i b
  done

exception Mem
let mem eq a s =
  assert(well_formed s);
  try
    for i = s.top downto 0 do
      if eq a (Array.get s.arr i) then
	raise Mem
    done;
    false
  with
      Mem -> true

exception Counterexample
let for_all p s =   
  assert(well_formed s);
  try
    for i = s.top downto 0 do
      if not (p (Array.get s.arr i)) then
	raise Counterexample
    done;
    true
  with
      Counterexample -> false

let sort cmp s = 
  Array.sort cmp s.arr
  

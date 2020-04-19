(* 
 * The MIT License (MIT)
 *
 * Copyright (c) 2020 SRI International
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 *)

(** Datatype of stacks

    @author Harald Ruess *)

type 'a t = {mutable arr: 'a array; mutable top: int; mutable size: int}

let well_formed s = s.top < s.size && s.size = Array.length s.arr
let length s = s.top + 1

exception Empty

let initial_size = 4

let create () =
  let s =
    {arr= Array.make initial_size (Obj.magic 0); top= -1; size= initial_size}
  in
  assert (well_formed s) ;
  s

let clear s =
  assert (well_formed s) ;
  s.top <- -1

let is_empty s =
  assert (well_formed s) ;
  s.top == -1

let top s =
  assert (well_formed s) ;
  if is_empty s then raise Empty else s.arr.(s.top)

let rec push x s =
  assert (well_formed s) ;
  if s.top = s.size - 1 then resize s ;
  s.top <- s.top + 1 ;
  assert (s.top < s.size) ;
  s.arr.(s.top) <- x

and resize s =
  assert (well_formed s) ;
  let size' = 2 * s.size in
  let arr' = Array.make size' (Obj.magic 0) in
  assert (s.top < size') ;
  for i = 0 to s.top do
    arr'.(i) <- s.arr.(i)
  done ;
  s.arr <- arr' ;
  s.size <- size' ;
  assert (well_formed s) ;
  assert (s.top < s.size - 1)

let pop s =
  assert (well_formed s) ;
  if s.top < 0 then raise Empty
  else
    let x = s.arr.(s.top) in
    s.top <- s.top - 1 ;
    x

let iter f s =
  assert (well_formed s) ;
  for i = 0 to s.top do
    f s.arr.(i)
  done

let to_list s =
  assert (well_formed s) ;
  let l = ref [] in
  for i = 0 to s.top do
    l := s.arr.(i) :: !l
  done ;
  !l

let map f s =
  assert (well_formed s) ;
  for i = 0 to s.top do
    let a = s.arr.(i) in
    let b = f a in
    if a == b then () else s.arr.(i) <- b
  done

exception Mem

let mem eq a s =
  assert (well_formed s) ;
  try
    for i = s.top downto 0 do
      if eq a s.arr.(i) then raise Mem
    done ;
    false
  with Mem -> true

exception Counterexample

let for_all p s =
  assert (well_formed s) ;
  try
    for i = s.top downto 0 do
      if not (p s.arr.(i)) then raise Counterexample
    done ;
    true
  with Counterexample -> false

let sort cmp s = Array.sort cmp s.arr

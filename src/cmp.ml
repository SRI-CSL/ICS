
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
 *)

(*s Comparison function similar to Pervasives.compare but not recursive. *)

let generic f v1 v2 =
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

let string s1 s2 =
  let c = String.length s1 - String.length s2 in
  if c != 0 then c else Pervasives.compare s1 s2

let list cmp l1 l2 =
  let rec loop c = function
    | [], [] -> c
    | [], _  -> -1
    | _,  [] -> 1
    | t1::l1, t2::l2 -> 
	if c != 0 then loop c (l1,l2) else loop (cmp t1 t2) (l1,l2)
  in
  loop 0 (l1,l2)

let array cmp a1 a2 =
  let n1 = Array.length a1
  and n2 = Array.length a2 in
  let rec loop i =
    if i = n1 then 
      0 
    else 
      let c = cmp a1.(i) a2.(i) in
      if c != 0 then c else loop (succ i)
  in
  if n1 != n2 then n1 - n2 else loop 0

(*s Lexicographic comparisons. *)

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

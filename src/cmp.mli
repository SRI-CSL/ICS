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
 i*)

(*s Various comparison functions. For a comparison [cmp a b],
    the result [-1] is interpreted to mean [a] is less than [b],
    [0] denotes equality of the arguments, and [1] indicates that
    [a] is greater than [b].
  *)

    (*s Comparison function similar to Pervasives.compare
      but not recursive.  The function to handle the
      non-generic cases (i.e. the recursive call on constructors'
      arguments) is provided as the first argument. *)
val generic : ('a * 'a -> int) -> 'a -> 'a -> int

    (*s Lexicographic ordering on lists, given a comparison on the element type. *)
val list : ('a -> 'a -> int) -> 'a list -> 'a list -> int

    (*s Lexicographic ordering on arrays, given a comparison on the element type *)
val array : ('a -> 'a -> int) -> 'a array -> 'a array -> int

    (*s Lexicographic ordering on pairs, given comparisons for the first and
      the second component. *)
val lexico2 : ('a -> 'a -> int) -> 'a -> 'a -> 
              ('b -> 'b -> int) -> 'b -> 'b -> int

    (*s Lexicographic ordering on triples. *)	  
val lexico3 : ('a -> 'a -> int) -> 'a -> 'a -> 
              ('b -> 'b -> int) -> 'b -> 'b ->
	      ('c -> 'c -> int) -> 'c -> 'c -> int

    (*s Lexicographic ordering on quadruples. *)	   
val lexico4 : ('a -> 'a -> int) -> 'a -> 'a -> 
              ('b -> 'b -> int) -> 'b -> 'b ->
	      ('c -> 'c -> int) -> 'c -> 'c ->
	      ('d -> 'd -> int) -> 'd -> 'd -> int

    (*s Lexicographic ordering on n-tuples. *)
val lexico : (('a -> 'a -> int) * 'a * 'a) list -> int

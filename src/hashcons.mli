
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

(*s Module [Hashcons]: hashconsing over types with equality. *)

(*s This module implements hashconsing of types with equalities
  by injecting elements [a] of this type into a record  consisting of
  the node [a] itself, a unique integer tag, and a hash key for [a].

  The main advantage of hashconsing is that equality is reduced to
  a constant time operation.  On the other hand, the penalty to
  pay is that every entity has to be hashconsed.  Besides the
  time overhead there is also a space overhead for the storage of
  additional information in hashed elements and the use of global
  hash tables. These elements are never being garbage collected,
  since they are all kept in a hash table.
 *)

type 'a hashed = { 
  hkey : int;
  tag : int;
  node : 'a }

(*s Equality for hashconsed entities reduces to identity, and
  can thus be performed in constant time. *)
		   
val (===) : 'a hashed -> 'a hashed -> bool 


(* The input signature of the functor [Hashcons.Make].
   [t] is the type of the elements to be hashconsed.
   [equal] specifies the equality relation for hashconsing,
   and [hash] is a function for computing hash keys.
   Example: a suitable hashc function is often 
   the generic hash function [hash]. *)    

module type HashedType =
  sig
    type t
    val equal : t -> t -> bool
    val hash : t -> int
  end

  
module type S =
  sig
    type key

      (*s The type of hash tables for hashconsing elements of type [key]. *)
    type t

      (*s [create n] creates a new, empty hash table, with
	initial size [n].  For best results, [n] should be on the
	order of the expected number of elements that will be in
	the table.  The table grows as needed, so [n] is just an
	initial guess. *)
    val create : int -> t
	
	(*s Empty a hash table. *)
    val clear : t -> unit
	
	(*s Given a table [t] and a node [a], [hashcons t a] returns
	  a hashconsing record for [a] with a unique tag. *)
    val hashcons : t -> key -> key hashed


	(*s [iter f t] applies [f] in turn to all hashconsed elements of [t].
	  The order in which the elements of [t] are presented to [f] is unspecified. *)
    val iter : (key hashed -> unit) -> t -> unit

	(*s Prints on standard output some statistics for hash table [t] such as
	  percentige of used entries, and maximum bucket length. *)
    val stat : t -> unit
  end

  
  (*s Functor building an implementation of the hashcons structure
    given a structure of signature [HashedType]. *)

module Make(H : HashedType) : (S with type key = H.t)

















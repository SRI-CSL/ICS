
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

type t = string

let ht = Hashtbl.create 17            (*s Hashconsing of strings. *)
let _ = Tools.add_at_reset (fun () -> Hashtbl.clear ht)

let of_string (s: string) = 
 (* assert(String.get s 0 <> '_'); *)     (* reserved for 'fresh' variables *)
  try
    Hashtbl.find ht s
  with
      Not_found ->
	Hashtbl.add ht s s; s

let to_string s = s

let eq = (==)

let cmp n m =
  Pervasives.compare n m

let pp fmt s =
  Format.fprintf fmt "%s" s

let hash = Hashtbl.hash

type name = t  (* avoid type-check error below *)

module Set = Set.Make(
  struct
    type t = name
    let compare = Pervasives.compare
  end)

module Map = Map.Make(
  struct
    type t = name
    let compare = Pervasives.compare
  end)

let pp_map p fmt =
  Map.iter
    (fun x y ->
       pp fmt x;
       Format.fprintf fmt " |-> ";
       p fmt y;
       Format.fprintf fmt "\n")


(*
 * Ptmap: Maps over integers implemented as Patricia trees.
 * Copyright (C) 2000 Jean-Christophe FILLIATRE
 * 
 * This software is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License version 2, as published by the Free Software Foundation.
 * 
 * This software is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * 
 * See the GNU Library General Public License version 2 for more details
 * (enclosed in the file LGPL).
 *)

(*i $Id: ptmap.mli,v 1.7 2003/07/31 01:19:51 ruess Exp $ i*)

(*s Maps over integers implemented as Patricia trees.
    The following signature is exactly [Map.S with type key = int],
    with the same specifications. *)

type (+'a) t

type key = int

val empty : 'a t

val add : int -> 'a -> 'a t -> 'a t

val find : int -> 'a t -> 'a

val remove : int -> 'a t -> 'a t

val mem :  int -> 'a t -> bool

val iter : (int -> 'a -> unit) -> 'a t -> unit

val map : ('a -> 'b) -> 'a t -> 'b t

val mapi : (int -> 'a -> 'b) -> 'a t -> 'b t

val fold : (int -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b


(*i*)
open Hashcons
(*i*)
  
	  (*s Caches for functions over terms. *)

module HCache =
  Hashtbl.Make(
    struct 
      type t = Term.t
      let equal = (===) 
      let hash x = x.tag 
    end)
    
let cache n f =
  let ht = HCache.create n in
  fun x ->
    try
      HCache.find ht x
    with
	Not_found -> let y = f x in HCache.add ht x y; y

      
module HCache2 =
  Hashtbl.Make(
    struct 
      type t = Term.t * Term.t
      let equal (x1,y1) (x2,y2) = x1 === x2 && y1 === y2
      let hash (x,y) = (x.tag + y.tag) land 0x3FFFFFFF
  end)
    
let cache2 n f =
  let ht = HCache2.create n in
  fun x ->
    try
      HCache2.find ht x
    with
	Not_found -> let y = f x in HCache2.add ht x y; y

     
module HCachel =
  Hashtbl.Make(
    struct 
      type t = Term.t list
      let equal l1 l2 = try List.for_all2 (===) l1 l2 with Invalid_argument _ -> false
      let hash l = (List.fold_left (fun h a -> h + a.tag) 1 l) land 0x3FFFFFFF
    end)
    
let cachel n f =
  let ht = HCachel.create n in
  fun x ->
    try
      HCachel.find ht x
    with
	Not_found ->
	  let y = f x in HCachel.add ht x y; y 
    

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

module type ELT = Type.EQUAL

module type S = sig
  type elt

  type t = private
    | Empty
    | Singleton of elt
    | Add of elt * t
    | Union of t * t

  val equal : t -> t -> bool
  val is_empty : t -> bool
  val is_singleton : t -> bool
  val pp : Format.formatter -> t -> unit
  val mem : elt -> t -> bool
  val empty : t
  val singleton : elt -> t
  val add : elt -> t -> t
  val rem : elt -> t -> t
  val disjoint : t -> t -> bool
  val union : t -> t -> t
  val to_list : t -> elt list
  val iter : (elt -> unit) -> t -> unit
  val for_all : (elt -> bool) -> t -> bool
  val exists : (elt -> bool) -> t -> bool
  val choose : (elt -> bool) -> t -> elt
end

module Make (Elt : ELT) = struct
  type elt = Elt.t
  type t = Empty | Singleton of Elt.t | Add of Elt.t * t | Union of t * t

  let equal = ( = )
  let compare = Stdlib.compare
  let hash = Hashtbl.hash

  let rec mem x = function
    | Empty -> false
    | Singleton y -> Elt.equal x y
    | Add (y, s) -> Elt.equal x y || mem x s
    | Union (s1, s2) -> mem x s1 || mem x s2

  let empty = Empty
  let is_empty = function Empty -> true | _ -> false
  let is_singleton = function Singleton _ -> true | _ -> false

  let singleton =
    let module Cache = Weakhash.Make (Elt) in
    let table = Cache.create 7 in
    fun x ->
      try Cache.find table x
      with Not_found ->
        let s = Singleton x in
        assert (not (Cache.mem table x)) ;
        Cache.add table x s ;
        s

  let add x s =
    assert (not (mem x s)) ;
    match s with Empty -> singleton x | s -> Add (x, s)

  let rem x =
    let rec remx s =
      match s with
      | Empty -> Empty
      | Singleton y -> if Elt.equal x y then Empty else s
      | Add (y, s') ->
          if Elt.equal x y then s'
          else
            let s'' = remx s' in
            if s' == s'' then s else Add (y, s'')
      | Union (s1, s2) ->
          let s1' = remx s1 in
          if s1' == s1 then
            let s2' = remx s2 in
            if s2' == s2 then s else Union (s1, s2')
          else Union (s1', s2)
    in
    remx

  let exists p =
    let rec existsp = function
      | Empty -> false
      | Singleton x -> p x
      | Add (x, s) -> p x || existsp s
      | Union (s1, s2) -> existsp s1 || existsp s2
    in
    existsp

  let for_all p =
    let rec for_allp = function
      | Empty -> true
      | Singleton x -> p x
      | Add (x, s) -> p x && for_allp s
      | Union (s1, s2) -> for_allp s1 && for_allp s2
    in
    for_allp

  let disjoint s1 s2 =
    let notmem s x = not (mem x s) in
    for_all (notmem s2) s1 && for_all (notmem s1) s2

  let union s1 s2 =
    assert (disjoint s1 s2) ;
    match (s1, s2) with
    | Empty, _ -> s2
    | _, Empty -> s1
    | Singleton x1, _ -> add x1 s2
    | _, Singleton x2 -> add x2 s1
    | _ -> Union (s1, s2)

  let iter f =
    let rec iterf = function
      | Empty -> ()
      | Singleton x -> f x
      | Add (x, s) ->
          f x ;
          iterf s
      | Union (s1, s2) ->
          iterf s1 ;
          iterf s2
    in
    iterf

  let choose p =
    let rec choosep = function
      | Empty -> raise Not_found
      | Singleton x -> if p x then x else raise Not_found
      | Add (x, s) -> if p x then x else choosep s
      | Union (s1, s2) -> ( try choosep s1 with Not_found -> choosep s2 )
    in
    choosep

  let to_list =
    let rec accumulate acc = function
      | Empty -> acc
      | Singleton x -> x :: acc
      | Add (x, s) -> accumulate (x :: acc) s
      | Union (s1, s2) -> accumulate (accumulate acc s1) s2
    in
    accumulate []

  let pp fmt s =
    Format.fprintf fmt "@[{" ;
    let rec printl = function
      | [] -> ()
      | [x] -> Elt.pp fmt x
      | x :: xl ->
          Elt.pp fmt x ;
          Format.fprintf fmt ", " ;
          printl xl
    in
    printl (to_list s) ;
    Format.fprintf fmt "}@]@?"
end

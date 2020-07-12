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

type t = {mutable name: string; mutable hash: int}
type name = t (* nickname *)

let pp fmt n = Format.fprintf fmt "%s" n.name

let hash n =
  if n.hash >= 0 then n.hash
  else
    let h = Hashtbl.hash_param 4 4 n.name in
    n.hash <- h ;
    h

let length n = String.length n.name

module Table = Weak.Make (struct
  type t = name

  let equal n m = n.name = m.name
  let hash = hash
end)

let table = Table.create 117

let of_string =
  let dummy = {name= ""; hash= -1} in
  fun str ->
    dummy.name <- str ;
    dummy.hash <- -1 ;
    try Table.find table dummy
    with Not_found ->
      let n = {name= str; hash= dummy.hash} in
      Table.add table n ;
      n

let of_int i = of_string (string_of_int i)

let is_defined =
  let dummy = {name= ""; hash= -1} in
  fun str ->
    dummy.name <- str ;
    dummy.hash <- -1 ;
    Table.mem table dummy

let fresh =
  let str = "v" in
  fun () ->
    let rec loop i =
      let s = str ^ "!" ^ string_of_int i in
      if is_defined s then loop (i + 1) else of_string s
    in
    loop 0

let to_string n = n.name
let equal = ( == )
let compare n m = if n == m then 0 else Stdlib.compare n m

let fast_compare n m =
  if n == m then 0 else if hash n > hash m then 1 else -1

module Set = Set.Make (struct
  type t = name

  let compare = fast_compare
end)

module Map = Map.Make (struct
  type t = name

  let compare = fast_compare
end)

module Hash = Hashtbl.Make (struct
  type t = name

  let equal = equal
  let hash = hash
end)

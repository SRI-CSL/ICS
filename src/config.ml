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

module Set (S : Sets.S) = struct
  let init = ref (S.empty ()) (* initial configuration. *)

  let curr = ref (S.empty ()) (* current configuration. *)

  let initialize s =
    init := s ;
    curr := s

  let current () = !curr
  let unchanged () = !init == !curr
  let mem x = S.mem x !curr
  let sub s = S.subset s !curr
  let protect () = if !init == !curr then curr := S.copy !curr

  let add x =
    if not (mem x) then (
      protect () ;
      S.add x !curr )

  let remove x =
    if mem x then (
      protect () ;
      S.remove x !curr )

  let union s =
    if not (sub s) then (
      protect () ;
      S.union s !curr )
end

module Map (M : Maps.S) = struct
  let empty = M.empty ()
  let init = ref empty (* initial configuration. *)

  let curr = ref empty (* current configuration. *)

  let initialize s =
    init := s ;
    curr := s

  let reset () =
    assert (M.is_empty empty) ;
    initialize empty

  let current () = !curr
  let pp fmt = M.pp fmt !curr
  let unchanged () = !init == !curr
  let find x = M.find x !curr
  let mem x = M.mem x !curr
  let protect () = if !init == !curr then curr := M.copy !curr

  let set x v =
    protect () ;
    M.set x v !curr

  let remove x =
    if mem x then (
      protect () ;
      M.remove x !curr )

  let replace x y =
    protect () ;
    curr := M.replace !curr x y
end

module Subst (S : Subst.S) = struct
  type t = S.t

  let empty = S.empty ()
  let init = ref empty
  let curr = ref empty

  let initialize s =
    init := s ;
    curr := s

  let reset () = initialize empty
  let current () = !curr
  let pp fmt = S.pp fmt !curr
  let unchanged () = !init == !curr
  let lookup x = S.lookup !curr x
  let apply t = S.apply !curr t
  let mem x = S.dom x !curr
  let protect () = if !init == !curr then curr := S.copy !curr

  let add x t =
    protect () ;
    S.add !curr x t

  let compose rho =
    protect () ;
    S.compose !curr rho

  let update x t =
    protect () ;
    S.update !curr x t

  let remove x =
    if mem x then (
      protect () ;
      S.remove !curr x )

  let fold f e = S.fold f !curr e
  let iter f = S.iter f !curr
  let exists p = S.exists p !curr
  let for_all p = S.for_all p !curr
  let choose p = S.choose p !curr
end

module Powermap (M : Powermaps.S) = struct
  let empty = M.empty ()
  let init = ref empty (* initial configuration. *)

  let curr = ref empty (* current configuration. *)

  let initialize s =
    init := s ;
    curr := s

  let reset () = initialize empty
  let current () = !curr
  let unchanged () = !init == !curr
  let find x = M.find x !curr
  let mem x = M.mem x !curr
  let is_empty = mem
  let is_singleton x = M.Values.cardinal (find x) = 1
  let protect () = if !init == !curr then curr := M.copy !curr

  let remove x =
    if mem x then (
      protect () ;
      M.remove x !curr )

  let update x ys =
    if M.Values.is_empty ys then remove x
    else (
      protect () ;
      M.set x ys !curr )

  let merge x y =
    let xs = find x in
    if M.Values.is_empty xs then ()
    else
      let ys = find y in
      if M.Values.is_empty ys then (
        protect () ;
        M.remove x !curr ;
        M.set y xs !curr )
      else
        let ys' = M.Values.copy ys in
        M.Values.union xs ys' ;
        protect () ;
        M.remove x !curr ;
        M.set y ys' !curr

  let add x y =
    let dy = find y in
    if M.Values.is_empty dy then (
      protect () ;
      M.set y (M.Values.singleton x) !curr )
    else if not (M.Values.mem x dy) then (
      let dy' = M.Values.copy dy in
      M.Values.add x dy' ;
      protect () ;
      M.set y dy' !curr )

  let rem x y =
    let dy = find y in
    if M.Values.is_empty dy then ()
    else if M.Values.mem x dy then (
      if M.Values.cardinal dy = 1 then (
        protect () ;
        M.remove y !curr )
      else
        let dy' = M.Values.copy dy in
        M.Values.remove x dy' ;
        protect () ;
        M.set y dy' !curr )
end

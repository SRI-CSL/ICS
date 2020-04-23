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

module type HASH = sig
  type t

  val equal : t -> t -> bool
  val hash : t -> int
  val pp : Format.formatter -> t -> unit
end

module type S = sig
  type key
  type value
  type t

  val create : int -> t
  val count : t -> int
  val add : t -> key -> value -> unit
  val find : t -> key -> value
  val mem : t -> key -> bool
  val iter : (key -> value -> unit) -> t -> unit
  val fold : (key -> value -> 'a -> 'a) -> t -> 'a -> 'a
  val to_list : t -> (key * value) option list

  type stats = {length: int; count: int; del: int}

  val stats : t -> stats
end

let debug = ref 0

module Make
    (Key : HASH) (Value : sig
      type t

      val dummy : t
    end) : S with type key = Key.t and type value = Value.t = struct
  type key = Key.t
  type value = Value.t

  module D = Set.Make (struct
    type t = int

    let compare i j = if i == j then 0 else if i < j then -1 else 1
  end)

  type t =
    { mutable keys: key Weak.t
    ; mutable vals: value array
    ; mutable entries: int
    ; mutable deleted: D.t }

  let create m =
    let m = max 31 (min m Sys.max_array_length) in
    { keys= Weak.create m
    ; vals= Array.make m Value.dummy
    ; entries= 0
    ; deleted= D.empty }

  let length t =
    assert (Array.length t.vals = Weak.length t.keys) ;
    Array.length t.vals

  let count t = t.entries

  type stats = {length: int; count: int; del: int}

  let stats t = {length= length t; count= count t; del= D.cardinal t.deleted}
  let is_deleted t j = D.mem j t.deleted
  let protect = ref false

  module Trace = struct
    open Format

    let key_print = Key.pp err_formatter
    let val_print _ = Format.eprintf "???"

    let add k v =
      if !debug <> 0 then (
        eprintf "\nWeakhash.add: " ;
        key_print k ;
        eprintf " |-> " ;
        val_print v ;
        eprintf "@?" )
  end

  (** [with_protected f a] disables garbage collection of values while
      executing [f a]. As a consequence, [Array.get t.vals j] always
      produces a meaningful value, whenever [Weak.check t.keys j] holds. *)
  let with_protected f a =
    try
      protect := true ;
      let b = f a in
      protect := false ;
      b
    with exc ->
      protect := false ;
      raise exc

  let to_list t =
    let m = length t in
    let l = ref [] in
    let body j =
      let e =
        match Weak.get t.keys j with
        | Some k ->
            (* atomic. *)
            assert (Weak.check t.keys j) ;
            Some (k, t.vals.(j))
        | None -> None
      in
      l := e :: !l
    in
    for j = 0 to m - 1 do
      with_protected body j
    done ;
    !l

  let iter f t =
    let m = length t in
    let body j =
      match Weak.get t.keys j with
      | Some k ->
          (* atomic. *)
          assert (not (is_deleted t j)) ;
          let u = t.vals.(j) in
          f k u
      | None -> ()
    in
    for j = 0 to m - 1 do
      with_protected body j
    done

  let fold f t e =
    let acc = ref e in
    let f' k u = acc := f k u !acc in
    iter f' t ;
    !acc

  (** Linear probing. *)
  let hash m k i =
    assert (i <= m) ;
    let h' = Key.hash k mod (max_int - m) in
    assert (0 <= h' + i) ;
    (h' + i) mod m

  let mem t k =
    let m = length t in
    let h = hash m k in
    let rec repeat i =
      i < m
      &&
      let j = h i in
      assert (j < m) ;
      match Weak.get t.keys j with
      | Some k' -> if Key.equal k k' then true else repeat (i + 1)
      | None -> if is_deleted t j then repeat (i + 1) else false
    in
    repeat 0

  let find t k =
    let m = length t in
    let h = hash m k in
    let rec repeat i =
      if i = m then raise Not_found
      else
        let j = h i in
        assert (j < m) ;
        match Weak.get t.keys j with
        | Some k' ->
            (* atomic. *)
            if Key.equal k k' then (
              assert (Weak.check t.keys j) ;
              let v = t.vals.(j) in
              v )
            else repeat (i + 1)
        | None -> if is_deleted t j then repeat (i + 1) else raise Not_found
    in
    with_protected repeat 0

  exception Overflow

  let gc_finalise f x =
    try Gc.finalise f x
    with exc ->
      if !debug > 0 then
        let s =
          Key.pp Format.str_formatter x ;
          Format.flush_str_formatter ()
        in
        Format.eprintf "\nWarning(weakhash %s): %s@?" s
          (Printexc.to_string exc)

  let add_aux t k u =
    let m = length t in
    let h = hash m k in
    let rec repeat i =
      if i >= m then raise Overflow
      else
        let j = h i in
        assert (j < length t) ;
        match Weak.get t.keys j with
        | None ->
            let remove _ =
              t.deleted <- D.add j t.deleted ;
              t.entries <- t.entries - 1 ;
              if not !protect then
                (* do not free value while *)
                t.vals.(j) <- t.vals.(0)
              (* executing protected code. *)
            in
            Weak.set t.keys j (Some k) ;
            (* shadow values for [k] should be removed. *)
            t.vals.(j) <- u ;
            if is_deleted t j then t.deleted <- D.remove j t.deleted ;
            t.entries <- t.entries + 1 ;
            gc_finalise remove k
        | Some _ -> repeat (i + 1)
    in
    repeat 0

  let resize t =
    let m = length t in
    let keys = t.keys and vals = t.vals in
    let m' = min ((3 * m / 2) + 3) (Sys.max_array_length - 1) in
    if !debug > 0 then Format.eprintf "\nWeakhash.resize: %d@." m' ;
    if m' <= m then failwith "Weakhash.resize : out of memory."
    else (
      t.keys <- Weak.create m' ;
      t.vals <- Array.make m' t.vals.(0) ;
      t.deleted <- D.empty ;
      let body j =
        match Weak.get keys j with
        | Some k ->
            (* atomic. *)
            assert (Weak.check keys j) ;
            let u = vals.(j) in
            add_aux t k u
        | None -> ()
      in
      for j = 0 to m - 1 do
        with_protected body j
      done )

  let output t k _u =
    Format.eprintf "Error: " ;
    Key.pp Format.err_formatter k ;
    Format.eprintf " already in weak hash table@?" ;
    Format.eprintf " \n Elements(%d) " (count t) ;
    iter
      (fun k _ ->
        Key.pp Format.err_formatter k ;
        Format.eprintf ", " )
      t ;
    Format.eprintf "\n@?"

  let add t k u =
    assert (
      Trace.add k u ;
      true ) ;
    assert (
      if not (mem t k) then true
      else (
        output t k u ;
        false ) ) ;
    (* avoid creating shadows. *)
    if float_of_int t.entries /. float_of_int (length t) >= 0.7 then
      resize t ;
    (* resize when load factor [>= 0.7]. *)
    try add_aux t k u
    with Overflow ->
      resize t ;
      add_aux t k u
end

(**/**)

module Test = struct
  let maxruns = ref 10000
  let initsize = ref 5
  let maxkey = ref 1000

  module Int = struct
    type t = int

    let value = function i -> i
    let hash = function i -> i
    let equal i j = i == j
    let pp fmt = function i -> Format.fprintf fmt "<int %d>" i
    let random () = Random.int !maxkey
    let dummy = 0
  end

  module Table = Make (Int) (Int)

  let table = Table.create !initsize

  module Op = struct
    let add () =
      let x = Int.random () in
      if not (Table.mem table x) then
        let v = Int.value x in
        try
          Format.eprintf "\nAdd <-- %d %d@?" (Int.value x) v ;
          Table.add table x v ;
          Format.eprintf "\nAdd --> ()@?"
        with exc ->
          Format.eprintf "\nAdd[exit] %s@?" (Printexc.to_string exc)

    let find () =
      let x = Int.random () in
      Format.eprintf "\nFind <-- %d@?" (Int.value x) ;
      try
        let v = Table.find table x in
        Format.eprintf "\nFind --> %d@?" v ;
        ()
      with
      | Not_found -> Format.eprintf "\nFind --> ???@?"
      | exc -> Format.eprintf "\nFind[exit] %s@?" (Printexc.to_string exc)

    let mem () =
      let x = Int.random () in
      Format.eprintf "\nMem <-- %d@?" (Int.value x) ;
      try
        Format.eprintf "\nMem --> %s@?"
          (if Table.mem table x then "true" else "false")
      with exc ->
        Format.eprintf "\nMem[exit] %s@?" (Printexc.to_string exc)

    let random () =
      match Random.int 4 with
      | 0 -> add ()
      | 1 -> find ()
      | 2 -> mem ()
      | _ -> Gc.minor ()
  end

  let statistics () =
    Format.eprintf "\n\nStatistics: @?" ;
    let s = Table.stats table in
    Format.eprintf "\n length = %d;\n count = %d; \n del = %d@?"
      s.Table.length s.Table.count s.Table.del ;
    debug := 0

  let run () =
    debug := 3 ;
    for i = 0 to !maxruns do
      Op.random () ;
      if i mod 100 = 0 then statistics ()
    done ;
    statistics ()
end

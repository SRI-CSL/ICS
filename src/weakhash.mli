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

(** {i Weak hash tables}

  Functional hash tables for associating keys [k] with values [v] by means of a 
  binding [k |-> v]. This association is {i weak} in that [k] is still garbage 
  collectable even though it is a key of a hash table binding. Upon garbage collection, 
  the association [k |-> v] is deleted.


  @author Harald Ruess
*)

(** The input signature of the functor {!Weakhash.Make}. *)
module type HASH = sig
  type t
    (** The type of the hashtable keys. Should be heap-allocated. *)

  val equal : t -> t -> bool
    (** The equality predicate used to compare keys. *)

  val hash : t -> int
    (** A hashing function on keys. It must be such that if two keys are
      equal according to [equal], then they have identical hash values
      as computed by [hash]. Examples: suitable ([equal], [hash]) pairs for 
      arbitrary key types include
      ([(=)], [Hashtbl.hash]) for comparing objects by structure, and
      ([(==)], [Hashtbl.hash]) for comparing objects by addresses
      (e.g. for mutable or cyclic keys). *)

  val pp : Format.formatter -> t -> unit
    (** Pretty-printing of keys on given formatter. *)
end


(** The output signature of the functor {!Weakhash.Make}. *)
module type S =
  sig
    type key
      (** Representation of keys. *)
    type 'a t
      (** Representation of a hash table as a finite function with 
	bindings [k |-> v] with [k] a [key] and value [v] of type ['a]. *)

    val create : int -> 'a t
      (** [create n] creates a hash table with no bindings. The
	parameter [n] indicates how much memory is allocated initially. *)

    val count : 'a t -> int
      (** [count t] returns the number of bindings in hash table [t]. *)

    val add : 'a t -> key -> 'a -> unit
      (** [add t k v] adds a bindings [k |-> v] to [t]. It is assumed that
	there is no binding [k |-> w] in [t]!  A [Failure] exception
      is raised if memory allocation fails, that is, the maximum array
      length has been exceeded. *)

    val find : 'a t -> key -> 'a
      (** [find t k] returns [v] if [t] contains a binding [k |-> v];
	otherwise [Not_found] is raised. *)

    val mem : 'a t -> key -> bool
      (** [mem t k] holds iff there is a binding [k |-> v] in [t]. *)

    val iter : (key -> 'a -> unit) -> 'a t -> unit
      (** [iter t f] applies [f k v] for all bindings [k |-> v] in [t].
	The order of application is unspecified. *)

    val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
      (** [fold f t e] returns [f k{1} v{1} (f k{2} v{2} ... (f k{n} v{n} e)...)]
	if [k{i} |-> v{i}] for [i = 1,...,n] are all the bindings in [t].
	The order of accumulation is unspecified. *)

    val to_list : 'a t -> (key * 'a) option list
      (** Represent the set of bindings [k |-> v] in [t] as a list with 
	elements  [(k, v)]. *)

    type stats = { length : int; count : int; del: int }
	(** Some statistics for a hash table:
	  - [length] indicates current memory usage
	  - [count] is the number of bindings 
	  - [del] is the number of deleted entries. *)

    val stats : 'a t -> stats
      (** Return statistics for a hash table. *)
  end

module Make (H : HASH) : (S with type key = H.t)
  (** Functor building an implementation of the weak hashtable
    signature. The functor [Weakhash.Make] returns a structure 
    containing a type [key] of keys and a type ['a t] of hash tables
    associating data of type ['a] to keys of type [key].
    This association is {i weak} in the sense that [key]
    can be garbage collected even though it is a [key]
    in the hash table.  On garbage collection of [key],
    the association of [key] with its value is deleted from
    the hash table. *)
  

(**/**)

(** For debugging only. *)
module Test : sig
  val maxruns : int ref
  val initsize : int ref
  val maxkey : int ref
  val run : unit -> unit
end

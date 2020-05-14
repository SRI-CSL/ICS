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


module Z = struct
  open Big_int

  type t = big_int

  let of_int = big_int_of_int
  let zero = zero_big_int
  let one = unit_big_int
  let two = add_big_int one one
  let add = add_big_int
  let sub = sub_big_int
  let succ = add one
  let mult = mult_big_int
  let divexact =  div_big_int 
  let gcd = gcd_big_int
  let lcm a b = divexact (mult a b) (gcd a b)
  let pow = power_int_positive_int

  let expt = power_big_int_positive_int 
   
		 
  let compare = compare_big_int
  let equal =   eq_big_int
  let lt = lt_big_int
  let le = le_big_int 
  let gt = gt_big_int
  let ge = ge_big_int
		  
  let of_string = big_int_of_string
  let to_string = string_of_big_int

  let to_int = int_of_big_int   (** may raise [Failure "int_of_big_int"]. *)
 
  let pp fmt x = 
    Format.fprintf fmt "%s" (to_string x)
end


module Q = struct
  open Num
  
  type t = num

  let of_int = num_of_int
  let of_ints x y = div_num (num_of_int x) (num_of_int y)   

  let zero = of_int 0
  let one = of_int 1
  let two = of_int 2
  let negone = of_int (-1)

  let add = add_num
  let sub = sub_num
  let minus = minus_num
  let mult = mult_num
  let div = div_num
  let inv = div one

  let maxRandom = ref 20

  let random = 
    let initialized = ref false in
      fun () -> 
	if not(!initialized) then 
	  (Random.self_init(); initialized := true);
	let d = Random.int !maxRandom 
	and n = Random.int !maxRandom in
	  if n = 0 then of_int d else
	    of_ints d n
 
  let compare = compare_num

  let equal = eq_num
  let is_zero = equal zero
  let is_one = equal one
  let is_negone = equal negone

  let lt = lt_num
  let le = le_num
  let gt = gt_num
  let ge = ge_num

  let is_pos x = gt x zero
  let is_neg x = lt x zero
  let is_nonneg x = ge x zero
  let is_nonpos x = le x zero

  let min = min_num
  let max = max_num


  let rec expt a n =
    if equal a zero then zero
    else if n < 0 then
      inv (expt a (-n))
    else if n = 0 then 
      one
    else if n = 1 then
      a
    else 
      mult a (expt a (n - 1))

  let of_z = num_of_big_int
  let to_z = big_int_of_num

  let floor x = to_z (floor_num x)
  let ceil x = to_z (ceiling_num x)

  let frac x = sub x (floor_num x)
  let def x = sub (ceiling_num x) x

  let abs = abs_num

  type cmp = Equal | Greater | Less

  let cmp x y =
    let b = compare x y in
    if b == 0 then Equal else if b > 0 then Greater else Less

  let ratio x = 
    Ratio.cautious_normalize_ratio (ratio_of_num x)

  let denominator x = Ratio.denominator_ratio (ratio x)
  let numerator x = Ratio.numerator_ratio (ratio x)

  let is_integer = is_integer_num
 
  let hash = Hashtbl.hash
  
  let to_string = string_of_num
  let of_string = num_of_string

  let pp fmt x = Format.fprintf fmt "%s" (to_string x)

  type q = t

  module Hash = Hashtbl.Make(
    struct
      type t = q
      let equal = equal
      let hash = hash
    end)

end

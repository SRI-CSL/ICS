
(*s Multi-precision arithmetic. *)

(*i module GMP = struct i*)

open Gmp

module Z = struct

  type t = Z.t

  let of_int = Z.from_int
  let mult = Z.mul
  let divexact = Z.divexact
  let gcd = Z.gcd
  let lcm a b = Z.divexact (Z.mul a b) (Z.gcd a b)
  let pow = Z.pow_ui_ui 
		 
  let compare = Z.cmp
  let equal x y = Z.cmp x y == 0
  let lt x y = Z.cmp x y < 0
  let le x y = Z.cmp x y <= 0
  let gt x y = Z.cmp x y > 0
  let ge x y = Z.cmp x y >= 0
		  
  let to_string z = Z.string_from z 10
 
  let pp fmt x = Format.fprintf fmt "%s" (Z.string_from x 10)
end

module Q = struct
  
  type t = Q.t

  let zero = Q.from_ints 0 1
  let one = Q.from_ints 1 1
  let of_int n = Q.from_ints n 1
  let of_ints = Q.from_ints

  let add = Q.add
  let sub = Q.sub
  let minus = Q.neg
  let mult = Q.mul
  let div = Q.div
  let inv = Q.inv

  let floor x = Gmp.Z.fdiv_q (Q.get_num x) (Q.get_den x)
  let ceil x  = Gmp.Z.cdiv_q (Q.get_num x) (Q.get_den x)

  let compare = Q.cmp
  let equal = Q.equal
  let is_zero x = Q.equal zero x
  let is_one x = Q.equal one x
  let is_negone x = Q.equal (Q.neg one) x
  let lt x y = Q.cmp x y < 0
  let le x y = Q.cmp x y <= 0
  let gt x y = Q.cmp x y > 0
  let ge x y = Q.cmp x y >= 0

  type cmp = Equal | Greater | Less

  let cmp x y =
    let b = compare x y in
    if b == 0 then Equal else if b > 0 then Greater else Less

  let denominator = Q.get_den

  let is_integer q = (Gmp.Z.cmp_si (Q.get_den q) 1) = 0
  let to_z = Q.get_num
  let of_z = Q.from_z

  let hash = Hashtbl.hash

  let to_string q = 
    let d = Q.get_den q in
    if Gmp.Z.cmp_si d 1 == 0 then
      Gmp.Z.string_from (Q.get_num q) 10
    else
      (Gmp.Z.string_from (Q.get_num q) 10) ^ "/" ^ (Gmp.Z.string_from d 10)

  let of_string s =
    try
      let k = String.index s '/' in
      let l = String.length s in
      let n = Gmp.Z.from_string (String.sub s 0 k) 10 in
      let d = Gmp.Z.from_string (String.sub s (succ k) (l - k - 1)) 10 in
      Q.from_zs n d
    with Not_found ->
      Q.from_z (Gmp.Z.from_string s 10)

  let pp fmt x = Format.fprintf fmt "%s" (to_string x)

end

(*i end i*)

(*i*** Ocaml bignums: DEPRECATED ****

module NUM = struct

open Num

(*s Integers. *)

module Z = struct

  open Big_int

  type t = big_int

  let equal = eq_big_int
  let compare = compare_big_int
  let lt = lt_big_int
  let le = le_big_int
  let gt = gt_big_int
  let ge = ge_big_int

  let of_int = big_int_of_int
  let mult = mult_big_int
  let divexact = div_big_int
  let gcd = gcd_big_int
  let lcm a b = divexact (mult a b) (gcd a b)

  let to_string = string_of_big_int
  let pp z = Format.print_string (string_of_big_int z)

end

(*s Rationals. *)

module Q = struct

  type t = num

  let zero = Int 0
  let one = Int 1
  let of_int n = Int n

  let add = add_num
  let sub = sub_num
  let minus = minus_num
  let mult = mult_num
  let div = div_num

  let compare = compare_num	      
  let equal = eq_num
  let lt = lt_num
  let le = le_num
  let gt = gt_num
  let ge = ge_num
	
  (*s Hashing of nums. Here, the implementation of big nums adds a 
    difficulty. Indeed, a same number may have different representation,
    like for instance 1 and $1\over 1$. But the hash function must have
    the property that equal values have identical hash keys. Since equality
    is here the usual equality of numbers ([eq_num]) it means that both
    1 and $1\over 1$ must have the same hash key. 
    We achieve that by (1) normalizing all the ratios after the computations,
    so that we will never get $2\over 2$ instead of $1\over 1$; (2)
    normalizing all the ratios which are actually integers, so that we will
    never get $1\over 1$ instead of 1. 

    Then we can compute the hash key in the following way. For an integer [n]
    it is [n] modulo [m], where [m] is a great prime number (536870923). For
    a ratio $p\over q$, it is [p+q] modulo [m]. *)

  open Big_int
  open Ratio
  let _ = Arith_status.set_normalize_ratio true
  let big_mod = big_int_of_int 536870923
  let modulo b = int_of_big_int (mod_big_int (abs_big_int b) big_mod)
  let hash = function
    | Int n -> (abs n) mod 536870923
    | Big_int b -> modulo b
    | Ratio _ as r when is_integer_num r -> modulo (big_int_of_num r)
    | Ratio r -> modulo (add_big_int (denominator_ratio r) (numerator_ratio r))

  let floor q = big_int_of_num (floor_num q)
  let ceil q = big_int_of_num (ceiling_num q)
  let is_integer = is_integer_num
  let of_z = num_of_big_int
  let to_z = big_int_of_num

  let denominator q = denominator_ratio (ratio_of_num q)
		   
  let to_string = string_of_num
  let of_string = num_of_string

  let pp q = Format.print_string (to_string q)

end

end

module Z = struct
  module G = GMP.Z
  module N = NUM.Z
  type t = G.t * N.t

  let bin_compose check gf nf (g,n) (g',n') = 
    let gr = gf g g' 
    and nr = nf n n' in
    check gr nr;
    (gr,nr)

  let un_compose check gf nf x = 
    let gr = gf x
    and nr = nf x in
    check gr nr;
    (gr,nr)

  let check_same_z f g n = 
    let gs = G.to_string g
    and ns = N.to_string n in
    if gs <> ns then begin
      Printf.printf "DISAGREEMENT in %s: g=%s n=%s\n" f gs ns;
      exit 1
    end

  let bin_compose_2 f gf nf (g,n) (g',n') = 
    let gr = gf g g' 
    and nr = nf n n' in
    if gr <> nr then begin
      Printf.printf "DISAGREEMENT in %s\n" f;
      exit 1
    end;
    gr

  let mult = bin_compose (check_same_z "mult") G.mult N.mult
  let divexact = bin_compose (check_same_z "divexact") G.divexact N.divexact

  let equal = bin_compose_2 "Zequal" G.equal N.equal
  let compare = bin_compose_2 "compare" G.compare N.compare
  let lt = bin_compose_2 "lt" G.lt N.lt
  let le = bin_compose_2 "le" G.le N.le
  let gt = bin_compose_2 "gt" G.gt N.gt
  let ge = bin_compose_2 "ge" G.ge N.ge

  let of_int = un_compose (check_same_z "of_int") G.of_int N.of_int
  let gcd = bin_compose (check_same_z "gcd") G.gcd N.gcd
  let lcm = bin_compose (check_same_z "lcm") G.lcm N.lcm

  let pp (_,n) = N.pp n
     
end

module Q = struct

  module G = GMP.Q
  module N = NUM.Q
  type t = G.t * N.t

  let bin_compose check gf nf (g,n) (g',n') = 
    let gr = gf g g' 
    and nr = nf n n' in
    check gr nr;
    (gr,nr)

  let un_compose check gf nf (g,n) = 
    let gr = gf g
    and nr = nf n in
    check gr nr;
    (gr,nr)

  let un_compose_1 check gf nf x = 
    let gr = gf x
    and nr = nf x in
    check gr nr;
    (gr,nr)

  let un_compose_2 f gf nf x = 
    let gr = gf x
    and nr = nf x in
    if gr <> nr then begin
      Printf.printf "DISAGREEMENT in %s\n" f;
      exit 1
    end;
    gr

  let un_compose_3 f gf nf (g,n) = 
    let gr = gf g
    and nr = nf n in
    if gr <> nr then begin
      Printf.printf "DISAGREEMENT in %s\n" f;
      exit 1
    end;
    gr

  let check_same_q f g n = 
    let gs = G.to_string g
    and ns = N.to_string n in
    if gs <> ns then begin
      Printf.printf "DISAGREEMENT in %s: g=%s n=%s\n" f gs ns;
      exit 1
    end

  let bin_compose_2 f gf nf (g,n) (g',n') = 
    let gr = gf g g' 
    and nr = nf n n' in
    if gr <> nr then begin
      Printf.printf "DISAGREEMENT in %s with g=%s n=%s g'=%s n'=%s\n" f 
	(G.to_string g) (N.to_string n) (G.to_string g') (N.to_string n');
      exit 1
    end;
    gr

  let zero = (G.zero,N.zero)
  let one = (G.one,N.one)
  let of_int = un_compose_1 (check_same_q "of_int") G.of_int N.of_int

  let add = bin_compose (check_same_q "add") G.add N.add
  let sub = bin_compose (check_same_q "sub") G.sub N.sub
  let minus = un_compose (check_same_q "minus") G.minus N.minus
  let mult = bin_compose (check_same_q "mult") G.mult N.mult
  let div = bin_compose (check_same_q "div") G.div N.div

  let floor = un_compose (Z.check_same_z "floor") G.floor N.floor
  let ceil = un_compose (Z.check_same_z "ceil") G.ceil N.ceil

  let compare = bin_compose_2 "compare" G.compare N.compare
  let equal (g,n) (g',n') = 
    let gr = G.equal g g' in
    let nr = N.equal n n' in
    if gr <> nr then begin
      Printf.printf "DISAGREEMENT in Q.equal:\n";
      Printf.printf "  G.equal %s %s = %s\n" 
	(G.to_string g) (G.to_string g') (string_of_bool gr);
      Printf.printf "  N.equal %s %s = %s\n"
	(N.to_string n) (N.to_string n') (string_of_bool nr)
    end;
    gr
  
  let lt = bin_compose_2 "lt" G.lt N.lt
  let le = bin_compose_2 "le" G.le N.le
  let gt = bin_compose_2 "gt" G.gt N.gt
  let ge = bin_compose_2 "ge" G.ge N.ge

  let denominator = 
    un_compose (Z.check_same_z "denominator") G.denominator N.denominator
      
  let is_integer = un_compose_3 "is_integer" G.is_integer N.is_integer 
  let to_z = un_compose (Z.check_same_z "to_z") G.to_z N.to_z
  let of_z = un_compose (check_same_q "of_z") G.of_z N.of_z
      
  let hash (_,n) = N.hash n

  let to_string (_,n) = N.to_string n
  let of_string = un_compose_1 (check_same_q "of_string") G.of_string N.of_string

  let pp (_,n) = N.pp n

end

*** Ocaml bignums: DEPRECATED ***i*)

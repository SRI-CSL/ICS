
(*i*)
open Term
open Hashcons
open Mpa
(*i*)


type signum = F | Nonpos | Neg | Zero | Pos | Nonneg | T

  
(*s Sign interpretation of arithmetic terms. *)


let inf (s1,s2) =
  if s1 = s2 then
    s1
  else
    match s1,s2 with
      | F, _ -> F
      | _, F -> F
      | T, _ -> s2
      | _, T -> s1
      | Zero, (Nonpos | Nonneg) -> Zero
      | (Nonpos | Nonneg), Zero -> Zero
      | Pos, Nonneg -> Pos
      | Nonneg, Pos -> Pos
      | Neg, Nonpos -> Neg
      | Nonpos, Neg -> Neg
      | _ -> F
  
let ( ** ) s1 s2 =
  match s1,s2 with
    | F, _    -> F
    | _, F    -> F
    | T, _    -> T
    | _, T    -> T
    | Zero, _ -> Zero
    | _, Zero -> Zero
    | Pos, _ -> s2
    | Nonpos, (Nonneg | Pos) -> Nonpos
    | Nonpos, _ -> Nonneg
    | Nonneg, (Nonneg | Pos) -> Nonneg
    | Nonneg, _ -> Nonpos
    | Neg, Neg -> Pos
    | Neg, Pos -> Neg
    | Neg, Nonneg -> Nonpos
    | Neg, Nonpos -> Nonneg

let ( ++ ) s1 s2 =
  match s1, s2 with
    | F, _ -> F
    | _, F -> F
    | Zero, _ -> s2
    | _, Zero -> s1
    | Pos, (Pos | Nonneg) -> Pos
    | Nonneg, Pos -> Pos
    | Neg, (Neg | Nonpos) -> Neg
    | Nonpos, Neg -> Neg
    | Nonpos, Nonpos -> Nonpos
    | Nonneg, Nonneg -> Nonneg
    | _ -> T
  
let sign s t =
  let rec sign_of t =
    match t.node with
      | Arith a ->
	  (match a with
	     | Num q -> sign_of_num q
	     | Times l -> sign_of_times l
	     | Plus l -> sign_of_plus l)
      | _ ->
	  sign_from_state t

  and sign_from_state t =
    let f t' acc =
      match t'.node with
	| Cnstrnt(c,x) when t == State.find s x ->
	    (match c with
	       | Term.Pos -> Pos
	       | Term.Neg -> Neg
	       | Term.Nonpos -> Nonpos
	       | Term.Nonneg -> Nonneg
	       | _ -> acc)
	| _ -> acc
    in
    Tset.fold f (State.use s t) T
    
  and sign_of_num q =
    if Q.is_zero q then Zero
    else if Q.gt q Q.zero then Pos
    else Neg

  and sign_of_times l =
    match l with
      | [] ->
	  Pos
      | [x] ->
	  sign_of x
      | x :: y :: l when x == y ->         (*i Squares *)
	  let s = match sign_of x with
	    | Zero -> Zero
	    | (Pos | Neg) -> Pos
	    | _ -> Nonneg
	  in
	  s ** sign_of_times l
      | x :: l ->
	  sign_of x ** sign_of_times l

  and sign_of_plus l =
    match l with
      | [] -> Zero
      | [x] -> sign_of x
      | x :: l -> sign_of x ++ sign_of_plus l
  in
  sign_of t

let is_nonneg s t =
  let sgn = sign s t in sgn = Zero || sgn = Pos || sgn = Nonneg

let is_pos s t =
  sign s t = Pos

let is_nonpos s t =
  let sgn = sign s t in sgn = Zero || sgn = Neg || sgn = Nonpos

let is_neg s t =
  sign s t = Neg

let inconsistent s (t1,t2) =
  inf (sign s t1, sign s t2) = F

let infer s (t1,t2) =
  let s1 = sign s t1 in
  let s2 = sign s t2 in
  if s1 = s2 then
    []
  else
    match s1, s2 with
      | T, _
      | _, T -> []
      | Zero, (Nonpos | Nonneg) ->
	  [t2, Arith.zero]
      | (Nonpos | Nonneg), Zero ->
	  [t1, Arith.zero]
      | Pos, Nonneg ->
	  [Atom.pos t2, Bool.tt]
      | Nonneg, Pos ->
	  [Atom.pos t1, Bool.tt]
      | Nonneg, Nonpos ->
	  [t1, Arith.zero; t2, Arith.zero]
      | Neg, Nonpos ->
	  [Atom.neg t2, Bool.tt]
      | Nonpos, Neg ->
	  [Atom.neg t1, Bool.tt]
      | Nonpos, Nonneg ->
	  [t1, Arith.zero; t2, Arith.zero]
      | _ ->
	  begin
	    Format.printf "\nInconsistent: "; Pretty.eqn (t1,t2);
	    raise (Exc.Inconsistent "sign")
	  end







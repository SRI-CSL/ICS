
(*i*)
open Term
open Hashcons
open Mpa
(*i*)

(*s Sign interpretation of arithmetic terms. *)

type sign = F | Nonpos | Neg | Zero | Pos | Nonneg | T

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
  
let rec sign_of t =
  match t.node with
    | Var (_,_,Some(c)) ->
	sign_of_var c
    | Arith a ->
	(match a with
	   | Num q -> sign_of_num q
	   | Times l -> sign_of_times l
	   | Plus l -> sign_of_plus l)
    | _ -> T

and sign_of_var c =
  match c with
    | Term.Neg -> Neg
    | Term.Pos -> Pos
    | Term.Nonneg -> Nonneg
    | Term.Nonpos -> Nonpos

and sign_of_num q =
  if Q.is_zero q then Zero
  else if Q.gt q Q.zero then Pos
  else Neg

and sign_of_times l =
  match l with
    | [] -> Pos
    | [x] -> sign_of x
    | x :: y :: l when x == y ->         (*i Squares *)
	(match sign_of x with
	   | Zero -> Zero
	   | (Pos | Neg) -> Pos
	   | _ -> Nonneg)
	**
	sign_of_times l
    | x :: l -> sign_of x ** sign_of_times l

and sign_of_plus l =
  match l with
    | [] -> Zero
    | [x] -> sign_of x
    | x :: l -> sign_of x ++ sign_of_plus l

let sign = cache 1007 sign_of

let is_nonneg t =
  let s = sign t in s = Zero || s = Pos || s = Nonneg

let is_pos t =
  sign t = Pos

let is_nonpos t =
  let s = sign t in s = Zero || s = Neg || s = Nonpos

let is_neg t =
  sign t = Neg





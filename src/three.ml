
type t = 
  | Yes
  | No
  | X

let is_sub a b =
  match a, b with
    | _, X -> true
    | (X | No), Yes -> false
    | Yes, Yes -> true
    | (X | Yes), No -> false
    | No, No -> true

let inter =
  let yes = Some(Yes) in
  let no = Some(No) in
  let x = Some(X) in
  fun a b -> match a, b with
    | No, X -> no
    | Yes, X -> yes
    | X, No -> no
    | X, Yes -> yes
    | No, No -> no
    | X, X -> x
    | Yes,Yes -> yes
    | No, Yes -> None
    | Yes, No -> None

let union a b =
  match a, b with
    | _, X -> X
    | X, _ -> X
    | No, No -> No
    | Yes,Yes -> Yes
    | No, Yes -> X
    | Yes, No -> X

let is_disjoint a b =
  inter a b = None



type 'a seq = 
  | Nil
  | Cons of 'a * ('a seq) ref
  | Delay of (unit -> 'a seq)

let force xp = match !xp with
  | Delay xs -> (xp := xs(); !xp)
  | xs -> xs

let nil = Nil

let cons x xs = Cons(x, ref (Delay xs))

let singleton x = cons x (fun () -> nil)

let null = function
  | Nil -> true
  | Cons _ -> false
  | Delay _ -> assert false

let hd = function
  | Cons(x,_) -> x
  | _ -> assert false

let tl = function
  | Cons(_,xs) -> force xs
  | _ -> assert false

let rec map f = function
  | Nil -> Nil
  | Cons(x, xp) -> 
      cons (f x) (fun () -> map f (force xp))
  | _ -> assert false

let rec (@) xs ys = match xs with
  | Nil -> ys
  | Cons(x, xs') -> cons x (fun () -> (force xs' @ ys))
  | Delay _ -> assert false

let rec filter p = function
  | Nil -> Nil
  | Cons(x, xs) when p x -> cons x (fun () -> filter p (force xs))
  | Cons(_, xs) -> filter p (force xs)
  | Delay _ -> assert false

let rec concat = function
  | Nil -> Nil
  | Cons(Nil, ss) -> concat (force ss)
  | Cons(Cons(x, xs), ss) -> cons x (fun () -> concat (Cons(force xs, ss)))
  | _ -> assert false

let mapcan f xs = concat (map f xs)

let rec take n xs = match n, xs with
  | 0, _ -> [] 
  | _, Nil -> []
  | _, Cons(x, xs') -> x :: take (n - 1) (force xs')
  | _, Delay _ -> assert false

let cycle seqfn =
  let knot = ref Nil in
    knot := seqfn (fun () -> !knot );
    !knot

let from_list l =
  List.fold_right (fun x ys -> cons x (fun () -> ys)) l nil


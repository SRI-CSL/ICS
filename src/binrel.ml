
type t = Same | Disjoint | Sub | Super | Overlap

    (*s If [r1] is the best relation in [t] between [s1] and [t1] and
        [r2] is the best relation between [s2] and [t2], then
        [union r1 r2] is the best relation between [union s1 t1] and [union s2 t2].
      *)
      

let union r1 r2 =
  if r1 = r2 then
    r1
  else
  match r1, r2 with
    | Overlap, _ -> Overlap
    | _, Overlap -> Overlap
    | Same, Sub -> Sub
    | Same, Super -> Super
    | Same, _ -> Overlap
    | Disjoint, _ -> Overlap
    | Sub, Same -> Sub
    | Sub, Disjoint -> Overlap
    | Sub, Super -> Overlap
    | Super, Same -> Super
    | Super, Disjoint -> Overlap
    | Super, Sub -> Overlap
    | _ -> assert false

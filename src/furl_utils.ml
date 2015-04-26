
let map_snd f (x,y) = (x, f y)

let rec intersperse sep =
  function
    | [] -> []
    | [x] -> [x]
    | h :: t -> h :: sep :: intersperse sep t

(** Offset of el in l, given the function count.
    Used to get the first regexp group at a given place.
*)
(* Physical equality is on purpose, to avoid dealing with duplicates.
   TODO: get rid of it.
*)
let find_idx count el l =
  let rec aux el i = function
    | [] -> raise Not_found
    | x::l' ->
      if x == el then i
      else aux el (i + count el) l'
  in aux el 0 l

(** if [l' ∈ l] then [build_permutation offset count l l'] builds
    a mapping: index in l => offset in l'.
    Offsets are computed respecting [offset] and [count].
*)
(* Invariants:
   - l_before is included in l_after.
   - No duplicates [see note on {!find_idx}.
*)
let build_permutation offset count l_before l_after =
  let t = Array.make (List.length l_before) 0 in
  l_before |> List.iteri (fun i x ->
    let j = find_idx count x l_after in
    t.(i) <- offset + j
  ) ;
  t

let mul_string s f =
  let i = Int.of_float f in
  let rec aux acc left =
    match left with
    | 0 -> acc
    | _ -> aux (acc ^ s) (left - 1)
  in if i < 0 then "" else aux s i

let mul_string s f =
  let i = Int.of_float f in
  let rec aux acc left =
    match left with
    | 0 -> acc
    | _ -> aux (acc ^ s) (left - 1)
  in if i < 0 then "" else aux s i

let string_of_chars c = String.of_seq (List.to_seq (List.rev c))

let chars_of_string s = List.of_seq (String.to_seq s)

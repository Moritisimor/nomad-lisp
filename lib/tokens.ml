type token =
  | LPAREN
  | RPAREN
  | NUMLIT of float
  | BOOLLIT of bool
  | STRINGLIT of string
  | UNITLIT
  | SYMBOL of string
  | EOF

let rec string_of_token = function
  | LPAREN -> "LPAREN"
  | RPAREN -> "RPAREN"
  | NUMLIT x -> Printf.sprintf "NUMLIT(%.2f)" x
  | BOOLLIT x -> Printf.sprintf "BOOLLIT(%b)" x
  | STRINGLIT x -> Printf.sprintf "STRINGLIT(\"%s\")" x
  | UNITLIT -> "UNITLITERAL"
  | SYMBOL x -> Printf.sprintf "SYMBOL('%s')" x
  | EOF -> Printf.sprintf "EOF"

let scan_numlit char_stream =
  let rec aux acc left =
    match left with
    | [] -> Error "Number literal was never ended"
    | ')' :: xs 
      | '(' :: xs 
      | ' ' :: xs 
      | '\t' :: xs 
      | '\n' :: xs -> (
        let num_string = String.of_seq (List.to_seq (List.rev acc)) in
        match float_of_string_opt num_string with
        | Some x -> Ok ((NUMLIT x), left)
        | None -> Error (Printf.sprintf "Could not parse %s to a number" num_string)
      )

    | x :: xs -> aux (x :: acc) xs
  in aux [] char_stream

let scan_stringlit char_stream =
  let rec aux acc left =
    match left with
    | [] -> Error "String literal was never ended"
    | '"' :: xs -> Ok (STRINGLIT (String.of_seq (List.to_seq (List.rev acc))), xs)
    | '\\' :: 'n' :: xs -> aux ('\n' :: acc) xs
    | '\\' :: 't' :: xs -> aux ('\t' :: acc) xs
    | '\\' :: 'r' :: xs -> aux ('\r' :: acc) xs
    | '\\' :: 'b' :: xs -> aux ('\b' :: acc) xs
    | '\\' :: '"' :: xs -> aux ('"' :: acc) xs
    | x :: xs -> aux (x :: acc) xs
  in aux [] char_stream

let skip_to_newline char_stream =
  let rec aux left =
    match left with
    | [] -> []
    | '\n' :: xs -> xs
    | _ :: xs -> aux xs
  in aux char_stream

let scan_symbol char_stream =
  let rec aux acc left =
    match left with
    | [] -> Error "Symbol was never ended"
    | ')' :: xs 
      | '(' :: xs 
      | ' ' :: xs 
      | '\t' :: xs 
      | '\n' :: xs -> (
        let symbol_string = String.of_seq (List.to_seq (List.rev acc)) in
        Ok ((SYMBOL symbol_string), left)
      )
    
    | x :: xs -> aux (x :: acc) xs
  in aux [] char_stream

let count_parens token_stream =
  let rec aux accl accr left =
    match left with
    | [] -> (accl, accr)
    | x :: xs -> (
      match x with
      | RPAREN -> aux accl (accr + 1) xs
      | LPAREN -> aux (accl + 1) accr xs
      | _ -> aux accl accr xs
    )
  in aux 0 0 token_stream

let tokenize text =
  let chars = List.of_seq (String.to_seq text) in

  let rec aux acc left =
    match left with
    | [] -> Ok (List.rev (EOF :: RPAREN :: acc))
    | ' ' :: xs | '\t' :: xs | '\n' :: xs -> aux acc xs

    | 't' :: 'r' :: 'u' :: 'e' :: xs -> aux (BOOLLIT true :: acc) xs
    | 'f' :: 'a' :: 'l' :: 's' :: 'e' :: xs -> aux (BOOLLIT false :: acc) xs

    | 'u' :: 'n' :: 'i' :: 't' :: xs -> aux (UNITLIT :: acc) xs

    | '#' :: xs -> aux acc (skip_to_newline xs)

    | ')' :: xs -> aux (RPAREN :: acc) xs
    | '(' :: xs -> aux (LPAREN :: acc) xs
    | '0' :: xs
      | '1' :: xs
      | '2' :: xs
      | '3' :: xs
      | '4' :: xs
      | '5' :: xs
      | '6' :: xs
      | '7' :: xs
      | '8' :: xs
      | '9' :: xs -> (
        match scan_numlit left with
        | Ok (parsed_numlit, rest) -> aux (parsed_numlit :: acc) rest
        | Error e -> Error e
      )

    | '"' :: xs -> (
      match scan_stringlit xs with
      | Ok (parsed_stringlit, rest) -> aux (parsed_stringlit :: acc) rest
      | Error e -> Error e
    )

    | x :: xs -> (
      match scan_symbol left with
      | Ok (parsed_symbol, rest) -> aux (parsed_symbol :: acc) rest
      | Error e -> Error e
    )
  in 
  match aux [LPAREN] chars with
  | Ok tokens -> (
    let (l, r) = count_parens tokens in
    if l = r 
      then Ok tokens
      else if l > r then Error "Unbalanced parantheses: one or more unclosed left parantheses"
      else Error "Unbalanced parantheses: one or more superfluous right parantheses"
  )

  | Error e -> Error e

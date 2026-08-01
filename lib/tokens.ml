open Nomad_err
open Helpers
open Printf

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
  | NUMLIT x -> sprintf "NUMLIT(%.2f)" x
  | BOOLLIT x -> sprintf "BOOLLIT(%b)" x
  | STRINGLIT x -> sprintf "STRINGLIT(\"%s\")" x
  | UNITLIT -> "UNITLITERAL"
  | SYMBOL x -> sprintf "SYMBOL('%s')" x
  | EOF -> sprintf "EOF"

let scan_numlit char_stream =
  let rec aux acc left =
    match left with
    | [] -> Error (TokenizerError (sprintf "Number literal was never ended (Got %s)" (string_of_chars acc)))
    | ')' :: xs 
      | '(' :: xs 
      | ' ' :: xs 
      | '\t' :: xs 
      | '\n' :: xs -> (
        let num_string = string_of_chars acc in
        match float_of_string_opt num_string with
        | Some x -> Ok ((NUMLIT x), left)
        | None -> Error (TokenizerError (Printf.sprintf "Could not parse %s to a number" num_string))
      )

    | x :: xs -> aux (x :: acc) xs
  in aux [] char_stream

let scan_stringlit char_stream =
  let rec aux acc left =
    match left with
    | [] -> Error (TokenizerError (sprintf "String literal was never ended (Got \"%s)" (string_of_chars acc)))
    | '"' :: xs -> Ok (STRINGLIT (string_of_chars acc), xs)
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
    | [] -> Error (TokenizerError (sprintf "Symbol was never ended (Got %s)" (string_of_chars acc)))
    | ')' :: xs 
      | '(' :: xs 
      | ' ' :: xs 
      | '\t' :: xs 
      | '\n' :: xs -> (
        let symbol_string = string_of_chars acc in
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
  let chars = chars_of_string text in

  let rec aux acc left =
    match left with
    | [] -> Ok (List.rev (EOF :: RPAREN :: acc))
    (* Whitespace *)
    | ' ' :: xs | '\t' :: xs | '\n' :: xs -> aux acc xs

    (* Bool literals *)
    | 't' :: 'r' :: 'u' :: 'e' :: xs -> aux (BOOLLIT true :: acc) xs
    | 'f' :: 'a' :: 'l' :: 's' :: 'e' :: xs -> aux (BOOLLIT false :: acc) xs

    (* Unit literals *)
    | 'u' :: 'n' :: 'i' :: 't' :: xs -> aux (UNITLIT :: acc) xs

    (* Comments *)
    | '#' :: xs -> aux acc (skip_to_newline xs)

    | ')' :: xs -> aux (RPAREN :: acc) xs
    | '(' :: xs -> aux (LPAREN :: acc) xs

    | '-' :: ('0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9') :: xs (* Negative Numbers *)
    | ('0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9') :: xs -> (
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
      else if l > r then Error (TokenizerError "Unbalanced parantheses: one or more unclosed left parantheses")
      else Error (TokenizerError "Unbalanced parantheses: one or more superfluous right parantheses")
  )

  | Error e -> Error e

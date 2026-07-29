open Tokens
open Expr

let rec parse tokens =
  let rec aux acc left =
    match left with
    | NUMLIT n :: rest -> Ok (NumLit n, rest)
    | SYMBOL s :: rest -> Ok (Symbol s, rest)
    | STRINGLIT s :: rest -> Ok (StringLit s, rest)
    | BOOLLIT s :: rest -> Ok (BoolLit s, rest)
    | UNITLIT :: rest -> Ok (Unit, rest)

    | LPAREN :: rest -> (
      match parse_list rest with
      | Ok (x, xs) -> Ok (x, xs)
      | Error e -> Error e
    )

    | x :: xs -> Error ("Unexpected token: " ^ string_of_token x)
    | [] -> Error "Cannot parse EOF"
  in aux [] tokens

and parse_list tokens =
  let rec aux acc left =
    match left with
    | RPAREN :: rest -> Ok (List (List.rev acc), rest)
    | [] -> Error "Unexpected EOF"

    | _ -> (
      match parse left with
      | Ok (expr, rest) -> aux (expr :: acc) rest
      | Error e -> Error e
    )
  in aux [] tokens

let expr_list_of_listlit l =
  match l with
  | List l -> Ok l
  | _ -> Error "Root Expression is not a list"

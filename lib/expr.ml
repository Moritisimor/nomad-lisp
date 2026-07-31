open Printf

type expr = 
  | Lambda of string list * expr
  | Symbol of string
  | NumLit of float
  | StringLit of string
  | BoolLit of bool
  | List of expr list
  | Unit

let rec string_of_expr = function
  | Lambda _ -> "<LAMBDA>"
  | Symbol s -> sprintf "Symbol('%s')" s
  | NumLit i -> sprintf "Number(%f)" i
  | StringLit s -> sprintf "String(\"%s\")" s
  | BoolLit b -> sprintf "Bool(%b)" b
  | Unit -> "<UNIT>"
  | List l -> (
    let rec aux acc left = 
      match left with
      | [] -> acc
      | [x] -> acc ^ (string_of_expr x)
      | x :: xs -> aux (acc ^ string_of_expr x ^ " ") xs
    in aux "List(" l
  )

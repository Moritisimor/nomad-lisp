type token =
  | LPAREN
  | RPAREN
  | NUMLIT of float
  | BOOLLIT of bool
  | STRINGLIT of string
  | UNITLIT
  | LAMBDA
  | SYMBOL of string
  | EOF

type expr = 
  | Fun of string * expr list
  | Symbol of string
  | NumLit of float
  | StringLit of string
  | BoolLit of bool
  | List of expr list
  | Unit

type runtime_value =
  | RFun of string list * expr list
  | RNum of float
  | RString of string
  | RBool of bool
  | RList of runtime_value list
  | RUnit 
  | RErr of string

let string_of_rval = function
  | RFun _ -> "<FUNCTION>"
  | RNum x -> Printf.sprintf "%.2f" x
  | RString x -> x
  | RBool x -> Printf.sprintf "%b" x
  | RList x -> "<LIST>"
  | RUnit -> "<UNIT>"
  | RErr x -> Printf.sprintf "ERROR (%s)" x

let rec string_of_token = function
  | LPAREN -> "LPAREN"
  | RPAREN -> "RPAREN"
  | NUMLIT x -> Printf.sprintf "NUMLIT(%.2f)" x
  | BOOLLIT x -> Printf.sprintf "BOOLLIT(%b)" x
  | STRINGLIT x -> Printf.sprintf "STRINGLIT(\"%s\")" x
  | UNITLIT -> "UNITLITERAL"
  | LAMBDA -> "LAMBDA"
  | SYMBOL x -> Printf.sprintf "SYMBOL('%s')" x
  | EOF -> Printf.sprintf "EOF"

let print_token t = print_string (string_of_token t) 

let mul_string s f =
  let i = Int.of_float f in
  let rec aux acc left =
    match left with
    | 0 -> acc
    | _ -> aux (acc ^ s) (left - 1)
  in if i < 0 then "" else aux s i

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

let tokenize text =
  let chars = List.of_seq (String.to_seq text) in

  let rec aux acc left =
    match left with
    | [] -> Ok (List.rev (EOF :: RPAREN :: acc))
    | ' ' :: xs | '\t' :: xs | '\n' :: xs -> aux acc xs

    | 't' :: 'r' :: 'u' :: 'e' :: xs -> aux (BOOLLIT true :: acc) xs
    | 'f' :: 'a' :: 'l' :: 's' :: 'e' :: xs -> aux (BOOLLIT false :: acc) xs

    | '(' :: ')' :: xs -> aux (UNITLIT :: acc) xs
    | 'l' :: 'a' :: 'm' :: 'b' :: 'd' :: 'a' :: xs -> aux (LAMBDA :: acc) xs

    | ';' :: xs -> aux acc (skip_to_newline xs)

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
  in aux [LPAREN] chars

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

let rec eval expression env =
  match expression with
  | NumLit x -> RNum x
  | StringLit x -> RString x
  | BoolLit x -> RBool x
  | List [Symbol "+"; lhs; rhs] -> (
    let (x, y) = (eval lhs env, eval rhs env) in
    match (x, y) with
    | (RNum a, RNum b) -> RNum (a +. b)
    | (RString a, RString b) -> RString (a ^ b)
    | _ -> 
      RErr (Printf.sprintf "Cannot add these expressions: %s and %s" (string_of_rval x) (string_of_rval y))
  )

  | List [Symbol "exit"] -> exit 0
  | List [Symbol "exit"; c] -> (
    let code = eval c env in
    match code with
    | RNum a -> exit (int_of_float a)
    | _ -> RErr (Printf.sprintf "Exit Code is not a number. Expected a number, got %s" (string_of_rval code))
  )

  | List [Symbol "-"; lhs; rhs] -> (
    let (x, y) = (eval lhs env, eval rhs env) in
    match (x, y) with
    | (RNum a, RNum b) -> RNum (a -. b)
    | _ -> 
      RErr (Printf.sprintf "Cannot subtract these expressions: %s and %s" (string_of_rval x) (string_of_rval y))
  )

  | List [Symbol "*"; lhs; rhs] -> (
    let (x, y) = (eval lhs env, eval rhs env) in
    match (x, y) with
    | (RNum a, RNum b) -> RNum (a *. b)
    | (RString a, RNum b) -> RString (mul_string a b)
    | _ -> 
      RErr (Printf.sprintf "Cannot multiply these expressions: %s and %s" (string_of_rval x) (string_of_rval y))
  )

  | List [Symbol "/"; lhs; rhs] -> (
    let (x, y) = (eval lhs env, eval rhs env) in
    match (x, y) with
    | (RNum a, RNum b) -> if b = 0.0 then RErr "Division by zero!" else RNum (a /. b)
    
    | _ ->
      RErr (Printf.sprintf "Cannot divide these expressions: %s and %s" (string_of_rval x) (string_of_rval y))
  )

  | List [Symbol "print"; printee] -> (
    print_string (string_of_rval (eval printee env));
    RUnit
  )

  | List [Symbol "println"; printee] -> (
    print_endline (string_of_rval (eval printee env));
    RUnit
  )

  | _ -> RErr "I do not know what to make of this expression"

let do_string source_code = 
  let tokens = (
    match tokenize source_code with
    | Ok t -> t
    | Error e -> (
      print_endline ("Error while tokenizing: " ^ e);
      exit 1
    )
  ) in

  let root_expression = (
    match parse tokens with
    | Ok (a, _) -> a
    | Error e -> (
      print_endline ("Error while parsing: " ^ e);
      exit 1
    )
  ) in

  let ast = (
    match expr_list_of_listlit root_expression with
    | Ok a -> a
    | Error e -> (
      print_endline e;
      exit 1
    )
  ) in 

  let rec aux env last_expr left =
    match left with
    | [] -> last_expr
    | x :: xs -> aux env (eval x env) xs
  in aux () RUnit ast
  
let () =
  let rec aux () =
    print_string "Nomad LISP REPL >> ";
    Out_channel.flush stdout;
    let input = read_line () in
    Printf.printf "Evaluates to: %s\n" (string_of_rval (do_string input));
    aux ()
  in aux ()



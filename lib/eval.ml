open Runtime_value
open Expr
open Helpers
open Tokens
open Parser

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
  match tokenize source_code with
  | Error e -> Error e
  | Ok tokens -> (
    match parse tokens with
    | Error e -> Error e
    | Ok (root_expr, _) -> (
      match expr_list_of_listlit root_expr with
      | Error e -> Error e
      | Ok ast -> (
        let rec aux env last_expr left =
          match left with
          | [] -> last_expr
          | x :: xs -> aux env (eval x env) xs
        in Ok (aux () RUnit ast)
      )
    )
  )

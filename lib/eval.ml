open Runtime_value
open Expr
open Helpers
open Tokens
open Parser
open Env

let rec eval expression env =
  match expression with
  | NumLit x -> RNum x
  | StringLit x -> RString x
  | BoolLit x -> RBool x
  | Symbol s -> Env.get_binding env s
  | List [Symbol "let"; Symbol binding_name; binding_value] -> (
    let evaluated_binding_value = eval binding_value env in
    Env.set_binding env binding_name evaluated_binding_value;
    RUnit
  ) 

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

  | List [Symbol "if"; cond; true_body; false_body] -> (
    let evaluated_cond = eval cond env in
    match evaluated_cond with
    | RBool b -> (
      match b with
      | true -> eval true_body env
      | false -> eval false_body env
    )

    | _ -> 
      RErr (Printf.sprintf "Condition of if-construct does not evaluate to a bool: %s" (string_of_rval evaluated_cond))
  )

  | List [Symbol "not"; boolexpr] -> (
    let evaluated_boolexpr = eval boolexpr env in
    match evaluated_boolexpr with
    | RBool b -> (
      match b with
      | true -> RBool false
      | false -> RBool true
    )

    | _ -> 
      RErr (Printf.sprintf "Cannot negate non-boolean expression: %s" (string_of_rval evaluated_boolexpr))
  )

  | List [Symbol "="; a; b] -> (
    let (x, y) = (eval a env, eval b env) in
    match (x, y) with
    | (RNum i, RNum j) -> RBool (i = j)
    | (RString i, RString j) -> RBool (i = j)
    | (RBool i, RBool j) -> RBool (i = j)
    | _ ->
      RErr (Printf.sprintf "Cannot compare these expressions: %s and %s" (string_of_rval x) (string_of_rval y))
  )

  | List [Symbol "=="; a; b] -> if a = b then RBool true else RBool false
  | List [Symbol ">"; a; b] -> (
    let (x, y) = (eval a env, eval b env) in
    match (x, y) with
    | (RNum i, RNum j) -> RBool (i > j)
    | _ -> 
      RErr (Printf.sprintf "Cannot perform greater-comparison on non-numerical types: %s and %s" 
      (string_of_rval x) (string_of_rval y))
  )

  | List [Symbol ">="; a; b] -> (
    let (x, y) = (eval a env, eval b env) in
    match (x, y) with
    | (RNum i, RNum j) -> RBool (i > j || i = j)
    | _ -> 
      RErr (Printf.sprintf "Cannot perform greater/equal-comparison on non-numerical types: %s and %s" 
      (string_of_rval x) (string_of_rval y))
  )

  | List [Symbol "<"; a; b] -> (
    let (x, y) = (eval a env, eval b env) in
    match (x, y) with
    | (RNum i, RNum j) -> RBool (i < j)
    | _ -> 
      RErr (Printf.sprintf "Cannot perform smaller-comparison on non-numerical types: %s and %s" 
      (string_of_rval x) (string_of_rval y))
  )

  | List [Symbol "<="; a; b] -> (
    let (x, y) = (eval a env, eval b env) in
    match (x, y) with
    | (RNum i, RNum j) -> RBool (i < j || i = j)
    | _ -> 
      RErr (Printf.sprintf "Cannot perform smaller/equal-comparison on non-numerical types: %s and %s" 
      (string_of_rval x) (string_of_rval y))
  )

  | List [Symbol "print"; printee] -> (
    print_string (string_of_rval (eval printee env));
    RUnit
  )

  | List [Symbol "println"; printee] -> (
    print_endline (string_of_rval (eval printee env));
    RUnit
  )

  | List [Symbol "readln"] -> RString (read_line ())
  | List [Symbol "readln"; prompt] -> (
    let evaluated_prompt = eval prompt env in
    match evaluated_prompt with
    | RString s -> (
      print_string s;
      Out_channel.flush stdout;
      RString (read_line ())
    )

    | _ -> RErr "The prompt you supplied is a non-string expression"
  )

  | List l -> RList (List.map (fun x -> eval x env) l)
  | _ -> RErr "I do not know what to make of this expression"

let do_string source_code env = 
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
        in Ok (aux env RUnit ast)
      )
    )
  )

let do_file file_path =
  let file_channel = In_channel.open_text file_path in
  let source_code = In_channel.input_all file_channel in 
  In_channel.close file_channel;
  let env = new_env () in 

  match tokenize source_code with
  | Error e -> Error e
  | Ok tokens -> (
    match parse tokens with
    | Error e -> Error e
    | Ok (root_expr, _) -> (
      match expr_list_of_listlit root_expr with
      | Error e -> Error e
      | Ok ast -> (
        let rec aux env left =
          match left with
          | [] -> Ok ()
          | x :: xs -> (
            match eval x env with
            | RErr e -> Error ("Uncaught Error: " ^ e)
            | _ -> aux env xs
          )
        in aux env ast
      )
    )
  )

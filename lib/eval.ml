open Runtime_value
open Expr
open Helpers
open Tokens
open Parser
open Env
open Nomad_err

let rec eval expression env =
  match expression with
  | NumLit x -> RNum x
  | StringLit x -> RString x
  | BoolLit x -> RBool x
  | Symbol s -> Env.get_binding env s
  | Lambda (params, body) -> RLambda (params, body, Hashtbl.copy env.bindings)
  | Unit -> RUnit
  
  | List [Symbol "letfun"; Symbol name; List params; body] -> (
    let rec aux acc left = 
      match left with
      | [] -> Ok (List.rev acc)
      | x :: xs -> (
        match x with
        | Symbol s -> aux (s :: acc) xs
        | _ -> Error "Non-Symbol in parameter list"
      )
    in let param_list = aux [] params in
    match param_list with
    | Error e -> RErr e
    | Ok p -> (
      Env.set_binding env name (RLambda (p, body, env.bindings));
      RUnit
    )
  )

  | List [Symbol "let"; Symbol binding_name; binding_value] -> (
    let evaluated_binding_value = eval binding_value env in
    Env.set_binding env binding_name evaluated_binding_value;
    RUnit
  )

  | List [Symbol "lambda"; List params; body] | List [Symbol "λ"; List params; body] -> (
    let rec aux acc left = 
      match left with
      | [] -> Ok (List.rev acc)
      | x :: xs -> (
        match x with
        | Symbol s -> aux (s :: acc) xs
        | _ -> Error "Non-Symbol in parameter list"
      )
    in let param_list = aux [] params in 
    match param_list with
    | Error e -> RErr e
    | Ok p -> RLambda (p, body, Hashtbl.copy env.bindings)
  )

  | List [Symbol "head"; list_expr] | List [Symbol "car"; list_expr] -> (
    let l = eval list_expr env in
    match l with
    | RList content -> (
      match content with
      | head :: _ -> head
      | _ -> RUnit
    )

    | _ -> RErr (Printf.sprintf "Cannot perform head-operation on non-list expression: %s" (string_of_rval l))
  )

  | List [Symbol "tail"; list_expr] | List [Symbol "cdr"; list_expr] -> (
    let l = eval list_expr env in
    match l with
    | RList content -> (
      match content with
      | _ :: tail -> RList tail
      | _ -> RUnit
    )

    | _ -> RErr (Printf.sprintf "Cannot perform tail-operation on non-list expression: %s" (string_of_rval l))
  )

  | List [Symbol "chars"; str_expr] -> (
    let str_val = eval str_expr env in
    match str_val with
    | RString s -> (
      let chars = List.of_seq (String.to_seq s) in
      RList (List.map (fun c -> RString (String.of_char c)) chars)
    )

    | _ -> RErr (Printf.sprintf "")
  )

  | List (Symbol "do" :: body) -> (
    let rec aux last_expr left = 
      match left with
      | [] -> last_expr
      | x :: xs -> aux (eval x env) xs
    in aux RUnit body
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

  | List [Symbol "unless"; cond; true_body; false_body] -> (
    let evaluated_cond = eval cond env in
    match evaluated_cond with
    | RBool b -> (
      match b with
      | false -> eval true_body env
      | true -> eval false_body env
    )

    | _ ->
      RErr (Printf.sprintf "Condition of unless-construct does not evaluate to a bool: %s" (string_of_rval evaluated_cond))
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

  | List [Symbol "and"; boolexpr1; boolexpr2] -> (
    let (evaluated1, evaluated2) = (eval boolexpr1 env, eval boolexpr2 env) in
    match (evaluated1, evaluated2) with
    | (RBool b, RBool a) -> RBool (a && b)
    | _ -> RErr (
      Printf.sprintf "Cannot perform logical-and on these expressions: %s and %s" 
      (string_of_rval evaluated1) (string_of_rval evaluated2) 
    )
  )

  | List [Symbol "or"; boolexpr1; boolexpr2] -> (
    let (evaluated1, evaluated2) = (eval boolexpr1 env, eval boolexpr2 env) in
    match (evaluated1, evaluated2) with
    | (RBool b, RBool a) -> RBool (a || b)
    | _ -> RErr (
      Printf.sprintf "Cannot perform logical-or on these expressions: %s and %s" 
      (string_of_rval evaluated1) (string_of_rval evaluated2) 
    )
  )

  | List [Symbol "isunit"; a] -> (
    match eval a env with
    | RUnit -> RBool true
    | _ -> RBool false
  )

  | List [Symbol "iserr"; a] -> (
    match eval a env with
    | RErr _ -> RBool true
    | _ -> RBool false
  )

  | List [Symbol "isnum"; a] -> (
    match eval a env with
    | RNum _ -> RBool true
    | _ -> RBool false
  )

  | List [Symbol "islist"; a] -> (
    match eval a env with
    | RList _ -> RBool true
    | _ -> RBool false
  )

  | List [Symbol "isfun"; a] -> (
    match eval a env with
    | RLambda _ -> RBool true
    | _ -> RBool false
  )

  | List [Symbol "isstr"; a] -> (
    match eval a env with
    | RString _ -> RBool true
    | _ -> RBool false
  )

  | List [Symbol "isbool"; a] -> (
    match eval a env with
    | RBool _ -> RBool true
    | _ -> RBool false
  )

  | List [Symbol "cons"; l; e] -> (
    let (x, y) = (eval l env, eval e env) in
    match x with
    | RList i -> RList (List.cons y i)
    | _ -> RErr (Printf.sprintf "Cannot perform cons-operation on non-list expression: %s" (string_of_rval x))
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

  | List [Symbol "append"; a; b] -> (
    let (x, y) = (eval a env, eval b env) in
    match (x, y) with
    | (RList i, RList j) -> RList (List.append i j)
    | _ -> 
      RErr (Printf.sprintf "Cannot perform list-append on these expressions: %s and %s" (string_of_rval x) (string_of_rval y))
  )

  | List [Symbol "nth"; a; b] -> (
    let (x, idx) = (eval a env, eval b env) in
    match (x, idx) with
    | (RList i, RNum j) -> (
      match List.nth_opt i (int_of_float j) with
      | Some elem -> elem
      | None -> RUnit
    )

    | _ -> RErr (
      Printf.sprintf "Cannot perform nth-operation on these expressions: %s and %s"
      (string_of_rval x) (string_of_rval idx)
    )
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

  | List [Symbol "to_string"; e] -> RString (string_of_rval (eval e env))
  | List [Symbol "string_to_num"; e] -> (
    let x = eval e env in
    match x with
    | RString s -> (
      match float_of_string_opt s with
      | Some i -> RNum i
      | _ -> RErr (Printf.sprintf "Cannot parse this string to a number: %s" s)
    )

    | _ -> RErr (Printf.sprintf "Cannot convert this expression to a number: %s" (string_of_rval x))
  )

  | List [Symbol "quote"; List elems] -> RList (List.map (fun x -> eval x env) elems)
  
  | List (fun_expr :: fun_params) -> (
    let fun_binding = eval fun_expr env in
    match fun_binding with
    | RLambda (params, body, captured) -> (
      let (expected_len, actual_len) = (List.length params, List.length fun_params) in
      match List.length params <> List.length fun_params with
      | true -> RErr (
        Printf.sprintf "Attempted to invoke lambda with wrong amount of params. Expected: %d got: %d"
        expected_len actual_len
      )

      | false -> (
        let this_env = { bindings = captured } in
        let rec aux left idx =
          match left with
          | [] -> ()
          | x :: xs -> (
            (* This is honestly kinda stupid :/ *)
            Env.set_binding this_env x (eval (List.nth fun_params idx) this_env);
            aux xs (idx + 1)
          )

        in aux params 0;
        eval body this_env 
      )
    )

    | _ -> RErr (Printf.sprintf "Attempt to invoke non-lambda: %s" (string_of_expr fun_expr))
  )

  | List l -> RList (List.map (fun x -> eval x env) l)

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
            | RErr e -> Error (EvaluationError ("Uncaught Error: " ^ e))
            | _ -> aux env xs
          )
        in aux env ast
      )
    )
  )

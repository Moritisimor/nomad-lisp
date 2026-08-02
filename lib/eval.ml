open Runtime_value
open Expr
open Helpers
open Tokens
open Parser
open Nomad_err
open Printf

let ( let* ) = Result.bind

let rec eval expression env =
  match expression with
  | NumLit x -> Ok (RNum x)
  | StringLit x -> Ok (RString x)
  | BoolLit x -> Ok (RBool x)
  | Symbol s -> get_binding s env
  | Lambda (params, body) -> Ok (RLambda (params, body, env))
  | Unit -> Ok RUnit

  | List [Symbol "try"; may_fail; on_fail] -> (
    let evaluated = eval may_fail env in
    match evaluated with
    | Ok e -> Ok e
    | Error e -> eval on_fail env
  )
  
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
    | Error e -> Error (EvaluationError e)
    | Ok p -> (
      set_binding name (RLambda (p, body, env)) env;
      Ok RUnit
    )
  )

  | List [Symbol "let"; Symbol binding_name; binding_value] -> (
    let* evaluated_binding_value = eval binding_value env in
    set_binding binding_name evaluated_binding_value env;
    Ok RUnit
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
    | Error e -> Error (EvaluationError e)
    | Ok p -> Ok (RLambda (p, body, env))
  )

  | List [Symbol "head"; list_expr] | List [Symbol "car"; list_expr] -> (
    let* l = eval list_expr env in
    match l with
    | RList content -> (
      match content with
      | head :: _ -> Ok head
      | _ -> Ok RUnit
    )

    | _ -> 
      Error (EvaluationError (sprintf "Cannot perform head-operation on non-list expression: %s" (string_of_rval l)))
  )

  | List [Symbol "tail"; list_expr] | List [Symbol "cdr"; list_expr] -> (
    let* l = eval list_expr env in
    match l with
    | RList content -> (
      match content with
      | _ :: tail -> Ok (RList tail)
      | _ -> Ok RUnit
    )

    | _ -> Error (EvaluationError (sprintf "Cannot perform tail-operation on non-list expression: %s" (string_of_rval l)))
  )

  | List [Symbol "chars"; str_expr] -> (
    let* str_val = eval str_expr env in
    match str_val with
    | RString s -> (
      let chars = Helpers.chars_of_string s in
      Ok (RList (List.map (fun c -> RString (String.make 1 c)) chars))
    )

    | _ -> 
      Error (EvaluationError (sprintf "Cannot apply chars-operation on non-string expression: %s" (string_of_rval str_val)))
  )

  | List [Symbol "lower"; str_expr] -> (
    let* str_val = eval str_expr env in
    match str_val with
    | RString s -> Ok (RString (String.lowercase_ascii s))
    | _ -> 
      Error (EvaluationError (sprintf "Cannot apply lower-operation on non-string expression: %s" (string_of_rval str_val)))
  )

  | List [Symbol "trim"; str_expr] -> (
    let* str_val = eval str_expr env in
    match str_val with
    | RString s -> Ok (RString (String.trim s))
    | _ -> 
      Error (EvaluationError (sprintf "Cannot apply trim-operation on non-string expression: %s" (string_of_rval str_val)))
  )

  | List (Symbol "do" :: body) -> (
    let rec aux last_expr left = 
      match left with
      | [] -> Ok last_expr
      | x :: xs -> (
        match eval x env with
        | Ok e -> aux e xs 
        | Error e -> Error e
      )
    in aux RUnit body
  )

  | List [Symbol "+"; lhs; rhs] -> (
    let* x = eval lhs env in
    let* y = eval rhs env in

    match (x, y) with
    | (RNum a, RNum b) -> Ok (RNum (a +. b))
    | (RString a, RString b) -> Ok (RString (a ^ b))
    | _ -> 
      Error (EvaluationError (sprintf "Cannot add these expressions: %s and %s" (string_of_rval x) (string_of_rval y)))
  )

  | List [Symbol "exit"] -> exit 0
  | List [Symbol "exit"; c] -> (
    let* code = eval c env in
    match code with
    | RNum a -> exit (int_of_float a)
    | _ -> 
      Error (EvaluationError (sprintf "Exit Code is not a number. Expected a number, got %s" (string_of_rval code)))
  )

  | List [Symbol "-"; lhs; rhs] -> (
    let* x = eval lhs env in
    let* y = eval rhs env in

    match (x, y) with
    | (RNum a, RNum b) -> Ok (RNum (a -. b))
    | _ -> 
      Error (EvaluationError (sprintf "Cannot subtract these expressions: %s and %s" (string_of_rval x) (string_of_rval y)))
  )

  | List [Symbol "*"; lhs; rhs] -> (
    let* x = eval lhs env in
    let* y = eval rhs env in

    match (x, y) with
    | (RNum a, RNum b) -> Ok (RNum (a *. b))
    | (RString a, RNum b) -> Ok (RString (mul_string a b))
    | _ -> 
      Error (EvaluationError (sprintf "Cannot multiply these expressions: %s and %s" (string_of_rval x) (string_of_rval y)))
  )

  | List [Symbol "/"; lhs; rhs] -> (
    let* x = eval lhs env in
    let* y = eval rhs env in

    match (x, y) with
    | (RNum a, RNum b) -> if b = 0.0 
      then Error (EvaluationError "Division by zero!")
      else Ok (RNum (a /. b))
    
    | _ ->
      Error (EvaluationError (sprintf "Cannot divide these expressions: %s and %s" (string_of_rval x) (string_of_rval y)))
  )

  | List [Symbol "if"; cond; true_body; false_body] -> (
    let* evaluated_cond = eval cond env in
    match evaluated_cond with
    | RBool b -> (
      match b with
      | true -> eval true_body env
      | false -> eval false_body env
    )

    | _ -> 
      Error (EvaluationError (
        sprintf "Condition of if-construct does not evaluate to a bool: %s" (string_of_rval evaluated_cond)
      ))
  )

  | List [Symbol "unless"; cond; true_body; false_body] -> (
    let* evaluated_cond = eval cond env in
    match evaluated_cond with
    | RBool b -> (
      match b with
      | false -> eval true_body env
      | true -> eval false_body env
    )

    | _ ->
      Error (EvaluationError (
        sprintf "Condition of unless-construct does not evaluate to a bool: %s" (string_of_rval evaluated_cond)
      ))
  )

  | List [Symbol "not"; boolexpr] -> (
    let* evaluated_boolexpr = eval boolexpr env in
    match evaluated_boolexpr with
    | RBool b -> (
      match b with
      | true -> Ok (RBool false)
      | false -> Ok (RBool true)
    )

    | _ -> 
      Error (EvaluationError (sprintf "Cannot negate non-boolean expression: %s" (string_of_rval evaluated_boolexpr)))
  )

  | List [Symbol "and"; boolexpr1; boolexpr2] -> (
    let* evaluated1 = eval boolexpr1 env in 
    let* evaluated2 = eval boolexpr2 env in
    
    match (evaluated1, evaluated2) with
    | (RBool b, RBool a) -> Ok (RBool (a && b))
    | _ -> Error (EvaluationError (
      sprintf "Cannot perform logical-and on these expressions: %s and %s" 
      (string_of_rval evaluated1) (string_of_rval evaluated2) 
    ))
  )

  | List [Symbol "or"; boolexpr1; boolexpr2] -> (
    let* evaluated1 = eval boolexpr1 env in
    let* evaluated2 = eval boolexpr2 env in
    match (evaluated1, evaluated2) with
    | (RBool b, RBool a) -> Ok (RBool (a || b))
    | _ -> Error (EvaluationError (
      sprintf "Cannot perform logical-or on these expressions: %s and %s" 
      (string_of_rval evaluated1) (string_of_rval evaluated2) 
    ))
  )

  | List [Symbol "isunit"; a] -> (
    match eval a env with
    | Ok RUnit -> Ok (RBool true)
    | _ -> Ok (RBool false)
  )

  | List [Symbol "isnum"; a] -> (
    match eval a env with
    | Ok RNum _ -> Ok (RBool true)
    | _ -> Ok (RBool false)
  )

  | List [Symbol "islist"; a] -> (
    match eval a env with
    | Ok RList _ -> Ok (RBool true)
    | _ -> Ok (RBool false)
  )

  | List [Symbol "isfun"; a] -> (
    match eval a env with
    | Ok RLambda _ -> Ok (RBool true)
    | _ -> Ok (RBool false)
  )

  | List [Symbol "isstr"; a] -> (
    match eval a env with
    | Ok RString _ -> Ok (RBool true)
    | _ -> Ok (RBool false)
  )

  | List [Symbol "isbool"; a] -> (
    match eval a env with
    | Ok RBool _ -> Ok (RBool true)
    | _ -> Ok (RBool false)
  )

  | List [Symbol "cons"; e; l] -> (
    let* x = eval l env in 
    let* y = eval e env in

    match x with
    | RList i -> Ok (RList (List.cons y i))
    | _ -> 
      Error (EvaluationError (sprintf "Cannot perform cons-operation on non-list expression: %s" (string_of_rval x)))
  )

  | List [Symbol "="; a; b] -> (
    let* x = eval a env in 
    let* y = eval b env in

    match (x, y) with
    | (RNum i, RNum j) -> Ok (RBool (i = j))
    | (RString i, RString j) -> Ok (RBool (i = j))
    | (RBool i, RBool j) -> Ok (RBool (i = j))
    | _ ->
      Error (EvaluationError (sprintf "Cannot compare these expressions: %s and %s" (string_of_rval x) (string_of_rval y)))
  )

  | List [Symbol "=="; a; b] -> if a = b 
    then Ok (RBool true) 
    else Ok (RBool false)

  | List [Symbol ">"; a; b] -> (
    let* x = eval a env in 
    let* y = eval b env in

    match (x, y) with
    | (RNum i, RNum j) -> Ok (RBool (i > j))
    | _ -> 
      Error (EvaluationError (
        sprintf "Cannot perform greater-comparison on non-numerical types: %s and %s" 
        (string_of_rval x) (string_of_rval y)
      ))
  )

  | List [Symbol ">="; a; b] -> (
    let* x = eval a env in 
    let* y = eval b env in

    match (x, y) with
    | (RNum i, RNum j) -> Ok (RBool (i > j || i = j))
    | _ -> 
      Error (EvaluationError (sprintf "Cannot perform greater/equal-comparison on non-numerical types: %s and %s" 
      (string_of_rval x) (string_of_rval y)))
  )

  | List [Symbol "<"; a; b] -> (
    let* x = eval a env in 
    let* y = eval b env in

    match (x, y) with
    | (RNum i, RNum j) -> Ok (RBool (i < j))
    | _ -> 
      Error (EvaluationError (
        sprintf "Cannot perform smaller-comparison on non-numerical types: %s and %s" 
        (string_of_rval x) (string_of_rval y)
      ))
  )

  | List [Symbol "<="; a; b] -> (
    let* x = eval a env in 
    let* y = eval b env in

    match (x, y) with
    | (RNum i, RNum j) -> Ok (RBool (i < j || i = j))
    | _ -> 
      Error (EvaluationError (
        sprintf "Cannot perform smaller/equal-comparison on non-numerical types: %s and %s" 
        (string_of_rval x) (string_of_rval y)
      ))
  )

  | List [Symbol "append"; a; b] -> (
    let* x = eval a env in 
    let* y = eval b env in

    match (x, y) with
    | (RList i, RList j) -> Ok (RList (List.append i j))
    | _ -> Error (EvaluationError 
      (sprintf "Cannot perform list-append on these expressions: %s and %s" (string_of_rval x) (string_of_rval y)
    ))
  )

  | List [Symbol "nth"; a; b] -> (
    let* x = eval a env in 
    let* idx = eval b env in

    match (x, idx) with
    | (RList i, RNum j) -> (
      match List.nth_opt i (int_of_float j) with
      | Some elem -> Ok elem
      | None -> Ok RUnit
    )

    | _ -> Error (EvaluationError (
      sprintf "Cannot perform nth-operation on these expressions: %s and %s"
      (string_of_rval x) (string_of_rval idx)
    ))
  )

  | List (Symbol "print" :: params) -> (
    let rec aux acc left =
      match left with
      | [] -> Ok (List.rev acc)
      | x :: xs -> (
        let* evaluated = eval x env in
        aux (evaluated :: acc) xs
      )
    in let* exprs = aux [] params in
    exprs |> List.iter (fun e -> (
      print_string (string_of_rval e)
    ));

    Ok RUnit
  )

  | List (Symbol "println" :: params) -> (
    let rec aux acc left =
      match left with
      | [] -> Ok (List.rev acc)
      | x :: xs -> (
        let* evaluated = eval x env in
        aux (evaluated :: acc) xs
      )
    in let* exprs = aux [] params in
    exprs |> List.iter (fun e -> (
      print_string (string_of_rval e)
    ));

    print_endline "";
    Ok RUnit
  )

  | List [Symbol "readln"] -> Ok (RString (read_line ()))
  | List [Symbol "readln"; prompt] -> (
    let* evaluated_prompt = eval prompt env in
    match evaluated_prompt with
    | RString s -> (
      print_string s;
      Out_channel.flush stdout;
      Ok (RString (read_line ()))
    )

    | _ -> Error (EvaluationError "The prompt you supplied is a non-string expression")
  )

  | List [Symbol "to_string"; e] -> (
    let* evaluated = eval e env in
    Ok (RString (string_of_rval evaluated))
  )

  | List [Symbol "string_to_num"; e] -> (
    let* x = eval e env in
    match x with
    | RString s -> (
      match float_of_string_opt s with
      | Some i -> Ok (RNum i)
      | _ -> Error (EvaluationError (sprintf "Cannot parse this string to a number: %s" s))
    )

    | _ -> Error (EvaluationError (sprintf "Cannot convert this expression to a number: %s" (string_of_rval x)))
  )

  | List [Symbol "quote"; List elems] -> (
    let rec aux acc left =
      match left with
      | [] -> Ok (List.rev acc)
      | x :: xs -> (
        let* evaluated = eval x env in
        aux (evaluated :: acc) xs 
      )
    in let* exprs = aux [] elems in 
    Ok (RList exprs)
  )
  
  | List (fun_expr :: fun_params) -> (
    let* fun_binding = eval fun_expr env in
    match fun_binding with
    | RLambda (params, body, captured) -> (
      let (expected_len, actual_len) = (List.length params, List.length fun_params) in
      match expected_len <> actual_len with
      | true -> Error (EvaluationError (
        Printf.sprintf "Attempted to invoke lambda with wrong amount of params. Expected: %d got: %d"
        expected_len actual_len
      ))

      | false -> (
        let this_env = new_env (Some captured) in
        let kv_pairs = List.combine params fun_params in
        let rec aux left =
          match left with
          | [] -> Ok ()
          | (k, v) :: xs -> (
            let* evaluated = eval v env in
            set_binding k evaluated this_env; aux xs
          )
        in match aux kv_pairs with
        | Ok _ -> eval body this_env 
        | Error e -> Error e
      )
    )

    | _ -> Error (EvaluationError (sprintf "Attempt to invoke non-lambda: %s" (string_of_expr fun_expr)))
  )

  | List [] -> Ok (RList [])

let do_string source_code env =
  let* tokens = tokenize source_code in
  let* (root_expr, _) = parse tokens in
  let* ast = expr_list_of_listlit root_expr in

  let rec aux env last_expr left =
    match left with
    | [] -> last_expr
    | x :: xs -> aux env (eval x env) xs
  in aux env (Ok RUnit) ast
      
let do_file file_path =
  let file_channel = In_channel.open_text file_path in
  let source_code = In_channel.input_all file_channel in 
  In_channel.close file_channel;
  let env = new_env None in 

  let* tokens = tokenize source_code in
  let* (root_expr, _) = parse tokens in
  let* ast = expr_list_of_listlit root_expr in

  let rec aux env left =
    match left with
    | [] -> Ok ()
    | x :: xs -> (
      match eval x env with
      | Error e -> Error e
      | _ -> aux env xs
    )
  in aux env ast


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

  | List [Symbol "throw"; throw_expr] -> (
    let* msg = eval throw_expr env in 
    match msg with
    | RString s -> Error (EvaluationError s)
    | _ -> Error (EvaluationError ("Cannot throw non-string: " ^ string_of_rval msg))
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
      let* _ = set_binding name (RLambda (p, body, env)) env in
      Ok RUnit
    )
  )

  | List [Symbol "let"; Symbol binding_name; binding_value] -> (
    let* evaluated_binding_value = eval binding_value env in
    let* _ = set_binding binding_name evaluated_binding_value env in
    Ok RUnit
  )

  | List [Symbol "mut"; Symbol binding_name; binding_value] -> (
    let* evaluated_binding_value = eval binding_value env in
    let* _ = mutate_binding binding_name evaluated_binding_value env in
    Ok RUnit
  )

  | List (Symbol "switch" :: scrutinee :: cases) -> (
    let* evaluated_scrutinee = eval scrutinee env in

    let rec aux left =
      match left with
      | [] -> Ok RUnit
      
      | List [matcher; on_match] :: xs -> (
        if matcher = Symbol "_" then
          let* evaluated = eval on_match env in
          Ok evaluated
        else
          let* evaluated_matcher = eval matcher env in
          if evaluated_matcher = evaluated_scrutinee then 
            let* evaluated = eval on_match env in
            Ok evaluated
          else
            aux xs
      )

      | _ -> Error (EvaluationError "Malformed switch-arm syntax")
    in aux cases
  )

  | List [Symbol "lambda"; List params; body] | List [Symbol "λ"; List params; body] -> (
    let rec aux acc left = 
      match left with
      | [] -> Ok (List.rev acc)
      | x :: xs -> (
        match x with
        | Symbol s -> aux (s :: acc) xs
        | _ -> Error (EvaluationError "Non-Symbol in parameter list")
      )
    in let* param_list = aux [] params in Ok (RLambda (param_list, body, env))
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
            let* _ = set_binding k evaluated this_env in 
            aux xs
          )
        in match aux kv_pairs with
        | Ok _ -> eval body this_env 
        | Error e -> Error e
      )
    )

    | RNativeFun callback -> callback fun_params env
    | _ -> Error (EvaluationError (sprintf "Attempt to invoke non-lambda: %s" (string_of_expr fun_expr)))
  )

  | List [] -> Ok (RList [])

let err name expected actual = 
  Error (EvaluationError (
    sprintf "Native function %s is given the wrong amount of arguments. Expected: %d. Got: %d" 
    name expected actual
  ))

let native_funs = [
  ("+", (fun params env -> (
    match params with
    | [lhs; rhs] -> (
      let* x = eval lhs env in
      let* y = eval rhs env in

      match (x, y) with
      | (RNum a, RNum b) -> Ok (RNum (a +. b))
      | (RString a, RString b) -> Ok (RString (a ^ b))
      | _ -> 
        Error (EvaluationError (sprintf "Cannot add these expressions: %s and %s" (string_of_rval x) (string_of_rval y)))
    )

    | _ -> err "+" 2 (List.length params)
  )));

  ("-", (fun params env -> (
    match params with
    | [lhs; rhs] -> (
      let* x = eval lhs env in
      let* y = eval rhs env in

      match (x, y) with
      | (RNum a, RNum b) -> Ok (RNum (a -. b))
      | _ -> 
        Error (EvaluationError (sprintf "Cannot subtract these expressions: %s and %s" (string_of_rval x) (string_of_rval y)))
    )

    | _ -> err "-" 2 (List.length params)
  )));

  ("*", (fun params env -> (
    match params with
    | [lhs; rhs] -> (
      let* x = eval lhs env in
      let* y = eval rhs env in

      match (x, y) with
      | (RNum a, RNum b) -> Ok (RNum (a *. b))
      | (RNum a, RString b) | (RString b, RNum a) -> Ok (RString (mul_string b a))

      | _ -> 
        Error (EvaluationError (sprintf "Cannot multiply these expressions: %s and %s" (string_of_rval x) (string_of_rval y)))
    )

    | _ -> err "*" 2 (List.length params)
  )));

  ("/", (fun params env -> (
    match params with
    | [lhs; rhs] -> (
      let* x = eval lhs env in
      let* y = eval rhs env in

      match (x, y) with
      | (RNum a, RNum b) -> Ok (RNum (a /. b))
      | _ -> 
        Error (EvaluationError (sprintf "Cannot divide these expressions: %s and %s" (string_of_rval x) (string_of_rval y)))
    )

    | _ -> err "/" 2 (List.length params)
  )));

  ("=", (fun params env -> (
    match params with
    | [a; b] -> (
      let* x = eval a env in 
      let* y = eval b env in

      match (x, y) with
      | (RNum i, RNum j) -> Ok (RBool (i = j))
      | (RString i, RString j) -> Ok (RBool (i = j))
      | (RBool i, RBool j) -> Ok (RBool (i = j))
      | (RList i, RList j) -> Ok (RBool (i = j))
      | _ -> Ok (RBool false)
    )

    | _ -> err "=" 2 (List.length params)
  )));

  (">", (fun params env -> (
    match params with
    | [a; b] -> (
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

    | _ -> err ">" 2 (List.length params)
  )));

  (">=", (fun params env -> (
    match params with
    | [a; b] -> (
      let* x = eval a env in 
      let* y = eval b env in

      match (x, y) with
      | (RNum i, RNum j) -> Ok (RBool (i > j || i = j))
      | _ -> 
        Error (EvaluationError (sprintf "Cannot perform greater/equal-comparison on non-numerical types: %s and %s" 
        (string_of_rval x) (string_of_rval y)))
    )

    | _ -> err ">=" 2 (List.length params)
  )));

  ("<", (fun params env -> (
    match params with
    | [a; b] -> (
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

    | _ -> err "<" 2 (List.length params)
  )));

  ("<=", (fun params env -> (
    match params with
    | [a; b] -> (
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

    | _ -> err "<=" 2 (List.length params)
  )));

  ("or", (fun params env -> (
    match params with
    | [a; b] -> (
      let* evaluated_a = eval a env in
      match evaluated_a with
      | RBool x -> (
        match x with
        | true -> Ok (RBool true)
        | false -> (
          let* evaluated_b = eval b env in
          match evaluated_b with
          | RBool y -> (
            match y with
            | true -> Ok (RBool true)
            | false -> Ok (RBool false)
          )

          | _ -> 
            Error (EvaluationError (sprintf "Cannot apply logical-or on non-bool expression: %s" (string_of_rval evaluated_b)))
        )
      )

      | _ -> 
        Error (EvaluationError (sprintf "Cannot apply logical-or on non-bool expression: %s" (string_of_rval evaluated_a)))
      )

    | _ -> err "or" 2 (List.length params)
  )));

  ("and", (fun params env -> (
    match params with
    | [a; b] -> (
      let* evaluated_a = eval a env in
      match evaluated_a with
      | RBool x -> (
        match x with
        | false -> Ok (RBool false)
        | true -> (
          let* evaluated_b = eval b env in
          match evaluated_b with
          | RBool y -> (
            match y with
            | true -> Ok (RBool true)
            | false -> Ok (RBool false)
          )

          | _ -> Error (EvaluationError (
            sprintf "Cannot apply logical-and on non-bool expression: %s" (string_of_rval evaluated_b)
          ))
        )
      )

      | _ -> Error (EvaluationError (
        sprintf "Cannot apply logical-and on non-bool expression: %s" (string_of_rval evaluated_a)
      ))
    )

    | _ -> err "and" 2 (List.length params)
  )));

  ("exec", (fun params env -> (
    match params with
    | [e] -> (
      let* evaluated = eval e env in
      match evaluated with
      | RString s -> Ok (RNum (float_of_int (Sys.command s)))
      | _ -> Error (EvaluationError ("Cannot execute non-string expression: " ^ (string_of_rval evaluated)))
    )

    | _ -> err "exec" 1 (List.length params)
  )));

  ("list", (fun params env -> (
    match params with
    | [List e] -> (
      let rec aux acc left =
      match left with
      | [] -> Ok (List.rev acc)
      | x :: xs -> (
        let* evaluated = eval x env in
        aux (evaluated :: acc) xs 
      )
      in let* exprs = aux [] e in 
      Ok (RList exprs)
    )

    | _ -> err "list" 1 (List.length params)
  )));

  ("append", (fun params env -> (
    match params with
    | [a; b] -> (
      let* x = eval a env in 
      let* y = eval b env in

      match (x, y) with
      | (RList i, RList j) -> Ok (RList (List.append i j))
      | _ -> Error (EvaluationError 
        (sprintf "Cannot perform list-append on these expressions: %s and %s" (string_of_rval x) (string_of_rval y))
      )
    )

    | _ -> err "append" 2 (List.length params)
  )));

  ("car", (fun params env -> (
    match params with
    | [e] -> (
      let* l = eval e env in
      match l with
      | RList content -> (
        match content with
        | head :: _ -> Ok head
        | _ -> Ok RUnit
      )

      | _ -> 
        Error (EvaluationError (sprintf "Cannot perform head-operation on non-list expression: %s" (string_of_rval l)))
    )

    | _ -> err "car" 1 (List.length params)
  )));

  ("cdr", (fun params env -> (
    match params with
    | [e] -> (
      let* l = eval e env in
      match l with
      | RList content -> (
        match content with
        | _ :: tail -> Ok (RList tail)
        | _ -> Ok RUnit
      )

      | _ -> Error (EvaluationError (sprintf "Cannot perform tail-operation on non-list expression: %s" (string_of_rval l)))
    )

    | _ -> err "cdr" 1 (List.length params)
  )));

  ("cons", (fun params env -> (
    match params with
    | [e; l] -> (
      let* x = eval l env in 
      let* y = eval e env in

      match x with
      | RList i -> Ok (RList (List.cons y i))
      | _ -> 
        Error (EvaluationError (sprintf "Cannot perform cons-operation on non-list expression: %s" (string_of_rval x)))
    )

    | _ -> err "cons" 2 (List.length params)
  )));

  ("print", (fun params env -> (
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
  )));

  ("println", (fun params env -> (
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
  )));

  ("readln", (fun params env -> (
    match params with
    | [e] -> (
      let* evaluated_prompt = eval e env in
      match evaluated_prompt with
      | RString s -> (
        print_string s;
        Out_channel.flush stdout;
        Ok (RString (read_line ()))
      )

      | _ -> Error (EvaluationError "The prompt you supplied is a non-string expression")
    )

    | _ -> err "readln" 1 (List.length params)
  )));

  ("chars", (fun params env -> (
    match params with 
    | [e] -> (
      let* str_val = eval e env in
      match str_val with
      | RString s -> (
        let chars = Helpers.chars_of_string s in
        Ok (RList (List.map (fun c -> RString (String.make 1 c)) chars))
      )

      | _ -> 
        Error (EvaluationError (sprintf "Cannot apply chars-operation on non-string expression: %s" (string_of_rval str_val)))
    )

    | _ -> err "chars" 1 (List.length params)
  )));

  ("lower", (fun params env -> (
    match params with
    | [e] -> (
      let* str_val = eval e env in
      match str_val with
      | RString s -> Ok (RString (String.lowercase_ascii s))
      | _ -> 
        Error (EvaluationError (sprintf "Cannot apply lower-operation on non-string expression: %s" (string_of_rval str_val)))
    )

    | _ -> err "lower" 1 (List.length params)
  )));

  ("trim", (fun params env -> (
    match params with
    | [e] -> (
      let* str_val = eval e env in
      match str_val with
      | RString s -> Ok (RString (String.trim s))
      | _ -> 
        Error (EvaluationError (sprintf "Cannot apply trim-operation on non-string expression: %s" (string_of_rval str_val)))
    )

    | _ -> err "trim" 1 (List.length params)
  )));

  ("splitws", (fun params env -> (
    match params with
    | [e] -> (
      let* str_val = eval e env in
      match str_val with
      | RString s -> 
        let parts = List.filter (fun s -> String.trim s <> "") (String.split_on_char ' ' s) 
        in Ok (RList (List.map (fun x -> RString x) parts))

      | _ -> 
        Error (EvaluationError (sprintf "Cannot apply splitws-operation on non-string expression: %s" (string_of_rval str_val)))
    )

    | _ -> err "splitws" 1 (List.length params)
  )));

  ("exit", (fun params env -> (
    match params with
    | [e] -> (
      let* exit_code = eval e env in
      match exit_code with
      | RNum x -> exit (int_of_float x)
      | _ -> Error (EvaluationError "Cannot exit with non-number exit code")
    )

    | _ -> err "exit" 1 (List.length params)
  )));

  ("bye", (fun _ _ -> exit 0));

  ("to_string", (fun params env -> (
    match params with
    | [e] -> (
      let* evaluated = eval e env in
      Ok (RString (string_of_rval evaluated))
    )

    | _ -> err "to_string" 1 (List.length params)
  )));

  ("string_to_num", (fun params env -> (
    match params with
    | [e] -> (
      let* x = eval e env in
      match x with
      | RString s -> (
        match float_of_string_opt s with
        | Some i -> Ok (RNum i)
        | _ -> Error (EvaluationError (sprintf "Cannot parse this string to a number: %s" s))
      )

      | _ -> Error (EvaluationError (sprintf "Cannot convert this expression to a number: %s" (string_of_rval x)))
    )

    | _ -> err "string_to_num" 1 (List.length params)
  )));

  ("isunit", (fun params env -> (
    match params with
    | [e] -> (
      match eval e env with
      | Ok RUnit -> Ok (RBool true)
      | _ -> Ok (RBool false)
    )

    | _ -> err "isunit" 1 (List.length params)
  )));

  ("isstr", (fun params env -> (
    match params with
    | [e] -> (
      match eval e env with
      | Ok RString _ -> Ok (RBool true)
      | _ -> Ok (RBool false)
    )

    | _ -> err "isstring" 1 (List.length params)
  )));

  ("isnum", (fun params env -> (
    match params with
    | [e] -> (
      match eval e env with
      | Ok RNum _ -> Ok (RBool true)
      | _ -> Ok (RBool false)
    )

    | _ -> err "isnum" 1 (List.length params)
  )));

  ("islist", (fun params env -> (
    match params with
    | [e] -> (
      match eval e env with
      | Ok RList _ -> Ok (RBool true)
      | _ -> Ok (RBool false)
    )

    | _ -> err "islist" 1 (List.length params)
  )));

  ("isfun", (fun params env -> (
    match params with
    | [e] -> (
      match eval e env with
      | Ok RLambda _ -> Ok (RBool true)
      | _ -> Ok (RBool false)
    )

    | _ -> err "islambda" 1 (List.length params)
  )));

  ("isnative", (fun params env -> (
    match params with
    | [e] -> (
      match eval e env with
      | Ok RNativeFun _ -> Ok (RBool true)
      | _ -> Ok (RBool false)
    )

    | _ -> err "isnative" 1 (List.length params)
  )));

  ("isbool", (fun params env -> (
    match params with
    | [e] -> (
      match eval e env with
      | Ok RBool _ -> Ok (RBool true)
      | _ -> Ok (RBool false)
    )

    | _ -> err "isbool" 1 (List.length params)
  )));

  ("print_env", (fun params env -> (
    match params with
    | [] -> (
      let rec aux e idx =
        printf "Scope %d:\n" idx;
        e.bindings |>
        Hashtbl.iter (fun k v -> printf "\t%s: %s\n" k (string_of_rval v));

        match e.parent with
        | None -> Ok RUnit
        | Some p -> aux p (idx + 1)
      in aux env 0 
    )

    | _ -> err "print_env" 0 (List.length params)
  )));
]

let register_std_natives env =
  native_funs |>
  List.iter (fun (name, callback) ->
    register_native env name callback |> ignore
  )

let do_string source_code env =
  let* tokens = tokenize source_code in
  let* (root_expr, _) = parse tokens in
  let* ast = expr_list_of_listlit root_expr in

  let rec aux env last_expr left =
    match left with
    | [] -> last_expr
    | x :: xs -> aux env (eval x env) xs
  in aux env (Ok RUnit) ast
      
let load_stdlib env =
  Stdlib.stdlib_src |>
  List.iter (fun f -> do_string f env |> ignore)

let do_file file_path =
  let file_channel = In_channel.open_text file_path in
  let source_code = In_channel.input_all file_channel in 
  In_channel.close file_channel;
  let env = new_env None in
  load_stdlib env;
  register_std_natives env;

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

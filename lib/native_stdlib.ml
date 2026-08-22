open Nomad_err
open Eval
open Printf
open Runtime_value
open Helpers
open Expr

let err t e v = 
  Error (EvaluationError (
    sprintf "This expression was expected to evaluate to a %s, but it didn't: %s (%s)" 
    t (string_of_expr e) (string_of_rval v)
  )) 

let get_string expr env =
  let* evaluated = eval expr env in
  match evaluated with
  | RString s -> Ok s
  | _ -> err "string" expr evaluated

let get_number expr env =
  let* evaluated = eval expr env in
  match evaluated with
  | RNum x -> Ok x
  | _ -> err "number" expr evaluated
  
let get_bool expr env =
  let* evaluated = eval expr env in
  match evaluated with
  | RBool b -> Ok b
  | _ -> err "bool" expr evaluated

let get_list expr env =
  let* evaluated = eval expr env in
  match evaluated with
  | RList l -> Ok l
  | _ -> err "list" expr evaluated

let get_lambda expr env =
  let* evaluated = eval expr env in
  match evaluated with
  | RLambda (params, body, captured) -> Ok (params, body, captured)
  | _ -> err "lambda" expr evaluated

let get_record expr env =
  let* evaluated = eval expr env in
  match evaluated with
  | RRecord r -> Ok r
  | _ -> err "record" expr evaluated

let get_native expr env =
  let* evaluated = eval expr env in
  match evaluated with
  | RNativeFun r -> Ok r
  | _ -> err "native function" expr evaluated

let get_unit expr env =
  let* evaluated = eval expr env in
  match evaluated with
  | RUnit -> Ok RUnit
  | _ -> err "unit" expr evaluated

let err name expected actual = 
  Error (EvaluationError (
    sprintf "Native function %s was given bad syntax. Perhaps it was given the wrong amount of args? Args expected: %d. Got: %d"
    name expected actual
  ))

let native_funs = [
  ("try", (fun params env -> (
    match params with
    | [may_fail; on_fail] -> (
      let evaluated = eval may_fail env in
      match evaluated with
      | Ok e -> Ok e
      | Error e -> eval on_fail env
    )

    | _ -> err "try" 2 (List.length params)
  )));

  ("throw", (fun params env -> (
    match params with
    | [throw_expr] -> (
      let* msg = eval throw_expr env in 
      match msg with
      | RString s -> Error (EvaluationError s)
      | _ -> Error (EvaluationError ("Cannot throw non-string: " ^ string_of_rval msg))
    )

    | _ -> err "throw" 1 (List.length params)
  )));

  ("letmac", (fun params env -> (
    match params with
    | Symbol name :: List params :: body -> (
      let rec aux acc left =
        match left with
        | [] -> Ok (List.rev acc)
        | x :: xs -> (
          match x with
          | Symbol s -> aux (s :: acc) xs
          | _ -> Error (EvaluationError "Non-symbol in parameter list")
        )
      in

      let* args = aux [] params in
      let* _ = set_binding name (RMacro (args, body)) env in
      Ok RUnit
    )

    | _ -> err "letmac" 3 (List.length params)
  )));

  ("let", (fun params env -> (
    match params with
    | [Symbol binding_name; binding_expr] -> (
      let* evaluated_binding_value = eval binding_expr env in
      let* _ = set_binding binding_name evaluated_binding_value env in
      Ok RUnit
    )

    | _ -> err "let" 2 (List.length params)
  )));

  ("letfun", (fun params env -> (
    match params with
    | [Symbol name; List params; body] -> (
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

    | _ -> err "letfun" 3 (List.length params)
  )));

  ("mut", (fun params env -> (
    match params with
    | [Symbol binding_name; binding_value] -> (
      let* evaluated_binding_value = eval binding_value env in
      let* _ = mutate_binding binding_name evaluated_binding_value env in
      Ok RUnit
    )

    | _ -> err "mut" 2 (List.length params)
  )));

  ("switch", (fun params env -> (
    match params with
    | scrutinee :: cases -> (
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

    | _ -> err "switch" 2 (List.length params)
  )));

  ("lambda", (fun params env -> (
    match params with
    | [List params; body] -> (
      let rec aux acc left = 
        match left with
        | [] -> Ok (List.rev acc)
        | x :: xs -> (
          match x with
          | Symbol s -> aux (s :: acc) xs
          | _ -> Error (EvaluationError "Non-Symbol in parameter list")
        )
      in let* param_list = aux [] params in 
      Ok (RLambda (param_list, body, env))
    )

    | _ -> err "lambda" 2 (List.length params)
  )));

  ("scoped", (fun params env -> (
    match params with
    | [List binding_pairs; body] -> (
      let rec aux acc left = 
        match left with
        | [] -> Ok acc
        | x :: xs -> (
          match x with
          | List [Symbol s; binding_expr] -> (
            let* evaluated = eval binding_expr env in
            aux ((s, evaluated) :: acc) xs
          )

          | _ -> Error (EvaluationError "Bad Syntax! The binding list is in the wrong form! (Expected '(name value)')")
        )
      in

      let* pairs = aux [] binding_pairs in
      let this_env = new_env (Some env) in
      let rec aux left =
        match left with
        | [] -> Ok ()
        | (binding_name, binding_value) :: xs -> (
          let* _ = set_binding binding_name binding_value this_env in
          aux xs
        )
      in let* _ = aux pairs in

      let* evaluated = eval body this_env in
      Ok evaluated
    )

    | _ -> err "scoped" 2 (List.length params)
  )));

  ("do", (fun params env -> (
    let rec aux last_expr left = 
    match left with
    | [] -> Ok last_expr
    | x :: xs -> (
      match eval x env with
      | Ok e -> aux e xs 
      | Error e -> Error e
    )
    in aux RUnit params
  )));

  ("if", (fun params env -> (
    match params with
    | [cond; yes; no] -> (
      let* evaluated_cond = eval cond env in
      match evaluated_cond with
      | RBool b -> (
        match b with
        | true -> eval yes env
        | false -> eval no env
      )

      | _ -> Error (EvaluationError (
        sprintf "Condition of if-construct does not evaluate to a bool: %s" (string_of_rval evaluated_cond)
      ))
    )

    | _ -> err "if" 3 (List.length params)
  )));

  ("record", (fun params env -> (
    let acc = Hashtbl.create 0 in
    let rec aux left = 
      match left with
      | [] -> Ok (RRecord acc)
      | x :: xs -> (
        match x with
        | List [Symbol field_name; field_expr] -> (
          let* evaluated_field = eval field_expr env in
          Hashtbl.add acc field_name evaluated_field;
          aux xs
        )

        | _ -> Error (EvaluationError "Record field has bad syntax")
      )

    in aux params
  )));

  (".", (fun params env -> (
    match params with
    | [record_expr; Symbol field_name] -> (
      let* evaluated_record = eval record_expr env in 
      match evaluated_record with
      | RRecord r -> (
        match Hashtbl.find_opt r field_name with
        | Some f -> Ok f
        | None -> 
          Error (EvaluationError ("Attempt to access non-existant field of record: " ^ field_name))
      )

      | _ -> Error (EvaluationError ("Attempt to access field of non-record expression: " ^ (string_of_rval evaluated_record)))
    )

    | _ -> err "." 2 (List.length params)
  )));

  ("record_mut", (fun params env -> (
    match params with
    | [record_expr; Symbol field_name; new_expr] -> (
      let* evaluated_record = eval record_expr env in
      match evaluated_record with
      | RRecord r -> (
        match Hashtbl.find_opt r field_name with
        | None -> Error (EvaluationError ("Cannot mutate non-existant field: " ^ field_name))
        | Some _ -> (
          let* evaluated_expr = eval new_expr env in
          Hashtbl.replace r field_name evaluated_expr;

          Ok RUnit
        )
      )

      | _ -> Error (EvaluationError ("Attempt to mutate field of non-record expression: " ^ (string_of_rval evaluated_record)))
    )

    | _ -> err "record_mut" 3 (List.length params)
  )));

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
      let* x = get_number lhs env in
      let* y = get_number rhs env in
      Ok (RNum (x -. y))
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
      let* x = get_number lhs env in
      let* y = get_number rhs env in
      match y = 0. with
      | false -> Ok (RNum (x /. y))
      | true -> Error (EvaluationError "Attempt to divide by 0")
    )

    | _ -> err "/" 2 (List.length params)
  )));

  ("mod", (fun params env -> (
    match params with
    | [lhs; rhs] -> (
      let* x = get_number lhs env in
      let* y = get_number rhs env in
      Ok (RNum (mod_float x y))
    )

    | _ -> err "mod" 2 (List.length params)
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
      let* x = get_number a env in 
      let* y = get_number b env in
      Ok (RBool (x > y))
    )

    | _ -> err ">" 2 (List.length params)
  )));

  (">=", (fun params env -> (
    match params with
    | [a; b] -> (
      let* x = get_number a env in 
      let* y = get_number b env in
      Ok (RBool (x > y || x = y))
    )

    | _ -> err ">=" 2 (List.length params)
  )));

  ("<", (fun params env -> (
    match params with
    | [a; b] -> (
      let* x = get_number a env in 
      let* y = get_number b env in
      Ok (RBool (x < y))
    )

    | _ -> err "<" 2 (List.length params)
  )));

  ("<=", (fun params env -> (
    match params with
    | [a; b] -> (
      let* x = get_number a env in 
      let* y = get_number b env in
      Ok (RBool (x < y || x = y))
    )

    | _ -> err "<=" 2 (List.length params)
  )));

  ("or", (fun params env -> (
    match params with
    | [a; b] -> (
      let* evaluated_a = get_bool a env in
      match evaluated_a with
      | true -> Ok (RBool true)
      | false -> (
        let* evaluated_b = get_bool b env in
        match evaluated_b with
        | true -> Ok (RBool true)
        | false -> Ok (RBool false)
      )
    )

    | _ -> err "or" 2 (List.length params)
  )));

  ("and", (fun params env -> (
    match params with
    | [a; b] -> (
      let* evaluated_a = get_bool a env in
      match evaluated_a with
      | false -> Ok (RBool false)
      | true -> (
        let* evaluated_b = get_bool b env in
        match evaluated_b with
        | true -> Ok (RBool true)
        | false -> Ok (RBool false)
      )
    )

    | _ -> err "and" 2 (List.length params)
  )));

  ("exec", (fun params env -> (
    match params with
    | [e] -> (
      let* evaluated = get_string e env in
      Ok (RNum (float_of_int (Sys.command evaluated)))
    )

    | _ -> err "exec" 1 (List.length params)
  )));

  ("list", (fun params env -> (
    let rec aux acc left =
    match left with
    | [] -> Ok (List.rev acc)
    | x :: xs -> (
      let* evaluated = eval x env in
      aux (evaluated :: acc) xs 
    )
    in let* exprs = aux [] params in 
    Ok (RList exprs)
  )));

  ("append", (fun params env -> (
    match params with
    | [a; b] -> (
      let* x = get_list a env in 
      let* y = get_list b env in
      Ok (RList (List.append x y))
    )

    | _ -> err "append" 2 (List.length params)
  )));

  ("car", (fun params env -> (
    match params with
    | [e] -> (
      let* l = get_list e env in
      match l with
      | head :: _ -> Ok head
      | _ -> Ok RUnit
    )

    | _ -> err "car" 1 (List.length params)
  )));

  ("cdr", (fun params env -> (
    match params with
    | [e] -> (
      let* l = get_list e env in
        match l with
        | _ :: tail -> Ok (RList tail)
        | _ -> Ok RUnit
      )

    | _ -> err "cdr" 1 (List.length params)
  )));

  ("cons", (fun params env -> (
    match params with
    | [e; l] -> (
      let* x = get_list l env in 
      let* y = eval e env in
      Ok (RList (List.cons y x))
    )

    | _ -> err "cons" 2 (List.length params)
  )));

  ("sprint", (fun params env -> (
    let rec aux acc left =
      match left with
      | [] -> Ok acc
      | x :: xs -> (
        let* evaluated = eval x env in
        aux (acc ^ (string_of_rval evaluated)) xs
      )
    in let* str = aux "" params in
    Ok (RString str)
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
      let* evaluated_prompt = get_string e env in
      print_string evaluated_prompt;
      Out_channel.flush stdout;
      Ok (RString (read_line ()))
    )

    | _ -> err "readln" 1 (List.length params)
  )));

  ("chars", (fun params env -> (
    match params with 
    | [e] -> (
      let* str_val = get_string e env in
      let chars = Helpers.chars_of_string str_val in
      Ok (RList (List.map (fun c -> RString (String.make 1 c)) chars))
    )

    | _ -> err "chars" 1 (List.length params)
  )));

  ("lower", (fun params env -> (
    match params with
    | [e] -> (
      let* str_val = get_string e env in
      Ok (RString (String.lowercase_ascii str_val))
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
      let* str_val = get_string e env in
      let parts = List.filter (fun s -> String.trim s <> "") (String.split_on_char ' ' str_val) 
      in Ok (RList (List.map (fun x -> RString x) parts))
    )

    | _ -> err "splitws" 1 (List.length params)
  )));

  ("exit", (fun params env -> (
    match params with
    | [e] -> (
      let* exit_code = get_number e env in
      exit (int_of_float exit_code)
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
      let* x = get_string e env in
      match float_of_string_opt x with
      | Some i -> Ok (RNum i)
      | _ -> Error (EvaluationError (sprintf "Cannot parse this string to a number: %s" x))
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

  ("ismac", (fun params env -> (
    match params with
    | [e] -> (
      match eval e env with
      | Ok RMacro _ -> Ok (RBool true)
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

  ("isrecord", (fun params env -> (
    match params with
    | [e] -> (
      match eval e env with
      | Ok RBool _ -> Ok (RBool true)
      | _ -> Ok (RBool false)
    )

    | _ -> err "isrecord" 1 (List.length params)
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

  ("include", (fun params env -> (
    match params with
    | [Symbol path] -> (
      try
        let chan = In_channel.open_text path in
        let content = In_channel.input_all chan in
        In_channel.close chan;
        let* _ = do_string content env in
        Ok RUnit
      with Sys_error e -> Error (EvaluationError (sprintf "Error while including '%s': %s" path e))
    )

    | _ -> err "include" 1 (List.length params)
  )));

  ("read_file", (fun params env -> (
    match params with
    | [path_expr] -> (
      let* evaluated = get_string path_expr env in
      try
        let chan = In_channel.open_text evaluated in
        let content = In_channel.input_all chan in
        In_channel.close chan;
        Ok (RString content)
      with Sys_error e -> Error (EvaluationError (sprintf "Error while reading '%s': %s" evaluated e))
    )

    | _ -> err "read_file" 1 (List.length params)
  )));

  ("write_file", (fun params env -> (
    match params with
    | [path_expr; content_expr] -> (
      let* path = get_string path_expr env in
      let* content = get_string content_expr env in

      try
        let chan = Out_channel.open_text path in
        Out_channel.output_string chan content;
        Out_channel.close chan;
        Ok RUnit
      with Sys_error e ->
        Error (EvaluationError (sprintf "Couldn't write to '%s': %s" path e))
    )

    | _ -> err "write_file" 1 (List.length params)
  )));

  ("remove_file", (fun params env -> (
    match params with
    | [path_expr] -> (
      let* path = get_string path_expr env in
      try
        Sys.remove path;
        Ok RUnit
      with Sys_error e ->
        Error (EvaluationError (sprintf "Couldn't remove file '%s': %s" path e))
    )

    | _ -> err "remove_file" 1 (List.length params)
  )));

  ("read_dir", (fun params env -> (
    match params with
    | [path_expr] -> (
      let* path = get_string path_expr env in
      let rec aux acc left =
        match left with
        | [] -> List.rev acc
        | x :: xs -> aux (RString x :: acc) xs
      in try
        Ok (RList (aux [] (Array.to_list (Sys.readdir path))))
      with Sys_error e ->
        Error (EvaluationError (sprintf "Couldn't read directory '%s': %s" path e))
    )

    | _ -> err "read_dir" 1 (List.length params)
  )));

  ("mkdir", (fun params env -> (
    match params with
    | [path_expr] -> (
      let* path = get_string path_expr env in
      try
        Sys.mkdir path 0755;
        Ok RUnit
      with Sys_error e ->
        Error (EvaluationError (sprintf "Couldn't create directory '%s': %s" path e))
    )

    | _ -> err "mkdir" 1 (List.length params)
  )));

  ("remove_dir", (fun params env -> (
    match params with
    | [path_expr] -> (
      let* path = get_string path_expr env in
      try
        Sys.rmdir path;
        Ok RUnit
      with Sys_error e ->
        Error (EvaluationError (sprintf "Couldn't remove directory: '%s': %s" path e))
    )

    | _ -> err "remove_dir" 1 (List.length params)
  )));

  ("chdir", (fun params env -> (
    match params with
    | [path_expr] -> (
      let* path = get_string path_expr env in
      try 
        Sys.chdir path;
        Ok RUnit
      with Sys_error e ->
        Error (EvaluationError (sprintf "Error while changing working directory to '%s': %s" path e))
    )

    | _ -> err "chdir" 1 (List.length params)
  )));

  ("cwd", (fun params env -> (
    match params with
    | [] -> Ok (RString (Sys.getcwd ()))
    | _ -> err "cwd" 0 (List.length params)
  )));

  ("get_env", (fun params env -> (
    match params with
    | [var_expr] -> (
      let* x = get_string var_expr env in
      match Sys.getenv_opt x with
      | Some v -> Ok (RString v)
      | None -> Error (EvaluationError (sprintf "Environment variable '%s' not found" x))
    )

    | _ -> err "get_env" 1 (List.length params)
  )));

  ("get_env_unit", (fun params env -> (
    match params with
    | [var_expr] -> (
      let* x = get_string var_expr env in
      match Sys.getenv_opt x with
      | Some v -> Ok (RString v)
      | None -> Ok RUnit
    )

    | _ -> err "get_env" 1 (List.length params)
  )));
]

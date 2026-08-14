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
  
  | List (fun_expr :: fun_params) -> (
    let* fun_binding = eval fun_expr env in
    match fun_binding with
    | RNativeFun callback -> callback fun_params env
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

    | RMacro (params, body) -> (
      let (expected_len, actual_len) = (List.length params, List.length fun_params) in
      match expected_len <> actual_len with
      | true -> Error (EvaluationError (
        Printf.sprintf "Attempted to invoke macro with wrong amount of params. Expected: %d got: %d"
        expected_len actual_len
      ))

      | false -> (
        let kv_pairs = List.combine params fun_params in
        let tbl = Hashtbl.create 0 in
        kv_pairs |> List.iter (fun (k, v) -> Hashtbl.add tbl k v);

        let rec aux acc left =
          match left with
          | [] -> List (List.rev acc)
          | x :: xs -> (
            match x with
            | Symbol s -> (
              match Hashtbl.find_opt tbl s with
              | Some e -> aux (e :: acc) xs
              | None -> aux (Symbol s :: acc) xs
            )

            | List l -> aux (aux [] l :: acc) xs
            | _ -> aux (x :: acc) xs
          )
        in let new_expr = aux [] body in
        eval new_expr env
      )
    )

    | _ -> Error (EvaluationError (sprintf "Attempt to invoke non-function/non-macro: %s" (string_of_expr fun_expr)))
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

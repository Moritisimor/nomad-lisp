open Runtime_value
open Expr
open Helpers
open Tokens
open Parser
open Nomad_err
open Printf

let core_forms : (string, expr list -> env -> (runtime_value, error) result) Hashtbl.t = Hashtbl.create 5

let register_core name callback = Hashtbl.replace core_forms name callback

let is_core name callback =
  match Hashtbl.find_opt core_forms name with
  | Some expected -> expected == callback
  | None -> false

let arity name expected got =
  EvaluationError (sprintf
    "Native function %s was given bad syntax. Perhaps it was given the wrong amount of args? Args expected: %d. Got: %d"
    name expected got)

let rec substitute table = function
  | Symbol name as expression -> Option.value (Hashtbl.find_opt table name) ~default:expression
  | List items -> List (List.map (substitute table) items)
  | expression -> expression

let rec eval expression env =
  let rec resume handlers error =
    match error, handlers with
    | EvaluationError _, (handler, handler_env) :: rest -> loop handler handler_env rest
    | _ -> Error error

  and eval_earlier expressions scope =
    match expressions with
    | [] -> Ok ()
    | expression :: rest ->
        begin match eval expression scope with
        | Ok _ -> eval_earlier rest scope
        | Error error -> Error error
        end

  and eval_args expressions scope =
    let rec collect acc = function
      | [] -> Ok (List.rev acc)
      | expression :: rest ->
          let* value = eval expression scope in
          collect (value :: acc) rest
    in
    collect [] expressions

  and loop cursor scope handlers =
    match cursor with
    | NumLit x -> Ok (RNum x)
    | StringLit x -> Ok (RString x)
    | BoolLit x -> Ok (RBool x)
    | Unit -> Ok RUnit
    | Symbol name ->
        begin match get_binding name scope with
        | Ok value -> Ok value
        | Error error -> resume handlers error
        end
    | Lambda (params, body) -> Ok (RLambda (params, body, scope))
    | List [] -> Ok (RList [])
    | List (function_expression :: arguments) ->
        begin match eval function_expression scope with
        | Error error -> resume handlers error
        | Ok (RLambda (params, body, captured)) ->
            if List.length params <> List.length arguments then
              resume handlers (EvaluationError (sprintf
                "Attempted to invoke lambda with wrong amount of params. Expected: %d got: %d"
                (List.length params) (List.length arguments)))
            else
              begin match eval_args arguments scope with
              | Error error -> resume handlers error
              | Ok values ->
                  let local = new_env ~capacity:(List.length params) (Some captured) in
                  let rec bind names values =
                    match names, values with
                    | [], [] -> Ok ()
                    | name :: names, value :: values ->
                        let* () = set_binding name value local in
                        bind names values
                    | _ -> assert false
                  in
                  begin match bind params values with
                  | Ok () -> loop body local handlers
                  | Error error -> resume handlers error
                  end
              end
        | Ok (RMacro (params, body)) ->
            if List.length params <> List.length arguments then
              resume handlers (EvaluationError (sprintf
                "Attempted to invoke macro with wrong amount of params. Expected: %d got: %d"
                (List.length params) (List.length arguments)))
            else
              let table = Hashtbl.create (List.length params) in
              List.iter2 (Hashtbl.replace table) params arguments;
              loop (substitute table (List body)) scope handlers
        | Ok (RNativeFun callback) ->
            begin match function_expression with
            | Symbol "if" when is_core "if" callback ->
                begin match arguments with
                | [condition; yes; no] ->
                    begin match eval condition scope with
                    | Ok (RBool true) -> loop yes scope handlers
                    | Ok (RBool false) -> loop no scope handlers
                    | Ok other -> resume handlers (EvaluationError (sprintf
                        "Condition of if-construct does not evaluate to a bool: %s" (string_of_rval other)))
                    | Error error -> resume handlers error
                    end
                | _ -> resume handlers (arity "if" 3 (List.length arguments))
                end
            | Symbol "do" when is_core "do" callback ->
                begin match List.rev arguments with
                | [] -> Ok RUnit
                | last :: reversed_earlier ->
                    begin match eval_earlier (List.rev reversed_earlier) scope with
                    | Ok () -> loop last scope handlers
                    | Error error -> resume handlers error
                    end
                end
            | Symbol "switch" when is_core "switch" callback ->
                begin match arguments with
                | scrutinee :: (_ :: _ as cases) ->
                    begin match eval scrutinee scope with
                    | Error error -> resume handlers error
                    | Ok scrutinee_value ->
                        let rec choose = function
                          | [] -> Ok None
                          | List [Symbol "_"; on_match] :: _ -> Ok (Some on_match)
                          | List [matcher; on_match] :: rest ->
                              begin match eval matcher scope with
                              | Ok value when equal value scrutinee_value -> Ok (Some on_match)
                              | Ok _ -> choose rest
                              | Error error -> Error error
                              end
                          | _ -> Error (EvaluationError "Malformed switch-arm syntax")
                        in
                        begin match choose cases with
                        | Ok (Some on_match) -> loop on_match scope handlers
                        | Ok None -> Ok RUnit
                        | Error error -> resume handlers error
                        end
                    end
                | _ -> resume handlers (arity "switch" 2 (List.length arguments))
                end
            | Symbol "scoped" when is_core "scoped" callback ->
                begin match arguments with
                | [List pairs; body] ->
                    let local = new_env ~capacity:(List.length pairs) (Some scope) in
                    let rec bind = function
                      | [] -> Ok ()
                      | List [Symbol name; binding] :: rest ->
                          let* value = eval binding scope in
                          let* () = set_binding name value local in
                          bind rest
                      | _ -> Error (EvaluationError
                          "Bad Syntax! The binding list is in the wrong form! (Expected '(name value)')")
                    in
                    begin match bind pairs with
                    | Ok () -> loop body local handlers
                    | Error error -> resume handlers error
                    end
                | _ -> resume handlers (arity "scoped" 2 (List.length arguments))
                end
            | Symbol "try" when is_core "try" callback ->
                begin match arguments with
                | [may_fail; on_fail] -> loop may_fail scope ((on_fail, scope) :: handlers)
                | _ -> resume handlers (arity "try" 2 (List.length arguments))
                end
            | _ ->
                begin match callback arguments scope with
                | Ok value -> Ok value
                | Error error -> resume handlers error
                end
            end
        | Ok other -> resume handlers (EvaluationError (sprintf
            "Attempt to invoke non-function/non-macro: %s (%s)"
            (string_of_expr function_expression) (string_of_rval other)))
        end
  in
  loop expression env []

let eval_seq expressions env =
  let rec loop last = function
    | [] -> Ok last
    | expression :: rest ->
        let* value = eval expression env in
        loop value rest
  in
  loop RUnit expressions

let do_string source_code env =
  let* tokens = tokenize source_code in
  let* root_expr, _ = parse tokens in
  let* ast = expr_list_of_listlit root_expr in
  eval_seq ast env

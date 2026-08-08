(* This module is meant for user-facing APIs *)

open Runtime_value
open Tokens
open Parser
open Helpers
open Eval
open Native_stdlib

let register_std_natives env =
  native_funs |>
  List.iter (fun (name, callback) ->
    register_native env name callback |> ignore
  )

let load_stdlib env =
  Stdlib.stdlib_src |>
  List.iter (fun f -> do_string f env |> ignore)

(* Just a small convenience function for creating an environment and binding the stdlib as well. *)
let new_interpreter = 
  let e = new_env None in
  let rec aux acc left =
    match left with
    | [] -> List.rev acc
    | x :: xs -> aux (RString x :: acc) xs
  in let args = aux [] (List.tl (Array.to_list Sys.argv)) in

  set_binding "args" (RList args) e |> ignore;
  register_std_natives e;
  load_stdlib e;
  e

let do_string source_code env = Eval.do_string source_code env

let load_stdlib env =
  Stdlib.stdlib_src |>
  List.iter (fun f -> do_string f env |> ignore)

let do_file file_path =
  let file_channel = In_channel.open_text file_path in
  let source_code = In_channel.input_all file_channel in 
  In_channel.close file_channel;
  let env = new_interpreter in

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

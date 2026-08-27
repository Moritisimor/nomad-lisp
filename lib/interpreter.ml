open Runtime_value
open Eval
open Native_stdlib
open Nomad_err

let ( let* ) = Result.bind

let register_std_natives env =
  List.iter (fun (name, callback) ->
    match register_native env name callback with
    | Ok () -> ()
    | Error _ -> assert false
  ) native_funs

let register_core_forms env =
  List.iter (fun name ->
    match get_binding name env with
    | Ok (RNativeFun callback) -> register_core name callback
    | _ -> assert false
  ) ["if"; "do"; "switch"; "scoped"; "try"]

let parsed_stdlib = lazy (
  let parse source =
    let* tokens = Tokens.tokenize source in
    let* root, _ = Parser.parse tokens in
    Parser.expr_list_of_listlit root
  in
  let rec gather acc = function
    | [] -> Ok (List.rev acc)
    | source :: rest ->
        let* expressions = parse source in
        gather (List.rev_append expressions acc) rest
  in
  gather [] Stdlib.stdlib_src
)

let load_stdlib env =
  match Lazy.force parsed_stdlib with
  | Error error -> Error error
  | Ok expressions -> eval_seq expressions env

let create ?(args = []) () =
  let env = new_env ~capacity:64 None in
  let values = List.map (fun value -> RString value) args in
  let* () = set_binding "args" (RList values) env in
  register_std_natives env;
  register_core_forms env;
  let* _ = load_stdlib env in
  Ok env

let new_interpreter =
  let args = Array.to_list Sys.argv |> List.tl in
  match create ~args () with
  | Ok env -> env
  | Error error ->
      Nomad_err.print_err error;
      failwith "failed to initialize standard library"

let do_string = Eval.do_string

let do_file_in env file_path =
  try
    let channel = In_channel.open_text file_path in
    Fun.protect
      ~finally:(fun () -> In_channel.close channel)
      (fun () -> Eval.do_string (In_channel.input_all channel) env)
  with Sys_error message -> Error (IoError message)

let do_file file_path =
  let* env = create ~args:(Array.to_list Sys.argv |> List.tl) () in
  let* _ = do_file_in env file_path in
  Ok ()

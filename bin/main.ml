open Nomad_lisp.Eval
open Nomad_lisp.Runtime_value
open Nomad_lisp.Env
open Nomad_lisp

let () =
  match Sys.argv with
  | [|_; "--help"|] | [|_ ; "-h"|] -> (
    print_endline " \\\\";
    print_endline "  \\\\";
    print_endline " //\\\\";
    print_endline "//  \\\\ \n";

    print_endline "The Magnificent Nomad-LISP Interpretation System";
    print_endline "https://github.com/Moritisimor/nomad-lisp";
  )

  | [|_; "-e"; expr|] | [|_; "--eval"; expr|] -> (
    match Eval.do_string expr (Env.new_env ()) with
    | Ok evaluated -> print_endline (Runtime_value.string_of_rval evaluated)
    | Error e -> (
      print_endline e;
      exit 1
    )
  )

  | [|_|] | [|_; "--repl"|] | [|_; "-r"|] -> (
    let env = new_env () in
    let rec repl () =
      print_string "Nomad λ ";
      Out_channel.flush stdout;
      let input = read_line () in
      (match do_string input env with
      | Ok evaluated -> Printf.printf "Evaluates to: %s\n" (string_of_rval evaluated)
      | Error e -> print_endline e);
      repl ()
    in repl ()
  )

  | [|_; input_file|] -> (
    try
      match Eval.do_file input_file with
      | Ok _ -> ()
      | Error e -> print_endline e; exit 1
    with Sys_error e -> (
      print_endline ("Error while reading file: " ^ e);
      exit 1
    )
  )
  
  | _ -> (
    print_endline "Usage: nomad <input file>\n\tor omit input_file for REPL.\n\nSet '--help' flag for more info.";
    exit 1
  )

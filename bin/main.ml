open Nomad_lisp.Eval
open Nomad_lisp.Runtime_value
open Nomad_lisp.Env

let () =
  match Sys.argv with
  | [|_; "--help"|] -> (
    print_endline " \\\\";
    print_endline "  \\\\";
    print_endline " //\\\\";
    print_endline "//  \\\\ \n";

    print_endline "The Magnificent Nomad-LISP Interpretation System";
    print_endline "https://github.com/Moritisimor/nomad-lisp";
  )

  | [|_|] | [|_; "--repl"|] -> (
    let env = new_env () in
    let rec repl () =
      print_string "Nomad LISP REPL >> ";
      Out_channel.flush stdout;
      let input = read_line () in
      (match do_string input env with
      | Ok evaluated -> Printf.printf "Evaluates to: %s\n" (string_of_rval evaluated)
      | Error e -> print_endline e);
      repl ()
    in repl ()
  )
  
  | _ -> (
    print_endline "Usage: nomad <input file>\n\tor omit input_file for REPL.\n\nSet '--help' flag for more info.";
    exit 1
  )

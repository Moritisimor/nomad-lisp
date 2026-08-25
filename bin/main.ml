open Nomad_lisp.Eval
open Nomad_lisp.Runtime_value
open Nomad_lisp.Expr
open Nomad_lisp

let () =
  match Array.to_list Sys.argv with
  | [_; "--help"] | [_ ; "-h"] -> (
    print_endline " \\\\";
    print_endline "  \\\\";
    print_endline " //\\\\";
    print_endline "//  \\\\ \n";

    print_endline "The Magnificent Nomad-LISP Interpretation System\n";
    print_endline "Omit all arguments to enter REPL Mode.";
    print_endline "Use the -e | --eval flag to evaluate an expression which is passed as an argument.";
    print_endline "\tExample: nomad -e '(+ 1 2)' # => 3\n";
    print_endline "You can also pass a file to be run as a script.";
    print_endline "\tExample: nomad my_script.nomad\n";
    print_endline "For More information, visit:";
    print_endline "https://github.com/Moritisimor/nomad-lisp";
  )

  | [_; "-e"; expr] | [_; "--eval"; expr] -> (
    match Interpreter.do_string expr Interpreter.new_interpreter with
    | Ok evaluated -> print_endline (Runtime_value.string_of_rval evaluated)
    | Error e -> (
      Nomad_err.print_err e;
      exit 1
    )
  )

  | [_] | [_; "--repl"] | [_; "-r"] -> (
    let interpreter = Interpreter.new_interpreter in
    let rec repl () =
      let input = 
        match LNoise.linenoise "Nomad REPL >> " with
        | Some i -> (
          LNoise.history_add i |> ignore;
          i
        )
        
        | None -> (
          print_endline "Bye!";
          exit 0
        )
      in

      (match Interpreter.do_string input interpreter with
      | Error e -> Nomad_err.print_err e
      | Ok evaluated -> (
        Printf.printf "Evaluates to: %s\n" (string_of_rval evaluated);
        Out_channel.flush stdout
      ));
      repl ()
    in repl ()
  )

  | _ :: input_file :: _ -> (
    try
      match Interpreter.do_file input_file with
      | Ok _ -> ()
      | Error e -> Nomad_err.print_err e; exit 1
    with Sys_error e -> (
      print_endline ("Error while reading file: " ^ e);
      exit 1
    )
  )
  
  | _ -> (
    print_endline "Usage: nomad <input file>\n\tor omit input_file for REPL.\n\nSet '--help' flag for more info.";
    exit 1
  )

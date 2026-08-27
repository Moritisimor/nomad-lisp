type error =
  | ParseError of string
  | TokenizerError of string
  | EvaluationError of string
	| IoError of string
	| Exit of int

let print_err = function
  | ParseError e -> print_endline ("Error while parsing: " ^ e)
  | TokenizerError e -> print_endline ("Error while tokenizing: " ^ e)
  | EvaluationError e -> print_endline ("Error while evaluating: " ^ e)
	| IoError e -> print_endline ("Error while reading file: " ^ e)
	| Exit code -> Printf.printf "Error while evaluating: program requested exit with status %d\n" code

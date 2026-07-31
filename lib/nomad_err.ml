type error =
  | ParseError of string
  | TokenizerError of string
  | EvaluationError of string

let print_err = function
  | ParseError e -> print_endline ("Error while parsing: " ^ e)
  | TokenizerError e -> print_endline ("Error while tokenizing: " ^ e)
  | EvaluationError e -> print_endline ("Error while evaluating: " ^ e)

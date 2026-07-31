type error =
  | ParseError of string
  | TokenizerError of string
  | EvaluationError of string

let print_err e =
  match e with
  | ParseError e -> print_endline ("Error while parsing: " ^ e)
  | TokenizerError e -> print_endline ("Error while tokenizing: " ^ e)
  | EvaluationError e -> prerr_endline ("Error while evaluating: " ^ e)

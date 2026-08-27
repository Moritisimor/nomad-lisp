open Nomad_err
open Printf

type token =
  | LPAREN
  | RPAREN
  | NUMLIT of float
  | BOOLLIT of bool
  | STRINGLIT of string
  | UNITLIT
  | SYMBOL of string
  | EOF

let string_of_token = function
  | LPAREN -> "LPAREN"
  | RPAREN -> "RPAREN"
  | NUMLIT x when Float.is_nan x -> "NUMLIT(nan)"
  | NUMLIT x -> sprintf "NUMLIT(%.2f)" x
  | BOOLLIT x -> sprintf "BOOLLIT(%b)" x
  | STRINGLIT x -> sprintf "STRINGLIT(\"%s\")" x
  | UNITLIT -> "UNITLITERAL"
  | SYMBOL x -> sprintf "SYMBOL('%s')" x
  | EOF -> "EOF"

let is_delim = function
  | '(' | ')' | ' ' | '\t' | '\n' | '\r' -> true
  | _ -> false

let keyword_at source i word =
  let n = String.length word in
  let finish = i + n in
  finish <= String.length source
  && String.sub source i n = word
  && (finish = String.length source || is_delim source.[finish])

let scan_until_delim source i =
  let j = ref i in
  while !j < String.length source && not (is_delim source.[!j]) do
    incr j
  done;
  !j

let tokenize source =
  let len = String.length source in
  let tokens = ref [LPAREN] in
  let left = ref 1 in
  let right = ref 0 in
  let i = ref 0 in
  let error = ref None in
  let add token = tokens := token :: !tokens in
  while !i < len && Option.is_none !error do
    match source.[!i] with
    | ' ' | '\t' | '\n' | '\r' -> incr i
    | '#' ->
        while !i < len && source.[!i] <> '\n' do incr i done
    | '(' ->
        add LPAREN;
        incr left;
        incr i
    | ')' ->
        add RPAREN;
        incr right;
        incr i
    | '"' ->
        let b = Buffer.create 16 in
        incr i;
        let closed = ref false in
        while !i < len && not !closed do
          match source.[!i] with
          | '"' ->
              closed := true;
              incr i
          | '\\' when !i + 1 < len ->
              begin match source.[!i + 1] with
              | 'n' -> Buffer.add_char b '\n'
              | 't' -> Buffer.add_char b '\t'
              | 'r' -> Buffer.add_char b '\r'
              | 'b' -> Buffer.add_char b '\b'
              | '"' -> Buffer.add_char b '"'
              | c -> Buffer.add_char b '\\'; Buffer.add_char b c
              end;
              i := !i + 2
          | '\\' ->
              Buffer.add_char b '\\';
              incr i
          | c ->
              Buffer.add_char b c;
              incr i
        done;
        if !closed then add (STRINGLIT (Buffer.contents b))
        else error := Some (TokenizerError (sprintf "String literal was never ended (Got \"%s)" (Buffer.contents b)))
    | c when (c >= '0' && c <= '9')
          || (c = '-' && !i + 1 < len
              && source.[!i + 1] >= '0' && source.[!i + 1] <= '9') ->
        let finish = scan_until_delim source !i in
        let text = String.sub source !i (finish - !i) in
        begin match float_of_string_opt text with
        | Some value -> add (NUMLIT value)
        | None -> error := Some (TokenizerError (sprintf "Could not parse %s to a number" text))
        end;
        i := finish
    | _ when keyword_at source !i "true" ->
        add (BOOLLIT true);
        i := !i + 4
    | _ when keyword_at source !i "false" ->
        add (BOOLLIT false);
        i := !i + 5
    | _ when keyword_at source !i "unit" ->
        add UNITLIT;
        i := !i + 4
    | _ ->
        let finish = scan_until_delim source !i in
        add (SYMBOL (String.sub source !i (finish - !i)));
        i := finish
  done;
  match !error with
  | Some e -> Error e
  | None ->
      add RPAREN;
      incr right;
      let result = List.rev (EOF :: !tokens) in
      if !left = !right then Ok result
      else if !left > !right then
        Error (TokenizerError "Unbalanced parantheses: one or more unclosed left parantheses")
      else
        Error (TokenizerError "Unbalanced parantheses: one or more superfluous right parantheses")

open Nomad_lisp
open Runtime_value
open Nomad_err

let fail source message = failwith (Printf.sprintf "%s: %s" source message)

let run source =
  match Interpreter.create () with
  | Error _ -> fail source "interpreter initialization failed"
  | Ok env ->
      match Interpreter.do_string source env with
      | Ok value -> value
      | Error error ->
          Nomad_err.print_err error;
          fail source "evaluation failed"

let expect source expected =
  let actual = run source |> string_of_rval in
  if actual <> expected then
    fail source (Printf.sprintf "expected %S, got %S" expected actual)

let expect_eval_error source =
  match Interpreter.create () with
  | Error _ -> fail source "interpreter initialization failed"
  | Ok env ->
      match Interpreter.do_string source env with
      | Error (EvaluationError _) -> ()
      | Error _ -> fail source "wrong error category"
      | Ok value -> fail source ("unexpectedly returned " ^ string_of_rval value)

let () =
  expect "(+ (* 10 5) (- 1000 250))" "800";
  expect "(let truest 55) truest" "55";
  expect "(let x 10) (let y 20) (+ x y)" "30";
  expect "(let add (lambda (x) (lambda (y) (+ x y)))) (let add10 (add 10)) (add10 20)" "30";
  expect "(letfun fact (n) (switch n (0 1) (_ (* n (fact (dec n)))))) (fact 10)" "3628800";
  expect "(do 1 2 3)" "3";
  expect "(try (throw \"boom\") \"caught\")" "caught";
  expect "(+ 0x1A 1_000)" "1026";
  expect "(= unit unit)" "true";
  expect "(chars \"héllo\")" "(h é l l o)";
  expect "(strlen \"héllo\")" "5";
  expect "(strlen \"a😀b\")" "3";
  expect "(list_init 200000 (lambda (i) i)) (nth (list_init 200000 (lambda (i) i)) 199999)" "199999";
  expect_eval_error "(throw \"first\") (+ 1 2)";
  expect_eval_error "(switch 1 (missing 2) (_ 3))";
  expect_eval_error "(switch 1)";
  begin match Interpreter.create () with
  | Error _ -> fail "exit" "interpreter initialization failed"
  | Ok env ->
      match Interpreter.do_string "(try (exit 7) 0)" env with
      | Error (Exit 7) -> ()
      | _ -> fail "exit" "exit was swallowed"
  end

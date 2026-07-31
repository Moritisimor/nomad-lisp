open Expr
open Printf

type runtime_value =
  | RLambda of string list * expr * (string, runtime_value) Hashtbl.t
  | RNum of float
  | RString of string
  | RBool of bool
  | RList of runtime_value list
  | RUnit 
  | RErr of string

let rec string_of_rval = function
  | RLambda _ -> "<FUNCTION>"
  | RNum x -> (
    if mod_float x 1. = 0. 
      then sprintf "%d" (int_of_float x)
      else sprintf "%.2f" x
  )
  | RString x -> x
  | RBool x -> sprintf "%b" x
  | RList x -> (
    let rec aux acc left =
      match left with
      | [] -> acc ^ ")"
      | [x] -> acc ^ (string_of_rval x) ^ ")"
      | x :: xs -> aux (acc ^ string_of_rval x ^ " ") xs
    in aux "(" x
  )

  | RUnit -> "<UNIT>"
  | RErr x -> Printf.sprintf "ERROR (%s)" x

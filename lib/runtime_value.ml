open Expr

type runtime_value =
  | RFun of string list * expr list
  | RNum of float
  | RString of string
  | RBool of bool
  | RList of runtime_value list
  | RUnit 
  | RErr of string

let rec string_of_rval = function
  | RFun _ -> "<FUNCTION>"
  | RNum x -> (
    if mod_float x 1. = 0. 
      then Printf.sprintf "%d" (int_of_float x)
      else Printf.sprintf "%.2f" x
  )
  | RString x -> x
  | RBool x -> Printf.sprintf "%b" x
  | RList x -> (
    let rec aux acc left =
      match left with
      | [] -> acc ^ ")"
      | [x] -> (
        acc ^ (string_of_rval x) ^ ")"
      )
      | x :: xs -> (
        aux (acc ^ string_of_rval x ^ " ") xs
      )
    in aux "(" x
  )

  | RUnit -> "<UNIT>"
  | RErr x -> Printf.sprintf "ERROR (%s)" x

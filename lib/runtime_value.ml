open Expr
open Printf

type runtime_value =
  | RLambda of string list * expr * env
  | RNum of float
  | RString of string
  | RBool of bool
  | RList of runtime_value list
  | RUnit 
  | RErr of string

and env = {
  bindings : (string, runtime_value) Hashtbl.t;
  parent : env option
}

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


let new_env parent = {
  bindings = Hashtbl.create 0;
  parent = parent
}

let rec get_binding key environ =
  match Hashtbl.find_opt environ.bindings key with
  | Some v -> v
  | None -> (
    match environ.parent with
    | Some e -> get_binding key e
    | None -> RErr ("No such variable: " ^ key)
  )

let rec set_binding key value environ = Hashtbl.add environ.bindings key value

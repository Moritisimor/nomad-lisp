open Expr
open Printf
open Nomad_err

type runtime_value =
  | RNativeFun of (expr list -> env -> (runtime_value, error) result)
  | RMacro of string list * expr list
  | RLambda of string list * expr * env
  | RNum of float
  | RString of string
  | RBool of bool
  | RList of runtime_value list
  | RRecord of (string, runtime_value) Hashtbl.t
  | RUnit 

and env = {
  bindings : (string, runtime_value) Hashtbl.t;
  parent : env option
}

let rec string_of_rval = function
  | RNativeFun _ -> "<NATIVEFUNCTION>"
  | RLambda _ -> "<FUNCTION>"
  | RMacro _ -> "<MACRO>"
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

  | RRecord _ -> "<RECORD>"
  | RUnit -> "<UNIT>"

let new_env parent = {
  bindings = Hashtbl.create 0;
  parent = parent
}

let rec get_binding key environ =
  match Hashtbl.find_opt environ.bindings key with
  | Some v -> Ok v
  | None -> (
    match environ.parent with
    | Some e -> get_binding key e
    | None -> Error (EvaluationError ("No such variable: " ^ key))
  )

let set_binding key value environ =
  match Hashtbl.find_opt environ.bindings key with
  | None -> Ok (Hashtbl.add environ.bindings key value)
  | Some _ -> Error (EvaluationError (sprintf "Cannot bind %s: Already exists in this scope" key))

let rec mutate_binding key value environ =
  match Hashtbl.find_opt environ.bindings key with
  | Some _ -> Ok (Hashtbl.replace environ.bindings key value)
  | None -> (
    match environ.parent with
    | Some e -> mutate_binding key value e
    | None -> Error (EvaluationError (sprintf "Cannot mutate non-existant binding: %s" key))
  )

let register_native env name func =
  set_binding name (RNativeFun func) env

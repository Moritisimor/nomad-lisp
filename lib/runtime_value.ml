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

let format_number x =
  match Float.classify_float x with
  | FP_nan -> "nan"
  | FP_infinite -> if x > 0. then "inf" else "-inf"
  | _ when Float.is_integer x && Float.abs x < 9.223372036854776e18 ->
      Int64.to_string (Int64.of_float x)
  | _ -> sprintf "%.2f" x

let rec string_of_rval = function
  | RNativeFun _ -> "<NATIVEFUNCTION>"
  | RLambda _ -> "<FUNCTION>"
  | RMacro _ -> "<MACRO>"
  | RNum x -> format_number x
  | RString x -> x
  | RBool x -> string_of_bool x
  | RList values ->
      let b = Buffer.create 32 in
      Buffer.add_char b '(';
      let rec add = function
        | [] -> ()
        | [x] -> Buffer.add_string b (string_of_rval x)
        | x :: xs ->
            Buffer.add_string b (string_of_rval x);
            Buffer.add_char b ' ';
            add xs
      in
      add values;
      Buffer.add_char b ')';
      Buffer.contents b
  | RRecord _ -> "<RECORD>"
  | RUnit -> "<UNIT>"

let new_env ?(capacity = 4) parent = {
  bindings = Hashtbl.create capacity;
  parent
}

let get_binding key environ =
  let rec loop current =
    match Hashtbl.find_opt current.bindings key with
    | Some v -> Ok v
    | None ->
        match current.parent with
        | Some parent -> loop parent
        | None -> Error (EvaluationError ("No such variable: " ^ key))
  in
  loop environ

let set_binding key value environ =
  if Hashtbl.mem environ.bindings key then
    Error (EvaluationError (sprintf "Cannot bind %s: Already exists in this scope" key))
  else begin
    Hashtbl.add environ.bindings key value;
    Ok ()
  end

let mutate_binding key value environ =
  let rec loop current =
    if Hashtbl.mem current.bindings key then begin
      Hashtbl.replace current.bindings key value;
      Ok ()
    end else
      match current.parent with
      | Some parent -> loop parent
      | None -> Error (EvaluationError (sprintf "Cannot mutate non-existant binding: %s" key))
  in
  loop environ

let register_native env name func = set_binding name (RNativeFun func) env

let rec equal a b =
  match a, b with
  | RNum x, RNum y -> x = y
  | RString x, RString y -> String.equal x y
  | RBool x, RBool y -> Bool.equal x y
  | RUnit, RUnit -> true
  | RList x, RList y ->
      let rec lists_equal left right =
        match left, right with
        | [], [] -> true
        | x :: xs, y :: ys -> equal x y && lists_equal xs ys
        | _ -> false
      in
      lists_equal x y
  | RRecord x, RRecord y -> x == y
  | RLambda (xp, xb, xe), RLambda (yp, yb, ye) -> xp = yp && xb == yb && xe == ye
  | RMacro (xp, xb), RMacro (yp, yb) -> xp = yp && xb == yb
  | RNativeFun x, RNativeFun y -> x == y
  | _ -> false

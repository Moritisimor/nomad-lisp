open Runtime_value

type env = {
  bindings : (string, runtime_value) Hashtbl.t
}

let new_env () =
  let hashtbl = Hashtbl.create 0 in
  Hashtbl.add hashtbl "PI" (RNum 3.1415926535);
  Hashtbl.add hashtbl "EULER" (RNum 2.7182818284);
  { bindings = hashtbl }

let copy_env existing_env = { bindings = Hashtbl.copy existing_env.bindings }

let get_binding environment binding_name = 
  let binding = Hashtbl.find_opt environment.bindings binding_name in
  match binding with
  | Some x -> x
  | None -> RUnit

let set_binding environment binding_name binding_value =
  Hashtbl.add environment.bindings binding_name binding_value

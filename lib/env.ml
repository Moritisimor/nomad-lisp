type env = {
  bindings : (string, Runtime_value.runtime_value) Hashtbl.t
}

let new_env () = { bindings = Hashtbl.create 0 }
let new_env_with_bindings existing_bindings = { bindings = existing_bindings }

let get_binding environment binding_name = 
  let binding = Hashtbl.find_opt environment.bindings binding_name in
  match binding with
  | Some x -> x
  | None -> RUnit

let set_binding environment binding_name binding_value =
  Hashtbl.add environment.bindings binding_name binding_value

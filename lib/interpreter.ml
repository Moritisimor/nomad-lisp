open Runtime_value
open Eval

(* Just a small convenience function for creating an environment and binding the stdlib as well. *)
let new_interpreter = 
  let e = new_env None in
  register_std_natives e;
  load_stdlib e;
  e

let full = ref false
let dx = ref 0.1

let set_dx value =
  if value < 0.0 then raise (Arg.Bad "dx must be greater than zero")
  else dx := value

let speclist =
  [
    ("-full", Arg.Set full, "Do not use windowed mode, only finish after input");
    ("-dx", Arg.Float set_dx, "dx for interpolations");
  ]

let usage_msg = "interpolate [--full] -dx 0.2"
let anon_fun _f = ()

let () =
  Arg.parse speclist anon_fun usage_msg;
  let config = Console.{ dx = !dx; is_full = !full } in
  let final_state = Console.infinite_loop config [] in
  if !full then print_endline "Computing interpolation on all points";
  Console.interpolate_and_print { config with is_full = true } final_state

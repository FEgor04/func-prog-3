let full = ref false
let dx = ref 0.1
let max_window_size = ref 10

let set_dx value =
  if value < 0.0 then raise (Arg.Bad "dx must be greater than zero")
  else dx := value

let set_max_window_size = function
  | value when value < 4 ->
      raise (Arg.Bad "max_window_size must be greater or equal to 4")
  | value when value > 1000 ->
      raise (Arg.Bad "max_window_size must be less than 1000")
  | value -> max_window_size := value

let speclist =
  [
    ("-full", Arg.Set full, "Do not use windowed mode, only finish after input");
    ("-dx", Arg.Float set_dx, "dx for interpolations");
    ("-max", Arg.Int set_max_window_size, "max window size");
  ]

let usage_msg = "interpolate [--full] -dx 0.2"
let anon_fun _f = ()

let () =
  Arg.parse speclist anon_fun usage_msg;
  let initial_state =
    Console.
      {
        dx = !dx;
        is_full = !full;
        max_window_size = !max_window_size;
        points = [];
        methods_starts = InterpolationMethodMap.empty;
      }
  in
  let points = Console.infinite_loop initial_state in
  print_endline "Computing interpolation on all points";
  ignore
  @@ Console.interpolate_and_print
       Console.
         {
           dx = initial_state.dx;
           points;
           is_full = true;
           max_window_size = !max_window_size;
           methods_starts = InterpolationMethodMap.empty;
         }

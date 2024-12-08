let methods =
  [
    Interpolation.Linear.LinearMethod.(interpolate, window_size, name);
    Interpolation.Lagrange.LagrangeMethod.(interpolate, window_size, name);
  ]

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

type read_line_result = Line of string | Eof

let read_line_opt () = try Line (read_line ()) with End_of_file -> Eof

let point_from_line line =
  let lst =
    line |> String.trim |> String.split_on_char ',' |> List.map float_of_string
  in
  let[@warning "-partial-match"] (x :: y :: _) = lst in
  (x, y)

let read_point () =
  let result = read_line_opt () in
  match result with Eof -> None | Line line -> Some (point_from_line line)

let get_window n lst =
  lst |> List.rev |> List.filteri (fun i _ -> i < n) |> List.rev

open Interpolation.Common

let interpolate_and_print { dx } state full =
  let n = List.length state in
  List.iter
    (fun (interpolate, window_size, name) ->
      if n < window_size then ()
      else
        let window = if full then state else get_window window_size state in
        let result = interpolate { dx } window in
        print_endline (show_interpolationMethod name);
        List.iter (fun (x, y) -> show_point (x, y) |> print_endline) result)
    methods

let rec infinite_loop { dx } state =
  if not !full then interpolate_and_print { dx } state false else ();
  let res = read_point () in
  match res with
  | Some (x, y) -> infinite_loop { dx } (state @ [ (x, y) ])
  | None -> state

let () =
  Arg.parse speclist anon_fun usage_msg;
  let final_state = infinite_loop { dx = !dx } [] in
  if !full then print_endline "Computing interpolation on all points";
  interpolate_and_print { dx = !dx } final_state true

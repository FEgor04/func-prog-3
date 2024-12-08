let methods =
  [
    Interpolation.Linear.LinearMethod.(interpolate, window_size);
    Interpolation.Lagrange.LagrangeMethod.(interpolate, window_size);
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

let usage_msg = "interpolate [--full] -dx 0.2"
let anon_fun _f = ()

let get_window n lst =
  lst |> List.rev |> List.filteri (fun i _ -> i < n) |> List.rev

open Interpolation.Common

let rec infinite_loop { dx } state =
  let n = List.length state in
  List.iter
    (fun (interpolate, window_size) ->
      if n < window_size then ()
      else
        let window = get_window window_size state in
        let result = interpolate { dx } window in
        List.iter (fun (x, y) -> show_point (x, y) |> print_endline) result)
    methods;
  let res = read_point () in
  match res with
  | Some (x, y) -> infinite_loop { dx } (state @ [ (x, y) ])
  | None -> state

let () =
  Arg.parse speclist anon_fun usage_msg;
  let _ = infinite_loop { dx = !dx } [] in
  print_string "done!"

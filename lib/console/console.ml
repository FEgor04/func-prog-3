open Interpolation.Common

let methods =
  [
    Interpolation.Linear.LinearMethod.(interpolate, window_size, name);
    Interpolation.Lagrange.LagrangeMethod.(interpolate, window_size, name);
  ]

let get_window window_size lst =
  let n = List.length lst in
  lst |> List.filteri (fun i _ -> n - i - 1 < window_size)

let%test _ = get_window 1 [ 1; 2; 3 ] = [ 3 ]
let%test _ = get_window 2 [ 1; 2; 3 ] = [ 2; 3 ]
let%test _ = get_window 3 [ 1; 2; 3 ] = [ 1; 2; 3 ]

let interpolate_and_print_method { dx } points (interpolate, window_size, name)
    =
  let points = get_window window_size points in
  let n = List.length points in
  if n < window_size then ()
  else
    let result = interpolate { dx } points in
    print_endline (show_interpolationMethod name);
    List.iter (fun (x, y) -> show_point (x, y) |> print_endline) result

let interpolate_and_print Interpolation.Common.{ dx } points =
  methods |> List.iter (interpolate_and_print_method { dx } points)

type programConfig = { dx : float; is_full : bool; max_window_size : int }

let rec infinite_loop { dx; is_full; max_window_size } points =
  if not is_full then interpolate_and_print { dx } points else ();
  let points = get_window max_window_size points in
  let res = Reader.read_point () in
  match res with
  | Some (x, y) ->
      infinite_loop { dx; is_full; max_window_size } (points @ [ (x, y) ])
  | None -> points

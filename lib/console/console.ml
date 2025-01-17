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

let interpolate_and_print_method { dx } points is_full
    (interpolate, window_size, name) =
  let is_first =
    List.length points < window_size || List.length points = window_size
  in
  let x_geq =
    if (not is_first) && not is_full then List.nth_opt (List.rev points) 1
    else None
  in
  let points = if not is_full then get_window window_size points else points in
  let n = List.length points in
  if n < window_size then ()
  else
    let greater_than_option (x, _) =
      match x_geq with None -> true | Some (x_geq, _) -> x >= x_geq
    in
    let result = interpolate { dx } points in
    print_endline (show_interpolationMethod name);
    result |> List.filter greater_than_option |> List.iter print_point

let interpolate_and_print Interpolation.Common.{ dx } points is_full =
  methods |> List.iter (interpolate_and_print_method { dx } points is_full)

type state = {
  dx : float;
  is_full : bool;
  max_window_size : int;
  points : point list;
}

let rec infinite_loop { dx; is_full; max_window_size; points } =
  if not is_full then (
    interpolate_and_print { dx } points false;
    print_newline ());
  let points = get_window max_window_size points in
  let res = Reader.read_point () in
  match res with
  | Some (x, y) ->
      infinite_loop
        { dx; is_full; max_window_size; points = points @ [ (x, y) ] }
  | None -> points

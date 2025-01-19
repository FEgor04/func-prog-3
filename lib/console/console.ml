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

module InterpolationMethodCompare = struct
  type t = interpolationMethod

  let compare a b = compare_interpolationMethod a b
end

module InterpolationMethodMap = Map.Make (InterpolationMethodCompare)

type state = {
  dx : float;
  is_full : bool;
  max_window_size : int;
  points : point list;
  methods_starts : float InterpolationMethodMap.t;
}

let interpolate_and_print_method { dx; points; methods_starts; _ }
    (interpolate, window_size, name) =
  let n = List.length points in
  if n < window_size then None
  else
    let start_x = InterpolationMethodMap.find_opt name methods_starts in
    let result = interpolate ?start_x { dx } points in
    print_endline (show_interpolationMethod name);
    let last_x, _ = List.hd @@ List.rev result in
    result |> List.iter print_point;
    Some last_x

let interpolate_and_print state =
  let starts = methods |> List.map (interpolate_and_print_method state) in
  let method_names = List.map (fun (_, _, name) -> name) methods in
  let starts_with_methods = List.combine starts method_names in
  let updated_map =
    starts_with_methods
    |> List.fold_left
         (fun map (start, method_name) ->
           match start with
           | None -> map
           | Some x ->
               InterpolationMethodMap.add method_name (x +. state.dx) map)
         state.methods_starts
  in
  { state with methods_starts = updated_map }

let get_start_x dx = function Some last_x -> Some (last_x +. dx) | _ -> None

let rec infinite_loop state =
  let state =
    if not state.is_full then interpolate_and_print state else state
  in
  let points = get_window state.max_window_size state.points in
  let res = Reader.read_point () in
  match res with
  | Some (x, y) -> infinite_loop { state with points = points @ [ (x, y) ] }
  | None -> points

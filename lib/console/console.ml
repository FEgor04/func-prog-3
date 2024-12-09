open Interpolation.Common

let methods =
  [
    Interpolation.Linear.LinearMethod.(interpolate, window_size, name);
    Interpolation.Lagrange.LagrangeMethod.(interpolate, window_size, name);
  ]

type input = { dx : float; is_full : bool }

let get_window n lst =
  lst |> List.rev |> List.filteri (fun i _ -> i < n) |> List.rev

let interpolate_and_print { dx; is_full } state =
  let n = List.length state in
  List.iter
    (fun (interpolate, window_size, name) ->
      if n < window_size then ()
      else
        let window = if is_full then state else get_window window_size state in
        let result = interpolate { dx } window in
        print_endline (show_interpolationMethod name);
        List.iter (fun (x, y) -> show_point (x, y) |> print_endline) result)
    methods

let rec infinite_loop { dx; is_full } state =
  if not is_full then interpolate_and_print { dx; is_full } state else ();
  let res = Reader.read_point () in
  match res with
  | Some (x, y) -> infinite_loop { dx; is_full } (state @ [ (x, y) ])
  | None -> state

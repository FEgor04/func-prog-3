let count = 1000

open Interpolation.Common

let linear = Interpolation.Linear.LinearMethod.(interpolate, window_size)
let lagrange = Interpolation.Lagrange.LagrangeMethod.(interpolate, window_size)
let float_equals a b = abs_float (a -. b) < 1e-9

let interplation_equals_in_points (interpolate, min_length) =
  QCheck.Test.make ~count ~name:"y'(x_0) = y(x_0)"
    QCheck.(
      tup3 (float_range 0.1 10.0)
        (float_range (-100.0) 100.0)
        (small_list (float_range (-20.0) 20.0)))
    (fun (dx, x_start, ys) ->
      let n = List.length ys in
      QCheck.assume (n >= min_length);
      let xs = List.init n (fun i -> x_start +. (float_of_int i *. dx)) in
      let points = List.combine xs ys in
      let interpolation = interpolate ?start_x:None { dx } points in
      let interpolation_xs, _interpolation_ys = List.split interpolation in
      List.equal float_equals interpolation_xs xs)

let () =
  let linear_suite =
    List.map QCheck_alcotest.to_alcotest
      [ interplation_equals_in_points linear ]
  in
  let lagrange_suite =
    List.map QCheck_alcotest.to_alcotest
      [ interplation_equals_in_points lagrange ]
  in
  Alcotest.run "quickcheck"
    [ ("linear", linear_suite); ("lagrange", lagrange_suite) ]

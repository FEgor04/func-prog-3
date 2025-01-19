open Common

let lagrange_interpolation_compute points x =
  let xs = points |> List.map (fun (x, _) -> x) in
  let ys = points |> List.map (fun (_, y) -> y) in
  let n = List.length points in
  let polynoms =
    List.init n (fun i ->
        let prod_elems =
          List.init n (fun j ->
              if i == j then 1.0
              else (x -. List.nth xs j) /. (List.nth xs i -. List.nth xs j))
        in
        let prod = List.fold_left ( *. ) 1.0 prod_elems in
        prod)
  in
  polynoms
  |> List.mapi (fun i polynom -> polynom *. List.nth ys i)
  |> List.fold_left ( +. ) 0.0

let lagrange_interpolation ?start_x { dx } points =
  let xs = points |> List.map (fun (x, _) -> x) in
  let coverage_xs = get_x_coverage ?start_x { dx } xs in
  let interpolate = lagrange_interpolation_compute points in
  let ys = coverage_xs |> List.map interpolate in
  List.combine coverage_xs ys

module LagrangeMethod : Common.InterpolationMethod = struct
  let interpolate = lagrange_interpolation
  let window_size = 4
  let name = Lagrange
end

let%expect_test "parabola y = x^2" =
  let dx = 1.0 in
  let xs = [ 0.0; 1.0; 2.0; 3.0; 4.0 ] in
  let ys = xs |> List.map (fun x -> x *. x) in
  let input = List.combine xs ys in
  let interpolated = lagrange_interpolation { dx } input in
  interpolated |> List.iter (fun (x, y) -> Printf.printf " %f: %f \n" x y);
  [%expect
    {|
    0.000000: 0.000000
    1.000000: 1.000000
    2.000000: 4.000000
    3.000000: 9.000000
    4.000000: 16.000000
    |}]

let%expect_test "y = sin x" =
  let dx = 1.0 in
  let xs = [ 0.0; 1.0; 2.0; 3.0; 4.0 ] in
  let ys = xs |> List.map sin in
  let input = List.combine xs ys in
  let interpolated = lagrange_interpolation { dx } input in
  interpolated |> List.iter (fun (x, y) -> Printf.printf " %f: %f \n" x y);
  [%expect
    {|
    0.000000: 0.000000
    1.000000: 0.841471
    2.000000: 0.909297
    3.000000: 0.141120
    4.000000: -0.756802
    |}]

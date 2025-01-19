open Common

let linear_interpolation { dx } points =
  let xs = points |> List.map (fun (x, _) -> x) in
  let n = List.length xs in
  let first_x, first_y = List.hd points in
  let second_x, second_y = List.nth points 1 in
  let pre_x, pre_y = List.nth points (n - 2) in
  let last_x, last_y = points |> List.rev |> List.hd in
  let answer_x = get_x_coverage { dx } xs in
  let answer_y =
    answer_x
    |> List.map (fun target_x ->
           match target_x with
           | x when x <= second_x ->
               let k = (second_y -. first_y) /. (second_x -. first_x) in
               first_y +. ((x -. first_x) *. k)
           | x when x >= pre_x ->
               let k = (last_y -. pre_y) /. (last_x -. pre_x) in
               last_y +. ((x -. last_x) *. k)
           | x ->
               let next_idx = xs |> List.find_index (( <= ) x) |> Option.get in
               let previous_x, previous_y = List.nth points (next_idx - 1) in
               let next_x, next_y = List.nth points next_idx in
               let k = (next_y -. previous_y) /. (next_x -. previous_x) in
               previous_y +. ((x -. previous_x) *. k))
  in
  List.combine answer_x answer_y

module LinearMethod : Common.InterpolationMethod = struct
  let interpolate = linear_interpolation
  let window_size = 2
  let name = Linear
end

let%expect_test "line x = y" =
  let dx = 0.5 in
  let xs = [ 0.0; 1.0 ] in
  let ys = [ 0.0; 1.0 ] in
  let input = List.combine xs ys in
  let interpolated = linear_interpolation { dx } input in
  interpolated |> List.iter (fun (x, y) -> Printf.printf " %f: %f \n" x y);
  [%expect
    {|
    0.000000: 0.000000
    0.500000: 0.500000
    1.000000: 1.000000
    |}]

let%expect_test "line y = 2 * x for dx = 0.3" =
  let dx = 0.3 in
  let xs = [ 0.0; 1.0 ] in
  let ys = [ 0.0; 2.0 ] in
  let input = List.combine xs ys in
  let interpolated = linear_interpolation { dx } input in
  interpolated |> List.iter (fun (x, y) -> Printf.printf " %f: %f \n" x y);
  [%expect
    {|
    0.000000: 0.000000
    0.300000: 0.600000
    0.600000: 1.200000
    0.900000: 1.800000
    |}]

let%expect_test "line for y = 1/2x dx = 0.25" =
  let dx = 0.25 in
  let xs = [ 0.0; 1.0 ] in
  let ys = [ 0.0; 0.5 ] in
  let input = List.combine xs ys in
  let interpolated = linear_interpolation { dx } input in
  interpolated |> List.iter (fun (x, y) -> Printf.printf " %f: %f \n" x y);
  [%expect
    {|
    0.000000: 0.000000
    0.250000: 0.125000
    0.500000: 0.250000
    0.750000: 0.375000
    1.000000: 0.500000
    |}]

let%expect_test "parabola y = x^2" =
  let dx = 0.25 in
  let xs = [ 0.0; 3.0 ] in
  let ys = [ 0.0; 9.0 ] in
  let input = List.combine xs ys in
  let interpolated = linear_interpolation { dx } input in
  interpolated |> List.iter (fun (x, y) -> Printf.printf " %f: %f \n" x y);
  [%expect
    {|
    0.000000: 0.000000
    0.250000: 0.750000
    0.500000: 1.500000
    0.750000: 2.250000
    1.000000: 3.000000
    1.250000: 3.750000
    1.500000: 4.500000
    1.750000: 5.250000
    2.000000: 6.000000
    2.250000: 6.750000
    2.500000: 7.500000
    2.750000: 8.250000
    3.000000: 9.000000
    |}]

let%expect_test "spline with multiple points" =
  let dx = 1.0 in
  let xs = [ 0.0; 1.0; 2.0; 3.0; 4.0 ] in
  let ys = [ 0.0; 1.0; 0.0; 1.0; 0.0 ] in
  let input = List.combine xs ys in
  let interpolated = linear_interpolation { dx } input in
  interpolated |> List.iter (fun (x, y) -> Printf.printf " %f: %f \n" x y);
  [%expect
    {|
    0.000000: 0.000000
    1.000000: 1.000000
    2.000000: 0.000000
    3.000000: 1.000000
    4.000000: 0.000000
    |}]

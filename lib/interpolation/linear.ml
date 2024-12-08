open Common

let linear_interpolation { dx } points =
  let x1, y1 = List.hd points in
  let x2, y2 = List.nth points 1 in
  let k = (y2 -. y1) /. (x2 -. x1) in
  let xs = points |> List.map (fun (x, _) -> x) in
  let answer_x = get_x_coverage { dx } xs in
  let n = List.length answer_x in
  let answer_y = List.init n (fun i -> y1 +. (float_of_int i *. k *. dx)) in
  List.combine answer_x answer_y

module LinearMethod : Common.InterpolationMethod = struct
  let interpolate = linear_interpolation
  let window_size = 2
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
    1.200000: 2.400000
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

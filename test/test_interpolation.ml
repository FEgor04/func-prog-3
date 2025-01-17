open Interpolation.Common

(* [build_windows points] returns testcases to properly show
      function behaviour for different windows *)
let build_windows points =
  let rec aux points acc =
    match points with
    | [] -> acc
    | [ _ ] -> acc
    | hd :: arr -> aux arr (acc @ [ List.rev (hd :: arr) ])
  in
  List.rev @@ aux (List.rev points) []

let print_windows windows =
  print_endline " === WINDOWS === ";
  List.iter
    (fun points ->
      List.iter print_point points;
      print_newline ())
    windows

let interpolate_test_case dx points =
  Console.interpolate_and_print { dx } points false

let print_interpolation_result dx windows =
  print_endline " === INTERPOLATION === ";
  List.iter (interpolate_test_case dx) windows

let test ?(dx = 1.0) points =
  let windows = build_windows points in
  print_windows windows;
  print_interpolation_result dx windows

let%expect_test "dx is greater than points" =
  let points = [ (1.0, 1.0); (2.0, 2.0); (4.0, 4.0); (5.0, 2.0) ] in
  test ~dx:3.0 points;
  [%expect
    {|
     === WINDOWS ===
    (1.0000, 1.0000)
    (2.0000, 2.0000)

    (1.0000, 1.0000)
    (2.0000, 2.0000)
    (4.0000, 4.0000)

    (1.0000, 1.0000)
    (2.0000, 2.0000)
    (4.0000, 4.0000)
    (5.0000, 2.0000)

     === INTERPOLATION ===
    Common.Linear
    (1.0000, 1.0000)
    (4.0000, 4.0000)
    Common.Linear
    (5.0000, 5.0000)
    Common.Linear
    (7.0000, -2.0000)
    Common.Lagrange
    (1.0000, 1.0000)
    (4.0000, 4.0000)
    (7.0000, -15.5000)
    |}]

let%expect_test "linear only" =
  let points = [ (1.0, 1.0); (2.0, 2.0) ] in
  test ~dx:0.5 points;
  [%expect
    {|
     === WINDOWS ===
    (1.0000, 1.0000)
    (2.0000, 2.0000)

     === INTERPOLATION ===
    Common.Linear
    (1.0000, 1.0000)
    (1.5000, 1.5000)
    (2.0000, 2.0000)
    |}]

let%expect_test "linear & lagrange" =
  let points = [ (1.0, 1.0); (2.0, 2.0); (3.0, 3.0); (4.0, 4.0) ] in
  test ~dx:0.5 points;
  [%expect
    {|
     === WINDOWS ===
    (1.0000, 1.0000)
    (2.0000, 2.0000)

    (1.0000, 1.0000)
    (2.0000, 2.0000)
    (3.0000, 3.0000)

    (1.0000, 1.0000)
    (2.0000, 2.0000)
    (3.0000, 3.0000)
    (4.0000, 4.0000)

     === INTERPOLATION ===
    Common.Linear
    (1.0000, 1.0000)
    (1.5000, 1.5000)
    (2.0000, 2.0000)
    Common.Linear
    (2.5000, 2.5000)
    (3.0000, 3.0000)
    Common.Linear
    (3.5000, 3.5000)
    (4.0000, 4.0000)
    Common.Lagrange
    (1.0000, 1.0000)
    (1.5000, 1.5000)
    (2.0000, 2.0000)
    (2.5000, 2.5000)
    (3.0000, 3.0000)
    (3.5000, 3.5000)
    (4.0000, 4.0000)
    |}]

let%expect_test "sin(x) test from lab" =
  let points =
    [ (0.0, 0.0); (1.571, 1.0); (3.142, 0.0); (4.712, -1.0); (12.568, 0.0) ]
  in
  test points;
  [%expect
    {|
     === WINDOWS ===
    (0.0000, 0.0000)
    (1.5710, 1.0000)

    (0.0000, 0.0000)
    (1.5710, 1.0000)
    (3.1420, 0.0000)

    (0.0000, 0.0000)
    (1.5710, 1.0000)
    (3.1420, 0.0000)
    (4.7120, -1.0000)

    (0.0000, 0.0000)
    (1.5710, 1.0000)
    (3.1420, 0.0000)
    (4.7120, -1.0000)
    (12.5680, 0.0000)

     === INTERPOLATION ===
    Common.Linear
    (0.0000, 0.0000)
    (1.0000, 0.6365)
    (2.0000, 1.2731)
    Common.Linear
    (2.5710, 0.3635)
    (3.5710, -0.2731)
    Common.Linear
    (4.1420, -0.6369)
    (5.1420, -1.2739)
    Common.Lagrange
    (0.0000, 0.0000)
    (1.0000, 0.9730)
    (2.0000, 0.8412)
    (3.0000, 0.1203)
    (4.0000, -0.6740)
    (5.0000, -1.0258)
    Common.Linear
    (5.7120, -0.8727)
    (6.7120, -0.7454)
    (7.7120, -0.6181)
    (8.7120, -0.4908)
    (9.7120, -0.3635)
    (10.7120, -0.2363)
    (11.7120, -0.1090)
    (12.7120, 0.0183)
    Common.Lagrange
    (5.5710, -1.4858)
    (6.5710, -1.9495)
    (7.5710, -2.2616)
    (8.5710, -2.3777)
    (9.5710, -2.2535)
    (10.5710, -1.8447)
    (11.5710, -1.1070)
    (12.5710, 0.0039)
    |}]

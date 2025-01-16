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
  Console.interpolate_and_print { dx } points true

let print_interpolation_result dx windows =
  print_endline " === INTERPOLATION === ";
  List.iter (interpolate_test_case dx) windows

let test ?(dx = 1.0) points =
  let windows = build_windows points in
  print_windows windows;
  print_interpolation_result dx windows

let%expect_test "linear only" =
  let points = [ (1.0, 1.0); (2.0, 2.0) ] in
  test ~dx:0.5 points;
  [%expect
    {|
     === WINDOWS ===
    (1., 1.)
    (2., 2.)

     === INTERPOLATION ===
    Common.Linear
    (1., 1.)
    (1.5, 1.5)
    (2., 2.)
    |}]

let%expect_test "linear & lagrange" =
  let points = [ (1.0, 1.0); (2.0, 2.0); (3.0, 3.0); (4.0, 4.0) ] in
  test ~dx:0.5 points;
  [%expect
    {|
     === WINDOWS ===
    (1., 1.)
    (2., 2.)

    (1., 1.)
    (2., 2.)
    (3., 3.)

    (1., 1.)
    (2., 2.)
    (3., 3.)
    (4., 4.)

     === INTERPOLATION ===
    Common.Linear
    (1., 1.)
    (1.5, 1.5)
    (2., 2.)
    Common.Linear
    (2., 2.)
    (2.5, 2.5)
    (3., 3.)
    Common.Linear
    (3., 3.)
    (3.5, 3.5)
    (4., 4.)
    Common.Lagrange
    (1., 1.)
    (1.5, 1.5)
    (2., 2.)
    (2.5, 2.5)
    (3., 3.)
    (3.5, 3.5)
    (4., 4.)
    |}]

let%expect_test "sin(x) test from lab" =
  let points =
    [ (0.0, 0.0); (1.571, 1.0); (3.142, 0.0); (4.712, -1.0); (12.568, 0.0) ]
  in
  test points;
  [%expect
    {|
     === WINDOWS ===
    (0., 0.)
    (1.571, 1.)

    (0., 0.)
    (1.571, 1.)
    (3.142, 0.)

    (0., 0.)
    (1.571, 1.)
    (3.142, 0.)
    (4.712, -1.)

    (0., 0.)
    (1.571, 1.)
    (3.142, 0.)
    (4.712, -1.)
    (12.568, 0.)

     === INTERPOLATION ===
    Common.Linear
    (0., 0.)
    (1., 0.636537237428)
    (2., 1.27307447486)
    Common.Linear
    (1.571, 1.)
    (2.571, 0.363462762572)
    (3.571, -0.273074474857)
    Common.Linear
    (3.142, 0.)
    (4.142, -0.636942675159)
    (5.142, -1.27388535032)
    Common.Lagrange
    (0., 0.)
    (1., 0.973032780308)
    (2., 0.841202161479)
    (3., 0.120277127525)
    (4., -0.673973337541)
    (5., -1.02578024971)
    Common.Linear
    (4.712, -1.)
    (5.712, -0.872708757637)
    (6.712, -0.745417515275)
    (7.712, -0.618126272912)
    (8.712, -0.49083503055)
    (9.712, -0.363543788187)
    (10.712, -0.236252545825)
    (11.712, -0.108961303462)
    (12.712, 0.0183299389002)
    Common.Lagrange
    (1.571, 1.)
    (2.571, 0.372563983334)
    (3.571, -0.280414395002)
    (4.571, -0.914628704004)
    (5.571, -1.48577251267)
    (6.571, -1.94953939)
    (7.571, -2.26162290498)
    (8.571, -2.37771662662)
    (9.571, -2.25351412391)
    (10.571, -1.84470896584)
    (11.571, -1.10699472143)
    (12.571, 0.00393504034297)
    |}]

type interpolationMethod = Linear | Lagrange [@@deriving show]
type point = float * float

let show_point (x, y) = Format.sprintf "(%.4f, %.4f)" x y
let print_point point = print_endline @@ show_point @@ point

type config = { dx : float }
type interpolate = config -> point list -> point list

module type InterpolationMethod = sig
  val name : interpolationMethod
  val interpolate : interpolate
  val window_size : int
end

let ( >. ) x y = x -. y > 1e-9

let get_x_coverage { dx } lst =
  let rec generate acc current last =
    if current >. last then List.rev acc
    else generate (current :: acc) (current +. dx) last
  in
  match lst with
  | [] -> []
  | hd :: _ ->
      let last = List.fold_left max hd lst in
      generate [] hd last

type floats = float list [@@deriving show]

let print_floats floats = print_endline @@ show_floats floats

let%expect_test "even" =
  let dx = 0.5 in
  let lst = [ 0.0; 1.0 ] in
  let xs = get_x_coverage { dx } lst in
  print_floats xs;
  [%expect {| [0.; 0.5; 1.] |}]

let%expect_test "not even" =
  let dx = 0.3 in
  let lst = [ 0.0; 0.3; 0.6 ] in
  let xs = get_x_coverage { dx } lst in
  print_floats xs;
  [%expect {| [0.; 0.3; 0.6] |}]

let%expect_test "not even" =
  let dx = 0.3 in
  let lst = [ 0.0; 0.3; 0.6; 0.9 ] in
  let xs = get_x_coverage { dx } lst in
  print_floats xs;
  [%expect {| [0.; 0.3; 0.6; 0.9] |}]

let%expect_test "not even" =
  let dx = 0.3 in
  let lst = [ 0.0; 0.3; 0.6; 0.9; 1.0 ] in
  let xs = get_x_coverage { dx } lst in
  print_floats xs;
  [%expect {| [0.; 0.3; 0.6; 0.9] |}]

let%expect_test "not" =
  let dx = 0.4 in
  let lst = [ 0.0; 0.3; 0.6; 0.9; 1.0 ] in
  let xs = get_x_coverage { dx } lst in
  print_floats xs;
  [%expect {| [0.; 0.4; 0.8] |}]

let%expect_test "big dx" =
  let dx = 5.46 in
  let lst = [ -0.64; 4.82; 10.28; 15.74; 21.2 ] in
  let xs = get_x_coverage { dx } lst in
  print_floats xs;
  [%expect {| [-0.64; 4.82; 10.28; 15.74; 21.2] |}]

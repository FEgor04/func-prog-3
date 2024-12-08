type interpolationMethod = Linear | Lagrange
type point = float * float
type config = { dx : float }
type interpolate = config -> point list -> point list

(** Overwrite less operator to fix comparisons *)
let ( <= ) a b = b -. a > 1e-9

let get_x_coverage { dx } points =
  let first_x = List.hd points in
  let last_x = points |> List.rev |> List.hd in
  let xs_coverage =
    Seq.unfold
      (fun x -> if x <= last_x then Some (x, x +. dx) else None)
      first_x
  in
  let coverage = List.of_seq xs_coverage in
  let last_coverage = coverage |> List.rev |> List.hd in
  if last_coverage < last_x then coverage @ [ last_coverage +. dx ]
  else coverage

let%expect_test "even" =
  let dx = 0.5 in
  let lst = [ 0.0; 1.0 ] in
  let xs = get_x_coverage { dx } lst in
  xs |> List.iter (Printf.printf " %f ");
  [%expect {| 0.000000  0.500000  1.000000 |}]

let%expect_test "not even" =
  let dx = 0.3 in
  let lst = [ 0.0; 0.3; 0.6 ] in
  let xs = get_x_coverage { dx } lst in
  xs |> List.iter (Printf.printf " %f ");
  [%expect {| 0.000000  0.300000  0.600000  |}]

let%expect_test "not even" =
  let dx = 0.3 in
  let lst = [ 0.0; 0.3; 0.6; 0.9 ] in
  let xs = get_x_coverage { dx } lst in
  xs |> List.iter (Printf.printf " %f ");
  [%expect {| 0.000000  0.300000  0.600000  0.900000 |}]

let%expect_test "not even" =
  let dx = 0.3 in
  let lst = [ 0.0; 0.3; 0.6; 0.9; 1.0 ] in
  let xs = get_x_coverage { dx } lst in
  xs |> List.iter (Printf.printf " %f ");
  [%expect {| 0.000000  0.300000  0.600000  0.900000  1.200000 |}]

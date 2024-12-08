open Common

let linearInterpolation { dx } points =
  let x1, y1 = List.hd points in
  let x2, y2 = points |> List.rev |> List.hd in
  let k = (y2 -. y1) /. (x2 -. x1) in
  let xs = points |> List.map (fun (x, _) -> x) in
  let answer_x = Common.get_x_coverage { dx } xs in
  let n = List.length answer_x in
  let answer_y = List.init n (fun i -> y1 +. (float_of_int i *. k)) in
  List.combine answer_x answer_y

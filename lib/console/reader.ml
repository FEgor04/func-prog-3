type read_line_result = Line of string | Eof

let read_line_opt () = try Line (read_line ()) with End_of_file -> Eof

let point_from_line line =
  let lst =
    line |> String.trim |> String.split_on_char ',' |> List.map float_of_string
  in
  let[@warning "-partial-match"] (x :: y :: _) = lst in
  (x, y)

let read_point () =
  let result = read_line_opt () in
  match result with Eof -> None | Line line -> Some (point_from_line line)

let rec list_is_sorted = function
  | [] -> true
  | [ _ ] -> true
  | a :: b :: c -> a < b && list_is_sorted (b :: c)

let%test "1 is sorted" = list_is_sorted [ 1 ]
let%test "empty list is sorted" = list_is_sorted []
let%test "1 2 3 4 is sorted" = list_is_sorted [ 1; 2; 3; 4 ]
let%test "1 2 3 4 5 is sorted" = list_is_sorted [ 1; 2; 3; 4; 5 ]
let%test "1 2 4 3 is not sorted" = not (list_is_sorted [ 1; 2; 4; 3 ])
let%test "1 2 3 5 4 is not sorted" = not (list_is_sorted [ 1; 2; 3; 5; 4 ])
let validate_points points = list_is_sorted points

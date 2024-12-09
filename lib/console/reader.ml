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

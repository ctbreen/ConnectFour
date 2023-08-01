exception Empty
exception Misinput

type command =
  | Num_Columns of int
  | Placement of int
  | Quit_Game
  | Empty
  | Unknown
  | YesAI
  | NoAI

type valid =
  | Invalid_width
  | Valid_width of int

let quit_parse lst =
  match lst with
  | [ "quit" ] -> Quit_Game
  | "quit" :: _ -> Unknown
  | _ -> Unknown

let column_parse lst =
  match lst with
  | [ x ] -> (
      let int_opt = int_of_string_opt x in
      match int_opt with
      | None -> Empty
      | Some x -> Placement x)
  | _ -> raise Misinput

let parse_command input_str =
  let list_of_string =
    List.filter (fun x -> x <> "") (String.split_on_char ' ' input_str)
  in
  match list_of_string with
  | [] -> Empty
  | "quit" :: _ -> quit_parse list_of_string
  | x :: _ -> column_parse list_of_string

let num_columns_parse s_list =
  match s_list with
  | [ x ] -> (
      let int_opt = int_of_string_opt x in
      match int_opt with
      | None -> Empty
      | Some x -> Num_Columns x)
  | _ -> raise Misinput

let parse_start_command input_str =
  let list_of_string =
    List.filter (fun x -> x <> "") (String.split_on_char ' ' input_str)
  in
  match list_of_string with
  | [] -> Empty
  | "quit" :: _ -> quit_parse list_of_string
  | x :: _ -> num_columns_parse list_of_string

let parse_y_or_n input_str =
  match input_str with
  | "y" -> YesAI
  | "n" -> NoAI
  | _ -> Unknown
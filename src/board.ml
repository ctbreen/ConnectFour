open List

exception InvalidMove

type player =
  | Red
  | Yellow
  | Empty

type piece_type =
  | Normal
  | Abnormal

type piece = {
  p : player;
  p_type : piece_type;
}

type column = piece list
type board = column list

let piece_color new_piece : player =
  let player_color = new_piece.p in
  player_color

let board_length b = List.length b

let height_inserted col_num board =
  if col_num < 0 || col_num > List.length board then raise InvalidMove
  else col_num |> List.nth board |> List.length |> ( + ) 1

let get_player col row board =
  if col < 0 || row < 0 then Empty
  else
    let column = List.nth_opt board col in
    if column = None then Empty
    else
      let the_piece = List.nth_opt (Option.get column) row in
      if the_piece = None then Empty else (Option.get the_piece).p

let get_player_opt col row (board : piece list list) : player option =
  if col < 0 || row < 0 then None
  else
    let column = List.nth_opt board col in
    if column = None then None
    else
      let square = List.nth_opt (Option.get column) row in
      if square = None then None else Some (Option.get square).p

let board_size board = List.fold_left (fun a b -> a + List.length b) 0 board


let make_piece color kind = { p = color; p_type = kind }

let make_move col_num piece_instance board =
  List.mapi
    (fun i x ->
      if i = col_num then x @ [piece_instance]
      else x)
    board

let make_move_temporary col_num board (color : player) =
  make_move col_num (make_piece color Normal) board

let rec winning_move_help col row (p : player) (board : piece list list)
    (l_inc, r_inc) =
  if get_player (col + l_inc) (row + r_inc) board = p then
    1 + winning_move_help (col + l_inc) (row + r_inc) p board (l_inc, r_inc)
  else 0

let winning_move_help_init col row (p : player) (board : piece list list)
    (l_inc, r_inc) =
  let first =
    if get_player (col + l_inc) (row + r_inc) board = p then 
      2 + winning_move_help (col + l_inc) (row+r_inc) p board (l_inc, r_inc) else 0
  in
  let second =
    if get_player (col - l_inc) (row - r_inc) board = p then
      winning_move_help (col - l_inc) (row - r_inc) p board (-l_inc, -r_inc)
      + if first = 0 then 2 else 1
    else 0
  in
  first + second

let winning_move col p board =
  let lst = [ (1, 0); (0, 1); (1, 1); (1, -1) ] in
  let lst' =
    List.map
      (fun x ->
        winning_move_help_init col (height_inserted col board - 1) p board x)
      lst
  in
  let largest_streak = List.find_opt (fun x -> x >= 4) lst' in
  if largest_streak <> None then (true, make_move_temporary col board p)
  else (false, [ [] ])

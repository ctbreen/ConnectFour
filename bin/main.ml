(** [main ()] will be the starting point for the game when player launches it. *)

(* so the idea here is to use a recursive function that calls itself whenever*)
(*let rec input_parser = match read_line () with | exception End_of_file -> () |
  input_text -> let _ = print_endline "Test" in let _ = input_parser in ()*)

open Game
open Board
open State
open Command
open Ai

exception InvalidBoard

let rec init_board num_cols =
  if num_cols = 0 then [] else [] :: init_board (num_cols - 1)

let rec get_line n = if n = 0 then "|" else "|    " ^ get_line (n - 1)

let rec repeat_lines line1 line2 n =
  if n = 0 then ""
  else line1 ^ "\n" ^ line2 ^ "\n" ^ repeat_lines line1 line2 (n - 1)

let rec get_line2 n = if n = 0 then "_\n" else "_____" ^ get_line2 (n - 1)

(*let print_piece piece_instance = match piece_instance

  let rec print_board board_instance n = List.map (fun x -> print_piece x)*)

let print_board n =
  let underscores = n |> get_line2 in
  let cells = n |> get_line in
  print_string ("\n\n\n\n\n" ^ repeat_lines underscores cells n ^ underscores)

(** extracts nth element from each row*)
let get_row_from_board (curr_board : piece list list) n =
  List.map (fun x -> List.nth_opt x (n - 1)) curr_board

let get_color_piece (x : piece option) =
  match x with
  | None -> print_string "|    "
  | Some y -> begin
      let z = Board.piece_color y in
      match z with
      | Red ->
          print_string "|  ";
          ANSITerminal.print_string [ ANSITerminal.red ] "X";
          print_string " "
      | Yellow ->
          print_string "|  ";
          ANSITerminal.print_string [ ANSITerminal.yellow ] "O";
          print_string " "
      | Empty -> ()
    end

let rec get_line_from_board_row row =
  match row with
  | [] -> print_string "|"
  | h :: t ->
      get_color_piece h;
      get_line_from_board_row t

let rec get_all_rows_from_board_help (curr_board : piece list list) n delim :
    unit =
  (*if n = 0 then delim else let line = n |> get_row_from_board curr_board |>
    get_line_from_board_row in delim ^ line ^ "\n" ^
    get_all_rows_from_board_help curr_board (n - 1) delim*)
  if n = 0 then print_string delim
  else (
    print_endline delim;
    n |> get_row_from_board curr_board |> get_line_from_board_row;
    print_string "\n";
    get_all_rows_from_board_help curr_board (n - 1) delim)

let get_all_rows_from_board curr_board =
  let board_width = List.length curr_board in
  get_all_rows_from_board_help curr_board board_width
    (get_line2 (List.length curr_board))

let rec numbers n n_track =
  if n = 0 then "\n"
  else
    let num = n_track - n in
    if num < 10 then
      "  " ^ string_of_int (n_track - n) ^ "  " ^ numbers (n - 1) n_track
    else "  " ^ string_of_int (n_track - n) ^ " " ^ numbers (n - 1) n_track

let print_numbers curr_board =
  let width = List.length curr_board in
  numbers width width

(*let print_board_filled curr_board = *)

let error_message error =
  if error <> "" then
    "\n\n\n\n\nLast input caused the following error: " ^ error
  else "\n"

let end_game_helper end_string state =
  if end_string = "Quit Game" then "You have quit Connect 4. See you soon!"
  else if end_string = "tie" then
    "You have tied, as there are no remaining squares left. See you soon!"
  else
    let who_didnt_win = placed_piece_color state in
    match who_didnt_win with
    | Red -> "Congrats Red Player, You Won!"
    | Yellow ->
        if get_ai_status state = false then "Congrats Yellow Player, You Won!"
        else "The ai bested you! :("
    | Empty -> "never"

let end_game end_string state =
  let final_string = end_game_helper end_string state in
  ANSITerminal.print_string [ ANSITerminal.green ] ("\n" ^ final_string ^ "\n");
  exit 0

let all_full board =
  let n = board_length board in
  List.fold_left (fun a b -> a + List.length b) 0 board = n * n

let rec during_game curr_board err state =
  if get_win_condition state then end_game "You Won" state
  else if all_full curr_board then
    let _ = get_all_rows_from_board curr_board in
    end_game "tie" state
  else if get_ai_status state = true && placed_piece_color state = Yellow then
    let ai_move = get_ai_move curr_board state in
    let won, b' =
      let n =
        match state.whos_move_next with
        | Red -> Red
        | Yellow -> Yellow
        | Empty -> Empty
      in
      winning_move ai_move n curr_board
    in
    if won then
      let _ = get_all_rows_from_board b' in
      end_game "You Won" state
    else
      try
        during_game
          (Board.make_move_temporary ai_move curr_board
             (State.placed_piece_color state))
          "" (new_state state)
      with _ -> during_game curr_board "Invalid Input" state
  else ANSITerminal.print_string [ ANSITerminal.magenta ] (error_message err);
  print_string "\nCurrent Board is: \n";
  get_all_rows_from_board curr_board;
  ANSITerminal.print_string [ ANSITerminal.blue ] (print_numbers curr_board);
  print_string "Type in which column you would like to place your piece: ";
  match read_line () with
  | exception End_of_file -> during_game curr_board "Unknown" state
  | str ->
      let command = Command.parse_command str in
      let integer =
        match command with
        | Placement i -> i
        | Empty -> -1
        | Unknown -> -2
        | Quit_Game -> -4
        | _ -> -3
      in
      let color = State.placed_piece_color state in
      let e =
        if integer = -1 then "You cannot enter a blank command"
        else if integer = -1 || board_length curr_board <= integer then
          "You must enter a valid column"
        else if integer = -2 then "Unknown command: try again"
        else if integer = -4 then "QUIT"
        else
          try
            let f = height_inserted integer curr_board in
            if f > board_length curr_board then
              "Invalid move: column " ^ string_of_int integer ^ " is full"
            else ""
          with InvalidMove ->
            "Invalid move: column " ^ string_of_int integer ^ " is full"
      in
      if e = "" then
        let won, b' =
          let n =
            match state.whos_move_next with
            | Red -> Red
            | Yellow -> Yellow
            | Empty -> Empty
          in
          winning_move integer n curr_board
        in
        if won then
          let _ = get_all_rows_from_board b' in
          end_game "You Won" state
        else
          try
            during_game
              (Board.make_move_temporary integer curr_board color)
              e (new_state state)
          with _ -> during_game curr_board "Invalid Input" state
      else if e = "QUIT" then end_game "Quit Game" state
      else during_game curr_board e state

let rec ai_setter s1 s2 =
  ANSITerminal.print_string [ ANSITerminal.cyan ] s1;
  print_string "\n ";
  ANSITerminal.print_string [ ANSITerminal.cyan ] s2;
  let y_or_n =
    match read_line () with
    | exception End_of_file -> ""
    | x -> x
  in
  let yes_or_no = parse_y_or_n y_or_n in
  match yes_or_no with
  | YesAI -> true
  | NoAI -> false
  | _ ->
      ai_setter "You did not input 'y' or 'n'"
        "Type 'y' to play with an ai and 'n' to not: "

(** [start_game n] starts a game with a n by n board*)
let start_game n =
  let ai_setter =
    ai_setter "Would you like to play with an ai?"
      "Type 'y' if yes and 'n' if no: "
  in
  let curr_board = init_board n in
  let curr_state = State.starter_state ai_setter in
  during_game curr_board "" curr_state

let rec before_game err =
  ANSITerminal.print_string [ ANSITerminal.red ] err;
  print_endline "";
  ANSITerminal.print_string [ ANSITerminal.red ]
    "TO PLAY INPUT A VALID BOARD LENGTH";
  print_string "";
  (* testing implementation works*)
  print_string
    "\n\
    \ Enter the desired length and width of your board. It must be at least 4 \
     and at most 8: ";
  let number =
    match read_line () with
    | exception End_of_file -> ""
    | x -> x
  in
  let parsed_n = Command.parse_start_command number in
  match parsed_n with
  | Num_Columns cols ->
      if cols < 4 || cols > 8 then
        before_game ("INVALID BOARD SIZE: (" ^ string_of_int cols ^ ")")
      else start_game cols
  | Quit_Game -> end_game "Quit Game" (State.starter_state false)
  | _ -> before_game "ERROR: PLEASE INPUT A VALID BOARD SIZE"

let main () =
  ANSITerminal.print_string [ ANSITerminal.cyan ] "Welcome to Connect 4";
  print_endline "";
  ANSITerminal.print_string [ ANSITerminal.on_yellow ]
    "Enjoy Playing Against Another Player";

  print_string
    "\n\
     Type quit to quit game at any time\n\
     Enter the desired length and width of your board. It must be at least 4 \
     and at most 8: ";
  let n =
    match read_line () with
    | exception End_of_file -> ""
    | x -> x
  in
  let parsed_n = Command.parse_start_command n in
  match parsed_n with
  | Num_Columns y ->
      if y < 4 || y > 8 then
        before_game ("INVALID BOARD SIZE: (" ^ string_of_int y ^ ")")
      else start_game y
  | Quit_Game -> end_game "Quit Game" (State.starter_state false)
  | _ -> before_game "ERROR: PLEASE INPUT A VALID BOARD SIZE"

(* Execute the game engine. *)
let () = main ()
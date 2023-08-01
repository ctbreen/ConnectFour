open Board
open Command

type state = {
  whos_move_next : player;
  ai_status : bool;
  game_won : bool;
}

let get_win_condition state = state.game_won

let get_ai_status state = state.ai_status

let starter_state ai_state =
  { whos_move_next = Red; ai_status = ai_state; game_won = false }

let create_state playa ai_state game_status =
  { whos_move_next = playa; ai_status = ai_state; game_won = game_status }

let placed_piece_color curr_state =
  let playa = curr_state.whos_move_next in
  playa

let new_state curr_state =
  let playa = curr_state.whos_move_next in
  match playa with
  | Red -> create_state Yellow curr_state.ai_status curr_state.game_won
  | Yellow -> create_state Red curr_state.ai_status curr_state.game_won
  | Empty -> create_state Red curr_state.ai_status curr_state.game_won

let new_state_with_wincheck curr_state maybewon =
  let playa = curr_state.whos_move_next in
  match playa with
  | Red ->
      create_state Yellow curr_state.ai_status
        (if maybewon then true else false)
  | Yellow ->
      create_state Red curr_state.ai_status (if maybewon then true else false)
  | Empty ->
      create_state Red curr_state.ai_status (if maybewon then true else false)

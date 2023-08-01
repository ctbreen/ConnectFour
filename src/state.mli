(** Representation of the state of the connect 4 game
    
    This module represents the state of the connect 4 game. It handles 
    processing of who's move is next, whether there is an ai in the game,
    and whether the game has been won yet. It also handles creation of states
    and unique states that only occur in certain instances (the starter state
    for example). This module also processes updating of state, taking in a 
    previous state and returning a new state.
*)


open Board
open Command

(**The type [state] represents the state of the game. It tracks who's
    move is next, whether there is an ai or not. and whether the game has
    been won or not. When whos_move_next is [Red] then the red player's turn is next
    and when whos_move_next is [Yellow] then the yellow player's turn is next.
    If ai_status is [false] then there is no ai and if ai_status is [true] then there
    is an ai. If ai_status is [true] then the ai plays as the yellow player. If 
    game_won is [true] then there has been a winning move made, otherwise not. *)
type state = {
  whos_move_next : player;
  ai_status : bool;
  game_won : bool;
}

val get_win_condition : state -> bool
(** [get_win_condition curr_state] is [curr_state.game_won]*)

val get_ai_status : state -> bool
(** [get_ai_status curr_state] is [curr_state.ai_status]*)

val starter_state : bool -> state
(** [starter_state t_or_f] is of type state where whos_move_next is [Red],
    ai_status is [t_or_f], and game_won is [false]*)

val create_state : player -> bool -> bool -> state
(** [create_state playa ai_state game_status] is of type state where
    whos_move_next is [playa], ai_status is [ai_state], and game_won
    is [game_status]*)

val placed_piece_color : state -> player
(** [placed_piece_color curr_state] is [curr_state.whos_move_next]*)

val new_state : state -> state
(** [new_state old_state] is the new state of the board given the previous
    state. If [old_state.whos_move_next] was [Red] then it is 
    [{old_state with whos_move_next = Yellow }]. If [old_state.whos_move_next] 
    was [Yellow] then it is [{old_state with whos_move_next = Red }].
    If [old_state.whos_move_next] was [Empty] then it is 
    [{old_state with whos_move_next = Red }].*)
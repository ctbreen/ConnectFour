(** Implementation of connect 4 ai
    
    This module represents the implementation of our connect 4 ai. Using the
    current board and the current state, our ai module is able to produce an
    informed piece placement. It also contains a component of randomization.
*)

open Board
open State
open Command

val get_ai_move : Board.board -> state -> int
(**[get_ai_move curr_board state] is an int [x] representing
   the column that the ai will place a piece in given the 
   current board [curr_board] and the current state [state]. *)
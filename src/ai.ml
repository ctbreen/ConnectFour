open Board
open State
open Command
open Random

let rec ai_helper curr_board col moves = 
  if col = List.length curr_board && List.length (snd moves) = 0 then (List.nth (fst moves) (Random.int (List.length (fst moves))))
  else if col = List.length curr_board then (List.nth (snd moves) (Random.int (List.length (snd moves))))
  else
 (match fst (Board.winning_move col Red curr_board), fst (Board.winning_move col Yellow curr_board), height_inserted col curr_board with
  | _, true, x -> if x-1 >= List.length curr_board then ai_helper curr_board (col + 1) moves else col
  | true, _, x -> if x-1 >= List.length curr_board then ai_helper curr_board (col + 1) moves else ai_helper curr_board (col + 1) (fst moves, List.append (snd moves) [col])
  | false, _, x -> if x-1 >= List.length curr_board then ai_helper curr_board (col + 1) moves
  else ai_helper curr_board (col + 1) (List.append (fst moves) [col], snd moves))

let get_ai_move curr_board state = 
  ai_helper curr_board 0 ([],[])

    




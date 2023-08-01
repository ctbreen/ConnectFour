(** Representation of the board of the connect 4 game

    This module represents the board of the connect 4 game. It sets up a type
    for the collumns, pieces, players, piece types, and board itself. It handles
    the process of making moves and assists in determining if the board
    is either in a winning state, non-winning state, or a draw state.
    This module also assist in preventing faulty moves. And printing to the screen.
*)


exception InvalidMove
(** Exception [InvalidMove] is raised when a player tries to make a move that is
    invalid *)

type piece
(** The type [piece] represents what player a given piece belongs to. *)

(** the type [player] represents which player a piece belongs to *)
type player =
  | Red
  | Yellow
  | Empty

(** the type [piece_type] represents the type of piece *)
type piece_type =
  | Normal
  | Abnormal

type column = piece list
(** The type [column] represents the pieces that exist in column, with the latest
    added piece always existing as the first element of the column. *)

type board = piece list list
(** The type [board] represents the board state *)

val piece_color : piece -> player
(** [piece_color p] is the piece color or player 
    associated with piece [p] *)

val board_length : piece list list -> int
(** [board_length b] is the int [x] where [b] has [x] columns *)

val height_inserted : int -> piece list list -> int
(** [height_inserted col board] is the height at which the piece would be
    inserted in column [col] in the board [board]. Raises [InvalidMove] if [col] is
    less than 0 or greater than the number of columns in [board]*)

val get_player : int -> int -> board -> player
(** [get_piece col_num pos board] returns which player the [pos]'th piece of the
    [col_num]'th column on board belongs to. Is [Empty] if n < 0 or there exists
    no piece at the given position*)

val winning_move : int -> player -> piece list list -> bool * piece list list
(** [winning_move col p board] checks if player [p] adding a piece to column
    [col] on board [board] wins for player [p] and returns [(ans, b')] where
    [ans] represents if the move wins and [b'] is the winning board if [ans] is
    true and an empty board if [ans] is false*)


val make_move : int -> piece -> piece list list -> piece list list
(** [make_move col_num piece_type board_state] adds a piece [piece_type] to the
    [col_num]'th column of board [board_state] *)

val make_piece : player -> piece_type -> piece
(** [make_piece color piece_t] is a piece record with color [color] and piece_type
    [piece_t]*)

val make_move_temporary : int -> piece list list -> player -> piece list list
(** [make_move_temp n board] tries to add a piece of color [Red] and type
    [Normal] to [board].*)

val make_move : int -> piece -> board -> board
(** [make_move n p b] adds a piece [p] to column [n] of board [b].*)

val board_size : board -> int
(** [board_size b] is [n] if there are [n] pieces on [b]*)
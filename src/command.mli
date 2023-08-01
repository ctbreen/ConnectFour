(** Implementation of command processing
    
    This module represents the implementation of command processing.
    It handles string inputs that are taken from the ANSITerminal
    and produces commands that are then able to be processed via our
    connect 4 system. 
*)


(** The type [command] represents a player command from the ANSITerminal 
    command line that is decomposed into a command, something that is 
    processable by our system for interpretation.  *)
type command =
  | Num_Columns of int
  | Placement of int
  | Quit_Game
  | Empty
  | Unknown
  | YesAI
  | NoAI

(** The type [valid] represents whether a width input was valid or not.
    The names are clearly representative of which is which. The object phrase
    of [Valid_width] is the width as an integer. *)
type valid =
  | Invalid_width
  | Valid_width of int

exception Empty
(** Raised when an empty command is attempted to be parsed. *)

exception Misinput
(** Raised when there is a misinput, an incorrect input from user *)

val quit_parse : string list -> command
(** [quit_parse lst] takes a list [lst] of strings, where the first element is
    "quit" and returns the command [Quit_Game] if the list has only "quit" as an
    element and returns [Empty] otherwise. *)

val column_parse : string list -> command
(** [column_parse lst] takes a list lst of strings. If there is a single element and it
    is an int in string form then it returns [Placement x] when x is the string in int
    form. It returns [Empty] if the single element was not an int in string form. Otherwise:
    Raises: [Misinput] *)

val parse_command : string -> command
(** [parse_command input_string] parses the player input into the terminal and
    if the player input is valid, it outputs a [command]. 
    
    Requires: [input_string] contains only alphanumeric and space characters.*)


val parse_start_command : string -> command
(** [parse_start_command input_string] parses the player input into the terminal and
    if the player input is valid, it outputs a [command]. This is used for column
    numbers. If [input_string] was "quit" then it returns [Quit_game]. If the 
    [input_string] was an int in string from it returns a command [Num_Columns]. 
    
    Requires: [input_string] contains only alphanumeric and space characters.*)

val parse_y_or_n : string -> command
(** [parse_y_or_n str] parses the player input. Is [YesAI] if
    [str] = "y". is [NoAI] if [str] = "n". Is [Unknown] otherwise.*)
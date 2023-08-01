open OUnit2
open Game
open Board
open Command

(* Testing Methodology Essentially all of our tests in OUNIT were run through
   the "winning_move" function, with some other tests on the command file. The
   rest of our testing was done manually by playing the game in the terminal and
   deciphering the error messages we received.

   This testing focused primarily on allowable inputs and end-game behavior. One
   reason this methodology showed the correctness of the system was that the
   "winning_move" function calls other functions which we were able to test
   through it. The other functions were also relatively simple so we could be
   confident in their correctness with the game running at all. Another reason
   for this methodology is that the win-and-tie conditions are the most
   important things for the user since they end the game. Because of this, we
   have a very long list of "winning_move" tests because we often were checking
   other aspects of the game like valid moves. Additionally, since
   height_inserted was critical to the function of a lot of our methods, we
   automatically tested that using OUnit test cases as well.

   Our OUNIT tests tested the Command and Board modules through glass box
   testing primarily to fully cover edge cases, with the occasional black box
   test to cover unexpected inputs/situations.

   Our manual tests tested the rest of the system including the State and Ai
   modules along with bin/main.ml which handled the U/I. Black box testing was
   our most common method for manual tests because we manually tested by playing
   the game, so black box tests ensured that we covered as many possible inputs
   by the user as possible. There were a couple of glass box manual tests,
   especially on the AI to ensure it follows the exact hueristic we gave *)

let get_player_test (name : string) (col : int) (row : int)
    (board : Board.board) (expected_output : Board.player) : test =
  name >:: fun _ -> assert_equal expected_output (get_player col row board)

let valid_command_test (name : string) (input : string)
    (expected_output : command) : test =
  name >:: fun _ -> assert_equal expected_output (Command.parse_command input)

let invalid_command_test (name : string) (input : string) expected_output =
  name >:: fun _ ->
  assert_raises expected_output (fun () -> Command.parse_command input)

let yes_no_test (name : string) (input : string) (expected_output : command) :
    test =
  name >:: fun _ -> assert_equal expected_output (Command.parse_y_or_n input)

let start_command_test (name : string) (input : string)
    (expected_output : command) : test =
  name >:: fun _ ->
  assert_equal expected_output (Command.parse_start_command input)

let get_piece i =
  if i mod 2 = 0 then make_piece Red Normal else make_piece Yellow Normal

let get_player i = if i mod 2 = 0 then Red else Yellow

let rec init_board num_cols =
  if num_cols = 0 then [] else [] :: init_board (num_cols - 1)

let rec apply_moves (input : int list) (original : board ref) : board ref =
  List.fold_left
    (fun a b ->
      let i = 1 + board_size !a in
      ref (make_move b (get_piece i) !a))
    original input

let rec apply_moves2 (input : int list) (original : piece list list ref) :
    piece list list ref =
  List.fold_left
    (fun a b ->
      let i = 1 + board_size !a in
      ref (make_move b (get_piece i) !a))
    original input

let winning_move_test (name : string)
    ((input1, input2, input3) : int * int list * int) (expected_output : bool) :
    test =
  name >:: fun _ ->
  let b = ref (init_board input1) in
  let b' = !(apply_moves input2 b) in
  let p = get_player (1 + List.length input2) in
  let result, _ = winning_move input3 p b' in
  assert_equal expected_output result

(* let ai_test (name : string) ((input1, input2) : int * int list)
   (expected_output : int) : test = name >:: fun _ -> let b = ref (init_board
   input1) in let b' = !(apply_moves input2 b) in let p = get_player (1 +
   List.length input2) in let result = get_ai_move b' p in assert_equal
   expected_output result

   let ai_tests = [ ai_test "less simple" (4, [0;0;0;0;1;1;1;1;3;2;2;2;2]) 3;
   ] *)

let rec init_board num_cols =
  if num_cols = 0 then [] else [] :: init_board (num_cols - 1)

let height_inserted_test (name : string) (b : piece list list)
    (moves : int list) (last_move : int) (expected_output : int) : test =
  name >:: fun _ ->
  let b2 = ref b in
  let b' = !(apply_moves2 moves b2) in
  assert_equal (height_inserted last_move b') expected_output

let board_tests =
  [
    height_inserted_test "empty" (init_board 4) [] 0 1;
    height_inserted_test "empty" (init_board 4) [] 1 1;
    height_inserted_test "empty" (init_board 4) [ 1 ] 1 2;
    height_inserted_test "empty" (init_board 4) [ 1; 2; 3; 2 ] 2 3;
    winning_move_test "empty" (4, [], 1) false;
    winning_move_test "simple" (4, [ 0; 1; 0; 1; 0; 1 ], 0) true;
    winning_move_test "less simple"
      (4, [ 0; 1; 1; 2; 2; 3; 2; 3; 3; 0 ], 3)
      true;
    winning_move_test "->4->0->4->4->3->3->2->4->0->0 1 true grid: 5"
      (5, [ 4; 0; 4; 4; 3; 3; 2; 4; 0; 0 ], 1)
      true;
    winning_move_test
      "->5->1->2->3->0->3->0->2->3->4->4->3->2->2->5->5->0->5 0 true grid: 6"
      (6, [ 5; 1; 2; 3; 0; 3; 0; 2; 3; 4; 4; 3; 2; 2; 5; 5; 0; 5 ], 0)
      true;
    winning_move_test "->3->3->3->6->7->3->2 5 false grid: 8"
      (8, [ 3; 3; 3; 6; 7; 3; 2 ], 5)
      false;
    winning_move_test "->0->4->1->1->4->1->0->4 3 false grid: 7"
      (7, [ 0; 4; 1; 1; 4; 1; 0; 4 ], 3)
      false;
    winning_move_test
      "->5->1->2->4->3->6->2->0->2->0->6->0->0->6->5->2->1->2 6 false grid: 8"
      (8, [ 5; 1; 2; 4; 3; 6; 2; 0; 2; 0; 6; 0; 0; 6; 5; 2; 1; 2 ], 6)
      false;
    winning_move_test " 3 false grid: 5" (5, [], 3) false;
    winning_move_test "->2->4->4->3->3->3->5->1->2->4 1 true grid: 7"
      (7, [ 2; 4; 4; 3; 3; 3; 5; 1; 2; 4 ], 1)
      true;
    winning_move_test
      "->2->3->3->2->4->1->3->0->4->3->4->0->2->1->3->1 1 true grid: 5"
      (5, [ 2; 3; 3; 2; 4; 1; 3; 0; 4; 3; 4; 0; 2; 1; 3; 1 ], 1)
      true;
    winning_move_test "->0->2->1->1->1->1->0->2->2->0->3 3 false grid: 4"
      (4, [ 0; 2; 1; 1; 1; 1; 0; 2; 2; 0; 3 ], 3)
      false;
    winning_move_test "->3->0->2->2->1 0 false grid: 4"
      (4, [ 3; 0; 2; 2; 1 ], 0)
      false;
    winning_move_test "->2->1->2->2 2 false grid: 5"
      (5, [ 2; 1; 2; 2 ], 2)
      false;
    winning_move_test
      "->3->5->5->1->2->5->0->2->4->2->4->3->2->0->5->0->5->4->5->0->2 3 true \
       grid: 6"
      (6, [ 3; 5; 5; 1; 2; 5; 0; 2; 4; 2; 4; 3; 2; 0; 5; 0; 5; 4; 5; 0; 2 ], 3)
      true;
    winning_move_test "->2->2->3->0->2->0->0->1 0 false grid: 4"
      (4, [ 2; 2; 3; 0; 2; 0; 0; 1 ], 0)
      false;
    winning_move_test
      "->4->1->6->5->7->2->2->4->4->7->3->0->5->0->3->7->5->6->1 5 false grid: \
       8"
      (8, [ 4; 1; 6; 5; 7; 2; 2; 4; 4; 7; 3; 0; 5; 0; 3; 7; 5; 6; 1 ], 5)
      false;
    winning_move_test
      "->0->7->3->0->1->7->4->6->7->0->0->4->1->5->6->3->5->5->7->3->3->2->7->0->0->2 \
       2 true grid: 8"
      ( 8,
        [
          0;
          7;
          3;
          0;
          1;
          7;
          4;
          6;
          7;
          0;
          0;
          4;
          1;
          5;
          6;
          3;
          5;
          5;
          7;
          3;
          3;
          2;
          7;
          0;
          0;
          2;
        ],
        2 )
      true;
    winning_move_test "->2->1->1 2 false grid: 4" (4, [ 2; 1; 1 ], 2) false;
    winning_move_test
      "->0->6->2->4->6->3->6->1->6->7->7->0->4->7->2->2->0->7->0->6 2 false \
       grid: 8"
      (8, [ 0; 6; 2; 4; 6; 3; 6; 1; 6; 7; 7; 0; 4; 7; 2; 2; 0; 7; 0; 6 ], 2)
      false;
    winning_move_test " 0 false grid: 5" (5, [], 0) false;
    winning_move_test
      "->3->2->2->6->1->4->3->4->1->1->0->4->1->0->2->3->4->3->3->4->2->3->3->2->1->4->1 \
       4 false grid: 7"
      ( 7,
        [
          3;
          2;
          2;
          6;
          1;
          4;
          3;
          4;
          1;
          1;
          0;
          4;
          1;
          0;
          2;
          3;
          4;
          3;
          3;
          4;
          2;
          3;
          3;
          2;
          1;
          4;
          1;
        ],
        4 )
      false;
    winning_move_test " 0 false grid: 4" (4, [], 0) false;
    winning_move_test
      "->2->2->2->4->4->1->4->1->4->3->2->0->0->4->1->2 0 false grid: 5"
      (5, [ 2; 2; 2; 4; 4; 1; 4; 1; 4; 3; 2; 0; 0; 4; 1; 2 ], 0)
      false;
    winning_move_test "->1->3->3->3->0 3 false grid: 4"
      (4, [ 1; 3; 3; 3; 0 ], 3)
      false;
    winning_move_test
      "->5->2->5->1->3->1->0->6->1->0->2->7->6->2->5->4->7->2->7->1 3 false \
       grid: 8"
      (8, [ 5; 2; 5; 1; 3; 1; 0; 6; 1; 0; 2; 7; 6; 2; 5; 4; 7; 2; 7; 1 ], 3)
      false;
    winning_move_test "->0->0->0->1 1 false grid: 4"
      (4, [ 0; 0; 0; 1 ], 1)
      false;
    winning_move_test
      "->3->1->2->1->4->1->2->4->1->0->0->3->1->4->4->2->4 3 true grid: 5"
      (5, [ 3; 1; 2; 1; 4; 1; 2; 4; 1; 0; 0; 3; 1; 4; 4; 2; 4 ], 3)
      true;
    winning_move_test "->6->1->1->2->1->2->6->0->2 3 true grid: 8"
      (8, [ 6; 1; 1; 2; 1; 2; 6; 0; 2 ], 3)
      true;
    winning_move_test "->3->2->1->1->2 2 false grid: 4"
      (4, [ 3; 2; 1; 1; 2 ], 2)
      false;
    winning_move_test
      "->2->4->7->1->6->7->1->6->1->5->6->3->4->0->1->6->0->5->3->2->3 7 false \
       grid: 8"
      (8, [ 2; 4; 7; 1; 6; 7; 1; 6; 1; 5; 6; 3; 4; 0; 1; 6; 0; 5; 3; 2; 3 ], 7)
      false;
    winning_move_test "->0->3->0->1->1->3 0 false grid: 4"
      (4, [ 0; 3; 0; 1; 1; 3 ], 0)
      false;
    winning_move_test "->1->3->0->2->2->2->0->0 3 false grid: 4"
      (4, [ 1; 3; 0; 2; 2; 2; 0; 0 ], 3)
      false;
    winning_move_test "->2->2->2->3->1->3->0->0->3 1 true grid: 4"
      (4, [ 2; 2; 2; 3; 1; 3; 0; 0; 3 ], 1)
      true;
    winning_move_test "->2->3->0->3->1->0->2->2->2->3->1 3 true grid: 4"
      (4, [ 2; 3; 0; 3; 1; 0; 2; 2; 2; 3; 1 ], 3)
      true;
    winning_move_test
      "->0->7->0->0->7->4->6->2->5->2->3->0->3->3->6->4->5->2->6->5->7->1->2->1->3->5->0->7->0->6->2->3 \
       4 true grid: 8"
      ( 8,
        [
          0;
          7;
          0;
          0;
          7;
          4;
          6;
          2;
          5;
          2;
          3;
          0;
          3;
          3;
          6;
          4;
          5;
          2;
          6;
          5;
          7;
          1;
          2;
          1;
          3;
          5;
          0;
          7;
          0;
          6;
          2;
          3;
        ],
        4 )
      true;
    winning_move_test "->1->5->4->4->4->4->2->4->1->3->3->5 3 false grid: 6"
      (6, [ 1; 5; 4; 4; 4; 4; 2; 4; 1; 3; 3; 5 ], 3)
      false;
    winning_move_test "->0->0->3->0->3->2->1->0->1->2->2->1->3 1 false grid: 4"
      (4, [ 0; 0; 3; 0; 3; 2; 1; 0; 1; 2; 2; 1; 3 ], 1)
      false;
    winning_move_test "->3->3->0->1->0->1->0->1->3 1 true grid: 4"
      (4, [ 3; 3; 0; 1; 0; 1; 0; 1; 3 ], 1)
      true;
    winning_move_test "->0->1->0->1->0->3 0 true grid: 4"
      (4, [ 0; 1; 0; 1; 0; 3 ], 0)
      true;
    winning_move_test "->0->2->3 1 false grid: 4" (4, [ 0; 2; 3 ], 1) false;
    winning_move_test
      "->1->2->1->2->2->1->4->1->1->2->2->0->3->4->4 0 false grid: 5"
      (5, [ 1; 2; 1; 2; 2; 1; 4; 1; 1; 2; 2; 0; 3; 4; 4 ], 0)
      false;
    winning_move_test
      "->0->7->5->2->0->6->6->4->1->1->4->3->7->3->7->3->3->0->3->2->7->6->2->7 \
       5 true grid: 8"
      ( 8,
        [
          0; 7; 5; 2; 0; 6; 6; 4; 1; 1; 4; 3; 7; 3; 7; 3; 3; 0; 3; 2; 7; 6; 2; 7;
        ],
        5 )
      true;
    winning_move_test "->1->1 2 false grid: 4" (4, [ 1; 1 ], 2) false;
    winning_move_test "->0->3->0->3->4->1->3->4 2 false grid: 5"
      (5, [ 0; 3; 0; 3; 4; 1; 3; 4 ], 2)
      false;
    winning_move_test
      "->1->0->1->2->4->1->4->5->1->7->7->6->6->3 5 true grid: 8"
      (8, [ 1; 0; 1; 2; 4; 1; 4; 5; 1; 7; 7; 6; 6; 3 ], 5)
      true;
    winning_move_test
      "->0->3->0->1->0->1->3->0->2->2->1->4->0->7->1->7->0->0->5->2->1->5->6->1->1->2->3->4->4 \
       2 true grid: 8"
      ( 8,
        [
          0;
          3;
          0;
          1;
          0;
          1;
          3;
          0;
          2;
          2;
          1;
          4;
          0;
          7;
          1;
          7;
          0;
          0;
          5;
          2;
          1;
          5;
          6;
          1;
          1;
          2;
          3;
          4;
          4;
        ],
        2 )
      true;
    winning_move_test "->0->1->0->1->3->1 3 false grid: 4"
      (4, [ 0; 1; 0; 1; 3; 1 ], 3)
      false;
    winning_move_test
      "->5->0->2->4->1->6->1->5->6->2->5->0->1->6->3->2->3->1->1->5->2->3->3->6->3->2->5->0->1->1->5->5->6->4->4->4->2 \
       0 true grid: 7"
      ( 7,
        [
          5;
          0;
          2;
          4;
          1;
          6;
          1;
          5;
          6;
          2;
          5;
          0;
          1;
          6;
          3;
          2;
          3;
          1;
          1;
          5;
          2;
          3;
          3;
          6;
          3;
          2;
          5;
          0;
          1;
          1;
          5;
          5;
          6;
          4;
          4;
          4;
          2;
        ],
        0 )
      true;
    winning_move_test "->2->3->1 3 false grid: 4" (4, [ 2; 3; 1 ], 3) false;
    winning_move_test "->0->6->5->7->3->7->6->2->7 0 false grid: 8"
      (8, [ 0; 6; 5; 7; 3; 7; 6; 2; 7 ], 0)
      false;
    winning_move_test
      "->0->2->1->6->5->4->2->5->2->0->6->2->5->2->4->5->0->3->3->4 1 true \
       grid: 7"
      (7, [ 0; 2; 1; 6; 5; 4; 2; 5; 2; 0; 6; 2; 5; 2; 4; 5; 0; 3; 3; 4 ], 1)
      true;
    winning_move_test
      "->2->1->4->3->3->3->5->5->2->5->5->3->4->3->3->0 1 true grid: 6"
      (6, [ 2; 1; 4; 3; 3; 3; 5; 5; 2; 5; 5; 3; 4; 3; 3; 0 ], 1)
      true;
    winning_move_test
      "->1->1->4->6->0->4->5->6->2->0->2->2->5->3->6->4->4->3->0->7 3 true \
       grid: 8"
      (8, [ 1; 1; 4; 6; 0; 4; 5; 6; 2; 0; 2; 2; 5; 3; 6; 4; 4; 3; 0; 7 ], 3)
      true;
    winning_move_test "->3->3->4->4->3->3->4->2->1 4 false grid: 5"
      (5, [ 3; 3; 4; 4; 3; 3; 4; 2; 1 ], 4)
      false;
    winning_move_test "->2->3->1->3->1 1 false grid: 4"
      (4, [ 2; 3; 1; 3; 1 ], 1)
      false;
    winning_move_test "->0->4->2->0->3->2 1 true grid: 5"
      (5, [ 0; 4; 2; 0; 3; 2 ], 1)
      true;
    winning_move_test
      "->0->4->0->6->1->6->4->6->6->0->3->6->1->0->0->2->3->2->3->3->0->4->1->4->2->5->2->2 \
       3 true grid: 7"
      ( 7,
        [
          0;
          4;
          0;
          6;
          1;
          6;
          4;
          6;
          6;
          0;
          3;
          6;
          1;
          0;
          0;
          2;
          3;
          2;
          3;
          3;
          0;
          4;
          1;
          4;
          2;
          5;
          2;
          2;
        ],
        3 )
      true;
    winning_move_test
      "->0->2->4->3->3->1->2->3->2->3->4->3->4->1 4 true grid: 5"
      (5, [ 0; 2; 4; 3; 3; 1; 2; 3; 2; 3; 4; 3; 4; 1 ], 4)
      true;
    winning_move_test " 1 false grid: 6" (6, [], 1) false;
    winning_move_test
      "->3->3->2->5->5->1->4->3->1->5->1->6->5->6->0->1->3->0->1->5->0->2->0->0->1->3 \
       2 true grid: 7"
      ( 7,
        [
          3;
          3;
          2;
          5;
          5;
          1;
          4;
          3;
          1;
          5;
          1;
          6;
          5;
          6;
          0;
          1;
          3;
          0;
          1;
          5;
          0;
          2;
          0;
          0;
          1;
          3;
        ],
        2 )
      true;
    winning_move_test "->0->1 4 false grid: 5" (5, [ 0; 1 ], 4) false;
    winning_move_test "->5->2->3->2->4->5->2->3->0 4 true grid: 6"
      (6, [ 5; 2; 3; 2; 4; 5; 2; 3; 0 ], 4)
      true;
    winning_move_test "->3->0->4->2->4->3->0->1 4 false grid: 5"
      (5, [ 3; 0; 4; 2; 4; 3; 0; 1 ], 4)
      false;
    winning_move_test
      "->3->3->0->3->2->2->2->4->5->4->4->0->4->1 1 false grid: 6"
      (6, [ 3; 3; 0; 3; 2; 2; 2; 4; 5; 4; 4; 0; 4; 1 ], 1)
      false;
    winning_move_test
      "->4->0->3->3->3->2->2->1->4->3->3->1->0->4 0 false grid: 5"
      (5, [ 4; 0; 3; 3; 3; 2; 2; 1; 4; 3; 3; 1; 0; 4 ], 0)
      false;
    winning_move_test "->4->1->3->2->3->0->3->0->1 4 false grid: 5"
      (5, [ 4; 1; 3; 2; 3; 0; 3; 0; 1 ], 4)
      false;
    winning_move_test "->1->1->2->0->0 1 false grid: 4"
      (4, [ 1; 1; 2; 0; 0 ], 1)
      false;
    winning_move_test
      "->3->4->3->5->0->0->4->0->4->5->3->2->2->4 3 true grid: 6"
      (6, [ 3; 4; 3; 5; 0; 0; 4; 0; 4; 5; 3; 2; 2; 4 ], 3)
      true;
    winning_move_test
      "->4->5->3->4->5->0->1->3->4->1->3->0->4->0->4->5->1->3 4 true grid: 6"
      (6, [ 4; 5; 3; 4; 5; 0; 1; 3; 4; 1; 3; 0; 4; 0; 4; 5; 1; 3 ], 4)
      true;
    winning_move_test "->4->4->4->1 0 false grid: 5"
      (5, [ 4; 4; 4; 1 ], 0)
      false;
    winning_move_test "->4->4->5->1->3->1 6 true grid: 7"
      (7, [ 4; 4; 5; 1; 3; 1 ], 6)
      true;
    winning_move_test "->4->3->2->4->0->2->4->4->3->3->3 1 true grid: 5"
      (5, [ 4; 3; 2; 4; 0; 2; 4; 4; 3; 3; 3 ], 1)
      true;
    winning_move_test
      "->6->4->6->1->6->6->2->0->5->5->7->2->2->7->2->6->4->0->6->7->0->0->2->4->3->7 \
       2 true grid: 8"
      ( 8,
        [
          6;
          4;
          6;
          1;
          6;
          6;
          2;
          0;
          5;
          5;
          7;
          2;
          2;
          7;
          2;
          6;
          4;
          0;
          6;
          7;
          0;
          0;
          2;
          4;
          3;
          7;
        ],
        2 )
      true;
    winning_move_test "->1->2->1->4->3->2->2->3->5->4->1->4->2 4 true grid: 6"
      (6, [ 1; 2; 1; 4; 3; 2; 2; 3; 5; 4; 1; 4; 2 ], 4)
      true;
    winning_move_test "->5->5->0->5->2->0->4->2->0->2 3 true grid: 6"
      (6, [ 5; 5; 0; 5; 2; 0; 4; 2; 0; 2 ], 3)
      true;
    winning_move_test
      "->4->1->6->6->5->4->4->5->2->0->6->3->0->4->5->4->1 3 true grid: 7"
      (7, [ 4; 1; 6; 6; 5; 4; 4; 5; 2; 0; 6; 3; 0; 4; 5; 4; 1 ], 3)
      true;
    winning_move_test "->1->2->1->0->1->2 1 true grid: 4"
      (4, [ 1; 2; 1; 0; 1; 2 ], 1)
      true;
    winning_move_test "->4->1->4->0->0->2->4->2->3->4->0->3->3 1 false grid: 5"
      (5, [ 4; 1; 4; 0; 0; 2; 4; 2; 3; 4; 0; 3; 3 ], 1)
      false;
    winning_move_test " 0 false grid: 4" (4, [], 0) false;
    winning_move_test
      "->4->6->5->4->5->5->3->5->3->5->5->2->3->5->0->4->4->4->6->4 3 true \
       grid: 7"
      (7, [ 4; 6; 5; 4; 5; 5; 3; 5; 3; 5; 5; 2; 3; 5; 0; 4; 4; 4; 6; 4 ], 3)
      true;
    winning_move_test "->6 4 false grid: 7" (7, [ 6 ], 4) false;
    winning_move_test "->1->1->0->0 2 false grid: 4"
      (4, [ 1; 1; 0; 0 ], 2)
      false;
    winning_move_test
      "->1->0->2->4->4->0->2->2->1->0->4->4->4->2->0->0->2 1 false grid: 5"
      (5, [ 1; 0; 2; 4; 4; 0; 2; 2; 1; 0; 4; 4; 4; 2; 0; 0; 2 ], 1)
      false;
    winning_move_test
      "->1->3->3->3->0->0->5->4->3->4->2->3->0->2->0->3->0->5 0 true grid: 6"
      (6, [ 1; 3; 3; 3; 0; 0; 5; 4; 3; 4; 2; 3; 0; 2; 0; 3; 0; 5 ], 0)
      true;
    winning_move_test
      "->2->3->2->0->6->2->3->4->3->6->3->3->0->1->6->3->2->6->0->1->3->2->5->4->6->6->1->0->2->0->5->4->0->2->4->4->6->0->5->1 \
       5 true grid: 7"
      ( 7,
        [
          2;
          3;
          2;
          0;
          6;
          2;
          3;
          4;
          3;
          6;
          3;
          3;
          0;
          1;
          6;
          3;
          2;
          6;
          0;
          1;
          3;
          2;
          5;
          4;
          6;
          6;
          1;
          0;
          2;
          0;
          5;
          4;
          0;
          2;
          4;
          4;
          6;
          0;
          5;
          1;
        ],
        5 )
      true;
    winning_move_test "->1->2->1 3 false grid: 4" (4, [ 1; 2; 1 ], 3) false;
    winning_move_test "->2->5->4->4->2->6->5->0->2 4 false grid: 7"
      (7, [ 2; 5; 4; 4; 2; 6; 5; 0; 2 ], 4)
      false;
    winning_move_test " 5 false grid: 6" (6, [], 5) false;
    winning_move_test " 0 false grid: 8" (8, [], 0) false;
    winning_move_test "->1->2->5->5->0 0 false grid: 6"
      (6, [ 1; 2; 5; 5; 0 ], 0)
      false;
    winning_move_test "->6->7->5->7->3->3->1->7->2->6->6->2->6 7 true grid: 8"
      (8, [ 6; 7; 5; 7; 3; 3; 1; 7; 2; 6; 6; 2; 6 ], 7)
      true;
    winning_move_test " 0 false grid: 4" (4, [], 0) false;
    winning_move_test "->3->0->3->1->2->0->2 3 false grid: 5"
      (5, [ 3; 0; 3; 1; 2; 0; 2 ], 3)
      false;
    winning_move_test
      "->6->7->3->1->2->1->1->6->6->6->2->5->7->4->7->6 1 false grid: 8"
      (8, [ 6; 7; 3; 1; 2; 1; 1; 6; 6; 6; 2; 5; 7; 4; 7; 6 ], 1)
      false;
    winning_move_test "->3->2->2->1->0->2->0->0->0->1->2->3 3 false grid: 4"
      (4, [ 3; 2; 2; 1; 0; 2; 0; 0; 0; 1; 2; 3 ], 3)
      false;
    winning_move_test "->2->2->0 5 false grid: 6" (6, [ 2; 2; 0 ], 5) false;
    winning_move_test
      "->0->4->7->0->7->0->0->1->3->1->7->1->5->4->4->0->3->3->2 1 true grid: 8"
      (8, [ 0; 4; 7; 0; 7; 0; 0; 1; 3; 1; 7; 1; 5; 4; 4; 0; 3; 3; 2 ], 1)
      true;
    winning_move_test " 0 false grid: 5" (5, [], 0) false;
    winning_move_test "->1->7->4->2->3 0 false grid: 8"
      (8, [ 1; 7; 4; 2; 3 ], 0)
      false;
    winning_move_test "->3->1->3->2->0->1->0->3->0->2 0 true grid: 4"
      (4, [ 3; 1; 3; 2; 0; 1; 0; 3; 0; 2 ], 0)
      true;
    winning_move_test "->1->4->1->3->0->4->1->2->3 5 true grid: 6"
      (6, [ 1; 4; 1; 3; 0; 4; 1; 2; 3 ], 5)
      true;
    winning_move_test " 3 false grid: 6" (6, [], 3) false;
    winning_move_test "->2->0->2->2->2->3->1->3 1 false grid: 4"
      (4, [ 2; 0; 2; 2; 2; 3; 1; 3 ], 1)
      false;
    winning_move_test
      "->4->0->7->0->5->7->2->7->4->2->2->3->3->1->0->2->3->3->4->4->4->5->2->5 \
       6 true grid: 8"
      ( 8,
        [
          4; 0; 7; 0; 5; 7; 2; 7; 4; 2; 2; 3; 3; 1; 0; 2; 3; 3; 4; 4; 4; 5; 2; 5;
        ],
        6 )
      true;
    winning_move_test "->0->4->4->1->0->3->2->2->4 0 false grid: 5"
      (5, [ 0; 4; 4; 1; 0; 3; 2; 2; 4 ], 0)
      false;
    winning_move_test "->0->1->0->1->1->0->3->5->1->0->4->0->2 1 false grid: 6"
      (6, [ 0; 1; 0; 1; 1; 0; 3; 5; 1; 0; 4; 0; 2 ], 1)
      false;
    winning_move_test
      "->3->0->6->1->6->4->1->5->4->3->4->6->0->5->1->4->3->4->1->5->2->4->0->2->3->2->3->0 \
       1 true grid: 7"
      ( 7,
        [
          3;
          0;
          6;
          1;
          6;
          4;
          1;
          5;
          4;
          3;
          4;
          6;
          0;
          5;
          1;
          4;
          3;
          4;
          1;
          5;
          2;
          4;
          0;
          2;
          3;
          2;
          3;
          0;
        ],
        1 )
      true;
    winning_move_test
      "->2->7->5->0->2->5->3->7->5->1->0->4->4->7->7->2->2->3->7->2->4->1->1 1 \
       true grid: 8"
      ( 8,
        [ 2; 7; 5; 0; 2; 5; 3; 7; 5; 1; 0; 4; 4; 7; 7; 2; 2; 3; 7; 2; 4; 1; 1 ],
        1 )
      true;
    winning_move_test " 0 false grid: 7" (7, [], 0) false;
    winning_move_test "->1->0->5->6->2->2->1 0 false grid: 7"
      (7, [ 1; 0; 5; 6; 2; 2; 1 ], 0)
      false;
    winning_move_test "->1->1->2->2 3 false grid: 4"
      (4, [ 1; 1; 2; 2 ], 3)
      false;
    winning_move_test "->5->4->4 4 false grid: 6" (6, [ 5; 4; 4 ], 4) false;
    winning_move_test "->2->0->4->1->3 1 false grid: 5"
      (5, [ 2; 0; 4; 1; 3 ], 1)
      false;
    winning_move_test
      "->4->4->1->3->2->0->2->5->0->4->0->3->5->4->5 1 false grid: 6"
      (6, [ 4; 4; 1; 3; 2; 0; 2; 5; 0; 4; 0; 3; 5; 4; 5 ], 1)
      false;
    winning_move_test "->0->2->1->2->1->1 1 false grid: 5"
      (5, [ 0; 2; 1; 2; 1; 1 ], 1)
      false;
    winning_move_test "->2->4->3->0 1 false grid: 5"
      (5, [ 2; 4; 3; 0 ], 1)
      false;
    winning_move_test
      "->6->2->5->2->3->3->0->1->0->3->0->6->6->2->1->1->7->7->3->4->5 1 true \
       grid: 8"
      (8, [ 6; 2; 5; 2; 3; 3; 0; 1; 0; 3; 0; 6; 6; 2; 1; 1; 7; 7; 3; 4; 5 ], 1)
      true;
    winning_move_test "->2->2->1->1->3->3->2->3 1 false grid: 4"
      (4, [ 2; 2; 1; 1; 3; 3; 2; 3 ], 1)
      false;
    winning_move_test "->3->0->0->1->3->4->3->3 2 false grid: 6"
      (6, [ 3; 0; 0; 1; 3; 4; 3; 3 ], 2)
      false;
    winning_move_test
      "->2->5->5->4->1->4->4->4->4->3->4->0->5->0->0->1->1->2->5->5->1->0->0->0->2->2->1->2 \
       1 true grid: 6"
      ( 6,
        [
          2;
          5;
          5;
          4;
          1;
          4;
          4;
          4;
          4;
          3;
          4;
          0;
          5;
          0;
          0;
          1;
          1;
          2;
          5;
          5;
          1;
          0;
          0;
          0;
          2;
          2;
          1;
          2;
        ],
        1 )
      true;
    winning_move_test "->3->0->2->3->1 3 false grid: 4"
      (4, [ 3; 0; 2; 3; 1 ], 3)
      false;
    winning_move_test "->0->3->3->3->0->2->3->0->2->0->1->1->1 1 false grid: 4"
      (4, [ 0; 3; 3; 3; 0; 2; 3; 0; 2; 0; 1; 1; 1 ], 1)
      false;
    winning_move_test
      "->2->3->0->2->2->3->3->0->4->2->1->3->4->2->1->3->1 0 false grid: 5"
      (5, [ 2; 3; 0; 2; 2; 3; 3; 0; 4; 2; 1; 3; 4; 2; 1; 3; 1 ], 0)
      false;
    winning_move_test "->6->6->1->2 2 false grid: 7"
      (7, [ 6; 6; 1; 2 ], 2)
      false;
    winning_move_test
      "->1->1->3->3->0->0->3->1->3->3->1->1->4->0->4->4->0->0->4->4 2 true \
       grid: 5"
      (5, [ 1; 1; 3; 3; 0; 0; 3; 1; 3; 3; 1; 1; 4; 0; 4; 4; 0; 0; 4; 4 ], 2)
      true;
    winning_move_test
      "->0->1->4->1->2->2->0->3->2->2->0->0->4->4->5->0->2->2->5->5 0 false \
       grid: 6"
      (6, [ 0; 1; 4; 1; 2; 2; 0; 3; 2; 2; 0; 0; 4; 4; 5; 0; 2; 2; 5; 5 ], 0)
      false;
    winning_move_test " 0 false grid: 4" (4, [], 0) false;
    winning_move_test "->3->2 1 false grid: 4" (4, [ 3; 2 ], 1) false;
    winning_move_test
      "->4->0->6->6->1->4->6->3->2->4->0->0->2->5->1->6->5->1->5->3->4->3->5->5->6->4->2->6->0->6->4->1->5 \
       3 true grid: 7"
      ( 7,
        [
          4;
          0;
          6;
          6;
          1;
          4;
          6;
          3;
          2;
          4;
          0;
          0;
          2;
          5;
          1;
          6;
          5;
          1;
          5;
          3;
          4;
          3;
          5;
          5;
          6;
          4;
          2;
          6;
          0;
          6;
          4;
          1;
          5;
        ],
        3 )
      true;
    winning_move_test
      "->6->0->3->0->3->1->0->1->4->5->2->6->2->6->4->5->5->4->1->0->1->2->0->1->3->0->4->4 \
       3 true grid: 7"
      ( 7,
        [
          6;
          0;
          3;
          0;
          3;
          1;
          0;
          1;
          4;
          5;
          2;
          6;
          2;
          6;
          4;
          5;
          5;
          4;
          1;
          0;
          1;
          2;
          0;
          1;
          3;
          0;
          4;
          4;
        ],
        3 )
      true;
    winning_move_test "->1->3->2->3->2->2 3 false grid: 4"
      (4, [ 1; 3; 2; 3; 2; 2 ], 3)
      false;
    winning_move_test
      "->5->0->5->3->3->0->2->5->3->1->5->3->5->2->1 5 false grid: 6"
      (6, [ 5; 0; 5; 3; 3; 0; 2; 5; 3; 1; 5; 3; 5; 2; 1 ], 5)
      false;
    winning_move_test
      "->3->1->4->4->1->2->0->4->3->2->1->2->3->0->4->4 3 true grid: 7"
      (7, [ 3; 1; 4; 4; 1; 2; 0; 4; 3; 2; 1; 2; 3; 0; 4; 4 ], 3)
      true;
    winning_move_test
      "->5->0->2->5->2->4->5->4->0->4->3->6->5->5->2->5 2 true grid: 7"
      (7, [ 5; 0; 2; 5; 2; 4; 5; 4; 0; 4; 3; 6; 5; 5; 2; 5 ], 2)
      true;
    winning_move_test "->0->3->4->1->4->2->0->5->6->3->0 2 false grid: 7"
      (7, [ 0; 3; 4; 1; 4; 2; 0; 5; 6; 3; 0 ], 2)
      false;
    winning_move_test
      "->2->5->7->2->2->6->3->5->5->0->6->1->2->3->7->1->1->6->0->4->1->2->0 4 \
       true grid: 8"
      ( 8,
        [ 2; 5; 7; 2; 2; 6; 3; 5; 5; 0; 6; 1; 2; 3; 7; 1; 1; 6; 0; 4; 1; 2; 0 ],
        4 )
      true;
    winning_move_test
      "->0->6->4->5->1->5->4->0->0->3->2->4->1->6->3->1->3 3 true grid: 7"
      (7, [ 0; 6; 4; 5; 1; 5; 4; 0; 0; 3; 2; 4; 1; 6; 3; 1; 3 ], 3)
      true;
    winning_move_test
      "->1->3->3->3->0->1->4->1->3->0->0->2->1->3->4->0->1->0->4->5->3->1->0 2 \
       true grid: 6"
      ( 6,
        [ 1; 3; 3; 3; 0; 1; 4; 1; 3; 0; 0; 2; 1; 3; 4; 0; 1; 0; 4; 5; 3; 1; 0 ],
        2 )
      true;
    winning_move_test "->2->0->4->3->3->3->1->3->0->1->4->0->4 3 false grid: 5"
      (5, [ 2; 0; 4; 3; 3; 3; 1; 3; 0; 1; 4; 0; 4 ], 3)
      false;
    winning_move_test "->0->3->6->5->6->0->3->6->2->4->4->5 1 false grid: 7"
      (7, [ 0; 3; 6; 5; 6; 0; 3; 6; 2; 4; 4; 5 ], 1)
      false;
    winning_move_test
      "->5->6->0->6->6->4->5->2->1->4->3->4->3->1->2->3->6->6->4->0->3->1->0->0->3 \
       2 true grid: 7"
      ( 7,
        [
          5;
          6;
          0;
          6;
          6;
          4;
          5;
          2;
          1;
          4;
          3;
          4;
          3;
          1;
          2;
          3;
          6;
          6;
          4;
          0;
          3;
          1;
          0;
          0;
          3;
        ],
        2 )
      true;
    winning_move_test "->0->2->0->3->1 3 false grid: 4"
      (4, [ 0; 2; 0; 3; 1 ], 3)
      false;
    winning_move_test "->1->2 3 false grid: 4" (4, [ 1; 2 ], 3) false;
    winning_move_test "->1->3->3->0->3->0->3->4->2 2 false grid: 6"
      (6, [ 1; 3; 3; 0; 3; 0; 3; 4; 2 ], 2)
      false;
    winning_move_test
      "->3->5->4->5->1->2->1->1->4->3->1->4->3->4->1->2->3->1->5 5 true grid: 6"
      (6, [ 3; 5; 4; 5; 1; 2; 1; 1; 4; 3; 1; 4; 3; 4; 1; 2; 3; 1; 5 ], 5)
      true;
    winning_move_test
      "->0->2->0->3->1->1->5->2->5->4->1->0->1->3->0->2->5 2 true grid: 6"
      (6, [ 0; 2; 0; 3; 1; 1; 5; 2; 5; 4; 1; 0; 1; 3; 0; 2; 5 ], 2)
      true;
    winning_move_test "->4->5->0->1->0->0->4->0->5->3->2 1 false grid: 6"
      (6, [ 4; 5; 0; 1; 0; 0; 4; 0; 5; 3; 2 ], 1)
      false;
    winning_move_test
      "->5->4->0->4->1->1->5->5->1->0->5->0->0->3->5->3->0 0 false grid: 6"
      (6, [ 5; 4; 0; 4; 1; 1; 5; 5; 1; 0; 5; 0; 0; 3; 5; 3; 0 ], 0)
      false;
    winning_move_test
      "->2->2->1->3->0->4->6->4->1->1->6->6->1->1->3->4->2->6->0->3->6->5->5->3 \
       3 false grid: 7"
      ( 7,
        [
          2; 2; 1; 3; 0; 4; 6; 4; 1; 1; 6; 6; 1; 1; 3; 4; 2; 6; 0; 3; 6; 5; 5; 3;
        ],
        3 )
      false;
    winning_move_test "->4->1->2->4->1->5->2->1 0 false grid: 7"
      (7, [ 4; 1; 2; 4; 1; 5; 2; 1 ], 0)
      false;
    winning_move_test "->2->1->1->3 0 false grid: 5"
      (5, [ 2; 1; 1; 3 ], 0)
      false;
    winning_move_test "->2->3->3->1->3->2->2->2->0->1->1 0 false grid: 4"
      (4, [ 2; 3; 3; 1; 3; 2; 2; 2; 0; 1; 1 ], 0)
      false;
    winning_move_test " 2 false grid: 6" (6, [], 2) false;
    winning_move_test "->3->2->1->2->6->2->1->3->1->4 1 true grid: 7"
      (7, [ 3; 2; 1; 2; 6; 2; 1; 3; 1; 4 ], 1)
      true;
    winning_move_test
      "->2->4->5->1->0->5->4->0->5->3->0->3->3->4->3->1->1 4 false grid: 6"
      (6, [ 2; 4; 5; 1; 0; 5; 4; 0; 5; 3; 0; 3; 3; 4; 3; 1; 1 ], 4)
      false;
    winning_move_test " 6 false grid: 8" (8, [], 6) false;
    winning_move_test
      "->2->6->5->4->1->6->0->1->4->3->1->4->1->0->3->2 3 false grid: 7"
      (7, [ 2; 6; 5; 4; 1; 6; 0; 1; 4; 3; 1; 4; 1; 0; 3; 2 ], 3)
      false;
    winning_move_test " 0 false grid: 4" (4, [], 0) false;
    winning_move_test
      "->3->2->2->5->7->5->3->7->7->7->0->3->2->4->2->5->6 5 true grid: 8"
      (8, [ 3; 2; 2; 5; 7; 5; 3; 7; 7; 7; 0; 3; 2; 4; 2; 5; 6 ], 5)
      true;
    winning_move_test "->3->2->2->0->2->3->1->2->3->1->0 1 false grid: 4"
      (4, [ 3; 2; 2; 0; 2; 3; 1; 2; 3; 1; 0 ], 1)
      false;
    winning_move_test "->0->3->1->0->2->1->0->2->3->3->0->0 1 false grid: 5"
      (5, [ 0; 3; 1; 0; 2; 1; 0; 2; 3; 3; 0; 0 ], 1)
      false;
    winning_move_test "->4->3->3->3->2->3->3->2 0 false grid: 5"
      (5, [ 4; 3; 3; 3; 2; 3; 3; 2 ], 0)
      false;
    winning_move_test "->3->2->3->4->3->4->2->4->1 4 true grid: 5"
      (5, [ 3; 2; 3; 4; 3; 4; 2; 4; 1 ], 4)
      true;
    winning_move_test "->1->0->3->3->3->3->2->1->2->1->0->1->0 2 true grid: 4"
      (4, [ 1; 0; 3; 3; 3; 3; 2; 1; 2; 1; 0; 1; 0 ], 2)
      true;
    winning_move_test
      "->6->1->6->1->3->3->0->4->4->4->2->6->1->1->3->6->2->6->5 2 true grid: 7"
      (7, [ 6; 1; 6; 1; 3; 3; 0; 4; 4; 4; 2; 6; 1; 1; 3; 6; 2; 6; 5 ], 2)
      true;
    winning_move_test
      "->7->0->2->4->7->2->1->7->2->4->0->2->4->3->1->2->3->5->3->3->5 6 true \
       grid: 8"
      (8, [ 7; 0; 2; 4; 7; 2; 1; 7; 2; 4; 0; 2; 4; 3; 1; 2; 3; 5; 3; 3; 5 ], 6)
      true;
    winning_move_test "->3->1->0->2->1->2->0->1->0->0->1->2->2 3 false grid: 4"
      (4, [ 3; 1; 0; 2; 1; 2; 0; 1; 0; 0; 1; 2; 2 ], 3)
      false;
    winning_move_test " 0 false grid: 4" (4, [], 0) false;
    winning_move_test "->1->3->0->3->1->2->3->3->0 2 false grid: 4"
      (4, [ 1; 3; 0; 3; 1; 2; 3; 3; 0 ], 2)
      false;
    winning_move_test "->0->4->0->1->1->0->0->0->2->3->3 2 false grid: 5"
      (5, [ 0; 4; 0; 1; 1; 0; 0; 0; 2; 3; 3 ], 2)
      false;
    winning_move_test
      "->5->4->1->4->3->3->2->5->3->1->5->4->3->5->4->3->1->4->3->4 2 true \
       grid: 6"
      (6, [ 5; 4; 1; 4; 3; 3; 2; 5; 3; 1; 5; 4; 3; 5; 4; 3; 1; 4; 3; 4 ], 2)
      true;
    winning_move_test "->3->3->0->5->2->2->0->2 1 true grid: 7"
      (7, [ 3; 3; 0; 5; 2; 2; 0; 2 ], 1)
      true;
    winning_move_test "->2->2->3->5->5->3 3 false grid: 7"
      (7, [ 2; 2; 3; 5; 5; 3 ], 3)
      false;
    winning_move_test "->1->4->4->1->2->1->0->3->0->5 6 false grid: 7"
      (7, [ 1; 4; 4; 1; 2; 1; 0; 3; 0; 5 ], 6)
      false;
    winning_move_test "->1->2 4 false grid: 6" (6, [ 1; 2 ], 4) false;
    winning_move_test
      "->2->3->4->5->7->1->1->3->0->7->3->0->1->1->3->7->0->5->7->4->4->5->1 5 \
       true grid: 8"
      ( 8,
        [ 2; 3; 4; 5; 7; 1; 1; 3; 0; 7; 3; 0; 1; 1; 3; 7; 0; 5; 7; 4; 4; 5; 1 ],
        5 )
      true;
    winning_move_test
      "->3->5->6->3->3->2->2->4->2->2->2->2->3->0->2->0->1->4->5->4->1->6->0->1 \
       4 true grid: 7"
      ( 7,
        [
          3; 5; 6; 3; 3; 2; 2; 4; 2; 2; 2; 2; 3; 0; 2; 0; 1; 4; 5; 4; 1; 6; 0; 1;
        ],
        4 )
      true;
    winning_move_test "->3->1->1->2->2->2->2->1->0 1 false grid: 4"
      (4, [ 3; 1; 1; 2; 2; 2; 2; 1; 0 ], 1)
      false;
    winning_move_test "->4->3->2 3 false grid: 5" (5, [ 4; 3; 2 ], 3) false;
    winning_move_test
      "->2->2->0->4->3->1->4->1->1->6->5->2->4->4->1->4->2->5->6->0->0->5->6->4->4->2->0->1->6->5->0->2->1->6->2->6->5->5->6->1->3->5 \
       3 true grid: 7"
      ( 7,
        [
          2;
          2;
          0;
          4;
          3;
          1;
          4;
          1;
          1;
          6;
          5;
          2;
          4;
          4;
          1;
          4;
          2;
          5;
          6;
          0;
          0;
          5;
          6;
          4;
          4;
          2;
          0;
          1;
          6;
          5;
          0;
          2;
          1;
          6;
          2;
          6;
          5;
          5;
          6;
          1;
          3;
          5;
        ],
        3 )
      true;
    winning_move_test "->2->1->4->1->3->0->4->2->2->4 1 false grid: 5"
      (5, [ 2; 1; 4; 1; 3; 0; 4; 2; 2; 4 ], 1)
      false;
    winning_move_test "->3->1->0->3->2->0->0->3->2->2->1->3->3 1 false grid: 5"
      (5, [ 3; 1; 0; 3; 2; 0; 0; 3; 2; 2; 1; 3; 3 ], 1)
      false;
    winning_move_test " 0 false grid: 5" (5, [], 0) false;
    winning_move_test
      "->2->2->2->4->6->4->1->5->0->2->2->5->6->3->6->6->6 3 true grid: 7"
      (7, [ 2; 2; 2; 4; 6; 4; 1; 5; 0; 2; 2; 5; 6; 3; 6; 6; 6 ], 3)
      true;
    winning_move_test "->6->0->7->3->6->3->2->3->5->1->6->7 4 true grid: 8"
      (8, [ 6; 0; 7; 3; 6; 3; 2; 3; 5; 1; 6; 7 ], 4)
      true;
    winning_move_test
      "->5->2->2->5->0->3->3->1->5->0->1->3->0->3->1->2->2 0 false grid: 6"
      (6, [ 5; 2; 2; 5; 0; 3; 3; 1; 5; 0; 1; 3; 0; 3; 1; 2; 2 ], 0)
      false;
    winning_move_test "->1 0 false grid: 8" (8, [ 1 ], 0) false;
    winning_move_test
      "->2->3->0->2->2->0->5->7->5->0->2->2->7->7->4->3->1->6->5->3->0->2->2->5->6->4->3->0->4->2 \
       5 false grid: 8"
      ( 8,
        [
          2;
          3;
          0;
          2;
          2;
          0;
          5;
          7;
          5;
          0;
          2;
          2;
          7;
          7;
          4;
          3;
          1;
          6;
          5;
          3;
          0;
          2;
          2;
          5;
          6;
          4;
          3;
          0;
          4;
          2;
        ],
        5 )
      false;
    winning_move_test
      "->0->4->4->7->4->3->7->5->5->1->1->0->2->0->1->3->0->2->0->4->5 6 true \
       grid: 8"
      (8, [ 0; 4; 4; 7; 4; 3; 7; 5; 5; 1; 1; 0; 2; 0; 1; 3; 0; 2; 0; 4; 5 ], 6)
      true;
    winning_move_test
      "->0->6->5->0->6->4->3->0->3->5->0->2->3->2->1->2->1->0->4 2 true grid: 7"
      (7, [ 0; 6; 5; 0; 6; 4; 3; 0; 3; 5; 0; 2; 3; 2; 1; 2; 1; 0; 4 ], 2)
      true;
    winning_move_test "->5->1->2->3->2 1 false grid: 6"
      (6, [ 5; 1; 2; 3; 2 ], 1)
      false;
    winning_move_test
      "->5->7->1->0->3->7->1->2->6->5->1->7->6->2->7->3->5->5 4 true grid: 8"
      (8, [ 5; 7; 1; 0; 3; 7; 1; 2; 6; 5; 1; 7; 6; 2; 7; 3; 5; 5 ], 4)
      true;
    winning_move_test
      "->3->5->0->4->2->5->4->3->0->4->4->4->2->2->0->1->3->2->2->3->1->2->3->0->4->3->5->0->0->3->1->0->4 \
       6 true grid: 7"
      ( 7,
        [
          3;
          5;
          0;
          4;
          2;
          5;
          4;
          3;
          0;
          4;
          4;
          4;
          2;
          2;
          0;
          1;
          3;
          2;
          2;
          3;
          1;
          2;
          3;
          0;
          4;
          3;
          5;
          0;
          0;
          3;
          1;
          0;
          4;
        ],
        6 )
      true;
    winning_move_test " 2 false grid: 4" (4, [], 2) false;
    winning_move_test "->0->0->6->6->5 2 false grid: 8"
      (8, [ 0; 0; 6; 6; 5 ], 2)
      false;
    winning_move_test
      "->4->7->1->5->0->6->1->5->6->1->5->4->0->6->2->2->6->3->1->7->2->5->7->4 \
       3 true grid: 8"
      ( 8,
        [
          4; 7; 1; 5; 0; 6; 1; 5; 6; 1; 5; 4; 0; 6; 2; 2; 6; 3; 1; 7; 2; 5; 7; 4;
        ],
        3 )
      true;
    winning_move_test "->4->2->2->4->3->0->3 3 false grid: 5"
      (5, [ 4; 2; 2; 4; 3; 0; 3 ], 3)
      false;
    winning_move_test "->2->5->3->6->1->1->6->0->3->0->5->0->5 0 true grid: 7"
      (7, [ 2; 5; 3; 6; 1; 1; 6; 0; 3; 0; 5; 0; 5 ], 0)
      true;
    winning_move_test "->0->2->1->0 1 false grid: 4"
      (4, [ 0; 2; 1; 0 ], 1)
      false;
    winning_move_test
      "->0->2->4->0->3->2->2->4->2->2->4->0->3->1->3->3->4->3 0 false grid: 5"
      (5, [ 0; 2; 4; 0; 3; 2; 2; 4; 2; 2; 4; 0; 3; 1; 3; 3; 4; 3 ], 0)
      false;
    winning_move_test
      "->0->5->4->0->2->5->4->5->5->5->4->2->1->1 3 true grid: 6"
      (6, [ 0; 5; 4; 0; 2; 5; 4; 5; 5; 5; 4; 2; 1; 1 ], 3)
      true;
    winning_move_test "->0->0->2->0->2->3->2->3->3->1->3->2 1 true grid: 4"
      (4, [ 0; 0; 2; 0; 2; 3; 2; 3; 3; 1; 3; 2 ], 1)
      true;
  ]

let command_tests =
  [
    valid_command_test "quit command" "    quit  " Quit_Game;
    valid_command_test "empty command" "" Empty;
    valid_command_test "placement command" "1" (Placement 1);
    valid_command_test "placement command" "jhasdf" Empty;
    start_command_test "quit start" " quit " Quit_Game;
    start_command_test "empy start" "" Empty;
    start_command_test "quit start" "8" (Num_Columns 8);
    start_command_test "quit start" "5" (Num_Columns 5);
    yes_no_test "valid yes" "y" YesAI;
    yes_no_test "valid no" "n" NoAI;
    yes_no_test "invalid no" "no" Unknown;
    yes_no_test "invalid yes" "yes" Unknown;
    yes_no_test "empty yes/no" "" Unknown;
    yes_no_test "random stuff yes/no" "yeasdfdsafs" Unknown;
    invalid_command_test "column command without an int as the object phrase"
      "column column" Misinput;
    invalid_command_test "column command without an int as the object phrase"
      "column hear hear" Misinput;
  ]

(** Runs all of the tests*)
let suite =
  "test suite for final_project"
  >::: List.flatten [ board_tests; command_tests ]

let _ = run_test_tt_main suite

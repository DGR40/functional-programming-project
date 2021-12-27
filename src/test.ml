open OUnit2
open Cards
open Node
open Playerstate

(* Our team’s approach to testing revolved around ensuring our client
   has a functioning version of Catan that is playable according to the
   rules to the game given at the beginning of each game started by our
   program. From here we implemented black box testing where we tested
   the game’s functionality without worrying about the internal
   structure of the program. We wanted to ensure that someone playing
   the game normally would not run into issues that would cause the game
   to fail and quit. We then included glass box testing where we looked
   at function branches to ensure that every option presented to the
   user is valid and correctly does what it is supposed to. Overall it
   was difficult to test Catan because of the heavy reliance on a graph
   of nodes and edges where each node referenced its neighbors
   preventing us from using subsections of a board to test. Therefore
   graph and board related features were tested manually by running the
   game and ensuring that player’s received the correct resources for
   their settlements on nodes, that settlements and cities interacted
   with board features correctly, and that other board related features
   acted appropriately. Features revolving around the state of a player
   were able to be tested through Ounit such as a player’s points
   changing. Card features were tested both manually and using Ounit.
   For example getting a “victory” development card that increases a
   player’s points by 1 was tested using Ounit whereas drawing other
   types of cards were tested manually as some of those card drawing
   functions drew random cards, making automatic testing difficult. Our
   graphics including our GUI was tested manually where we made changes
   to our code and saw how they were reflected on the GUI and went back
   to the code if we wanted to alter how the GUI looked. No module was
   fully tested by Ounit instead we decided upon a strategy where we
   tested functions that had an impact on other features of the game
   using Ounit and functions that impacted setup and behind the scenes
   aspects manually.

   We believe our program to be correct from a mixture of black box and
   glass box testing where we followed branches in glass box to ensure
   the program could not fail or go astray in places and black box where
   we ran hundreds of game simulations choosing to use different
   features at different times, typing incorrect inputs, as well as
   using real strategies to win the game. Our extensive manual testing
   and branch analysis has shown our program to be correct for many
   inputs and we believe our program to be sound and usable for our
   client. *)

(*[add_res_cards playername resource] adds a specific resource to a
  player's card hand for the purposes of testing*)
let add_res_cards (resource : card) (playername : player) =
  playername.cards <- Array.append playername.cards [| resource |];
  playername

let p1 = make_player "bot_1" Graphics.red

let can_settlement_p (p1 : player) =
  add_res_cards "brick" p1
  |> add_res_cards "stone" |> add_res_cards "wheat"
  |> add_res_cards "camel" |> add_res_cards "wood"

let city_p =
  add_res_cards "wheat" p1
  |> add_res_cards "wheat" |> add_res_cards "stone"
  |> add_res_cards "stone" |> add_res_cards "stone"

let dev_card_p =
  add_res_cards "wheat" p1
  |> add_res_cards "stone" |> add_res_cards "camel"

let trade_p =
  add_res_cards "brick" p1
  |> add_res_cards "brick" |> add_res_cards "brick"
  |> add_res_cards "brick"

let seven_p =
  add_res_cards "brick" p1
  |> add_res_cards "brick" |> add_res_cards "brick"
  |> add_res_cards "brick" |> add_res_cards "brick"
  |> add_res_cards "brick" |> add_res_cards "brick"
  |> add_res_cards "brick"

let n0 = node_maker " " [||] [] "" (0, 0)

let n1 =
  node_maker "1"
    [| (ref n0, 0); (ref n0, 0); (ref n0, 0) |]
    [ ("mountains", 10) ] "settlement" (100, 40)

let n2 =
  node_maker "hello"
    [| (ref n0, 0); (ref n0, 0); (ref n0, 0) |]
    [ ("mountains", 10); ("hills", 5) ]
    "city" (0, 0)

let test_get_id (name : string) (n : t) output : test =
  name >:: fun _ -> assert_equal output (get_id n)

let test_non_empt (name : string) (n : t) (s : string) (output : string)
    : test =
  name >:: fun _ -> assert_equal output (non_empt s n)

let test_node_pix name n exp =
  name >:: fun _ -> assert_equal exp (node_pix n)

let test_check_building name n b exp =
  name >:: fun _ -> assert_equal exp (check_building n b)

let test_change_building name n s exp output =
  change_building [| n |] 0 s;
  name >:: fun _ ->
  assert_equal output (non_empt exp n) ~printer:(fun x -> x)

let test_tile_array name n exp =
  name >:: fun _ -> assert_equal exp (get_tile_array n)

let node_test_suite =
  [
    test_get_id "test get_id on node with id" n1 "1";
    test_get_id "test get_id on node with empty id" n0 " ";
    test_get_id "test get_id on node with non-int id" n2 "hello";
    test_node_pix "test node_pix on a node with a 0 pixel" n0 (0, 0);
    test_node_pix "test node_pix on a node with a non-empty pixel" n1
      (100, 40);
    test_check_building
      "test check_building for no building on a node with no building"
      n0 "" true;
    test_check_building
      "test check_building for a builidng on a node with no building" n0
      "settlement" false;
    test_check_building
      "test check_building for no building on a node with a building" n1
      "" true;
    test_check_building
      "test check_building for a settlement on a node with a settlement"
      n1 "settlement" false;
    test_check_building
      "test check_building for a city on a node with a settlement" n1
      "city" true;
    test_check_building
      "test check_building for a city on a node with a city" n2 "city"
      false;
    test_non_empt
      "test non_empt for no building on node with no building" n0 "" "";
    test_non_empt
      "test non_empt for a building on node with no building" n0 "" "";
    test_non_empt
      "test non_empt for no building on a node with a building" n1 "" "";
    test_non_empt "test non_empt for a city on a node with a city" n2
      "city" "hello";
    test_change_building
      "change_building from an empty building to a settlement" n0
      "settlement" "" "";
    test_change_building "change_building from empty to a city" n0
      "city" "" "";
    test_tile_array "testing tile_array with empty tiles" n0 [||];
    test_tile_array "testing tile_array with one tile" n1
      [| (10, "mountains") |];
    test_tile_array "testing tile_array with multiple tiles" n2
      [| (10, "mountains"); (5, "hills") |];
  ]

let dev_card_point_test
    (name : string)
    (p : player)
    (dev_card : string)
    (expected_output : int) : test =
  name >:: fun _ ->
  dev_card_point p dev_card;
  assert_equal expected_output p.points

let p1 = make_player "bot_1" Graphics.red

let p2 = make_player "bot_2" Graphics.red

let seven_rule_test
    (name : string)
    (p : player)
    (r : int)
    (expected_output : int) : test =
  name >:: fun _ ->
  seven_rule p r;
  assert_equal expected_output (Array.length p.cards)

let p2 = seven_p

let count_cards_test
    (name : string)
    (deck : string list)
    (card : string)
    (expected_output : int) : test =
  name >:: fun _ -> assert_equal expected_output (count_cards deck card)

let update_buildings_test
    (name : string)
    (p : player)
    (f : player -> unit)
    (expected_output : int) : test =
  name >:: fun _ ->
  f p;
  assert_equal expected_output p.points

let city_player = make_player "city boy" Graphics.red

let cities_player =
  {
    settlements = 0;
    cities = 1;
    cards = [||];
    dev_cards = [||];
    points = 0;
    tile_array = [||];
    n_array = [||];
    p_name = "city girl";
    color = Graphics.blue;
  }

let settlements_player =
  {
    settlements = 1;
    cities = 0;
    cards = [||];
    dev_cards = [||];
    points = 0;
    tile_array = [||];
    n_array = [||];
    p_name = "settlement boy";
    color = Graphics.blue;
  }

let dev_player =
  {
    settlements = 1;
    cities = 0;
    cards = [||];
    dev_cards = [| "victory" |];
    points = 0;
    tile_array = [||];
    n_array = [||];
    p_name = "dev boy";
    color = Graphics.blue;
  }

let dev_player_1 =
  {
    settlements = 1;
    cities = 0;
    cards = [||];
    dev_cards = [| "victory"; "victory" |];
    points = 0;
    tile_array = [||];
    n_array = [||];
    p_name = "dev boy";
    color = Graphics.blue;
  }

let dev_player_2 =
  {
    settlements = 1;
    cities = 0;
    cards = [||];
    dev_cards = [| "victory"; "victory"; "victory" |];
    points = 0;
    tile_array = [||];
    n_array = [||];
    p_name = "dev boy";
    color = Graphics.blue;
  }

let winner =
  {
    settlements = 0;
    cities = 0;
    cards = [| "stone"; "stone"; "stone"; "wheat"; "wheat" |];
    dev_cards = [| "victory" |];
    points = 10;
    tile_array = [||];
    n_array = [||];
    p_name = "winner";
    color = Graphics.blue;
  }

let make_trader kind =
  {
    settlements = 1;
    cities = 0;
    cards = [| kind; kind; kind; kind |];
    dev_cards = [||];
    points = 0;
    tile_array = [||];
    n_array = [||];
    p_name = "trade boy";
    color = Graphics.blue;
  }

let wood_trader = make_trader "wood"

let dev_card_remove_tests
    (name : string)
    (playername : player)
    (expected_output : int) : test =
  name >:: fun _ ->
  dev_card_remove playername;
  assert_equal expected_output (Array.length playername.dev_cards)

let count_all_tests
    (name : string)
    (playername : player)
    (expected_output : int) : test =
  name >:: fun _ ->
  assert_equal expected_output (count_all_cards playername)

let end_of_game_tests (name : string) (playername : player) : test =
  name >:: fun _ ->
  assert_raises
    (Failure (playername.p_name ^ "Game Over"))
    (fun () -> end_game playername)

let not_end_of_game_tests
    (name : string)
    (playername : player)
    (expected_output : unit) : test =
  name >:: fun _ -> assert_equal expected_output ()

let player_test_suite =
  [
    dev_card_point_test "dev_card_point test on victory devcard" p1
      "victory" 1;
    dev_card_point_test "dev_card_point test on knight devcard" p2
      "knight" 0;
    dev_card_point_test "dev_card_point test on invalid devcard" p2 "1"
      0;
    count_cards_test "count_card on empty list" [] "wood" 0;
    count_cards_test "count_card on list of one wood" [ "wood" ] "wood"
      1;
    count_cards_test "count_card on list of multiple wood"
      [ "wood"; "wood"; "wood" ]
      "wood" 3;
    count_cards_test
      "count_card on list of multiple resources not matching"
      [ "wood"; "wood"; "wood" ]
      "stone" 0;
    end_of_game_tests
      "test where one player has 10 or more points and the game ends"
      winner;
    not_end_of_game_tests
      "test where no players have 10 or more points and the game \
       continues"
      cities_player ();
    count_all_tests
      "test counts the cards in a player with 0 cards\n\
      \       in their hand" p1 0;
    count_all_tests
      "tst counts the cards in a\n\
      \       player with multiple cards in their hand" wood_trader 4;
  ]

let settler =
  {
    settlements = 0;
    cities = 0;
    cards = [| "wood"; "wheat"; "brick"; "camel" |];
    dev_cards = [||];
    points = 0;
    tile_array = [||];
    n_array = [||];
    p_name = "settler boy";
    color = Graphics.blue;
  }

let citier =
  {
    settlements = 1;
    cities = 0;
    cards = [| "stone"; "stone"; "stone"; "wheat"; "wheat" |];
    dev_cards = [||];
    points = 0;
    tile_array = [||];
    n_array = [||];
    p_name = "city boy";
    color = Graphics.blue;
  }

let citier_no_settle =
  {
    settlements = 0;
    cities = 0;
    cards = [| "stone"; "stone"; "stone"; "wheat"; "wheat" |];
    dev_cards = [||];
    points = 0;
    tile_array = [||];
    n_array = [||];
    p_name = "city boy";
    color = Graphics.blue;
  }

let stone_trader = make_trader "stone"

let camel_trader = make_trader "camel"

let wheat_trader = make_trader "wheat"

let brick_trader = make_trader "brick"

let trade_options_test
    (name : string)
    (p : player)
    (expected_output : string list) : test =
  name >:: fun _ -> assert_equal expected_output (Play.trade_options p)

let can_build_test
    (name : string)
    (p : player)
    f
    (expected_output : bool) : test =
  name >:: fun _ -> assert_equal expected_output (f p)

let res_printer_test
    (name : string)
    (kind : string)
    (cards : string list)
    (expected_output : string) =
  name >:: fun _ ->
  assert_equal expected_output (Play.res_printer kind cards)

let check_test
    (name : string)
    (cards : string array)
    (card : string)
    (expected_output : bool) : test =
  name >:: fun _ -> assert_equal expected_output (Play.check cards card)

let play_test_suite =
  [
    trade_options_test
      "test trade_options when a player cannot trade anything bc they \
       have no cards"
      p1 [];
    trade_options_test "test trade_options when player has 4 wood"
      wood_trader [ "wood" ];
    trade_options_test "test trade_options when a player has 4 stone"
      stone_trader [ "stone" ];
    trade_options_test "test trade_options when a player has 4 brick"
      brick_trader [ "brick" ];
    trade_options_test "test trade_options when a player has 4 wheat"
      wheat_trader [ "wheat" ];
    trade_options_test "test trade_options when a player has 4 camel"
      camel_trader [ "camel" ];
    can_build_test
      "test can_build_settlement on player that \n\
      \    cannot build settlement" wood_trader
      Play.can_build_settlement false;
    can_build_test
      "test can_build_settlement on player that can build settlement"
      settler Play.can_build_settlement true;
    can_build_test
      "test can_build_city on player that cannot build a city bc not \
       enough resources"
      wood_trader Play.can_build_city false;
    can_build_test "test can_build_city on player that can build a city"
      citier Play.can_build_city true;
    can_build_test
      "test can_build_city on player that cannot build a city bc they \
       have \n\
      \      no settlement" citier_no_settle Play.can_build_city false;
    res_printer_test "test res printer when there is no card resource"
      "wood" [] "0 wood";
    res_printer_test
      " test res_printer when there is one card of the matching \
       resource"
      "wood" [ "wood" ] "1 wood";
    res_printer_test
      "test res_printer when there is multiple of the matching resource"
      "wood" [ "wood"; "wood" ] "2 wood";
    res_printer_test
      "test res_printer when there is multiple of non matching resource"
      "wood"
      [ "stone"; "stone"; "stone" ]
      "0 wood";
    check_test "check empty array" [||] "wood" false;
    check_test "check array of size one with wanted resource"
      [| "wood" |] "wood" true;
    check_test
      "check array of size three with one wanted resource in middle"
      [| "stone"; "wood"; "stone" |]
      "wood" true;
    check_test "check array of size 5 with one wanted resource at end"
      [| "stone"; "stone"; "stone"; "stone"; "wood" |]
      "wood" true;
  ]

let suite =
  "test suite for final project"
  >::: List.flatten
         [ node_test_suite; player_test_suite; play_test_suite ]

let _ = run_test_tt_main suite

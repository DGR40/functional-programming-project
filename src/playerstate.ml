open Random
open Unit

type player = {
  mutable settlements : int;
  mutable cities : int;
  mutable cards : string array;
  mutable dev_cards : string array;
  mutable points : int;
  mutable tile_array : (int * Node.terrain_id) array;
  mutable n_array : Node.t array;
  p_name : string;
  color : Graphics.color;
}

type knight = { mutable on_node : Node.t }

let color p = p.color

(** [make_player playername] creates a new player named [playername] of
    type player. *)
let make_player (playername : string) color : player =
  {
    settlements = 0;
    cities = 0;
    cards = [||];
    dev_cards = [||];
    points = 0;
    tile_array = [||];
    n_array = [||];
    p_name = playername;
    color;
  }

(** [count cards] is the number of cards of a given type in a given deck*)
let rec count_cards (deck : string list) (card : string) : int =
  match deck with
  | [] -> 0
  | h :: t ->
      if h = card then 1 + count_cards t card else count_cards t card

(*[count_all_cards playername] counts all the resource cards in a
  player's hand*)
let count_all_cards (playername : player) =
  let card_lst = Array.to_list playername.cards in
  count_cards card_lst "wood"
  + count_cards card_lst "brick"
  + count_cards card_lst "wheat"
  + count_cards card_lst "camel"
  + count_cards card_lst "stone"

(* [add_to_tile_array playername node] adds a node to a player's
   tile_array when player builds a settlment/city*)
let add_to_tile_array
    (playername : player)
    (node : (int * Node.terrain_id) array)
    (n : Node.t) : unit =
  playername.tile_array <- Array.append node playername.tile_array;
  playername.n_array <- Array.append [| n |] playername.n_array

(*[pp_helper2 e acc] is a helper function to [pp_array2 arr] and needed
  to pretty print an array*)
let pp_helper2 e acc = e ^ acc

(** pp_array [arr] pretty prints an array *)
let rec pp_array2 (arr : string array) =
  print_string (Array.fold_left pp_helper2 "" arr)

(*[remove_n_from_lst c_list n] removes [n] elements from the fron of
  list [c_list]*)
let rec remove_n_from_lst (c_list : string list) (n : int) =
  match n with
  | 0 ->
      pp_array2 (Array.of_list c_list);
      Array.of_list c_list
  | _ -> remove_n_from_lst (List.tl c_list) (n - 1)

(*[seven_rule playername roll] implements a game feature where if you
  have more than 7 cards in your and roll a 7 half your cards must be
  removed. The cards removed is rounded up if you have an odd number of
  cards*)
let seven_rule (playername : player) (roll : int) =
  let num_cards = count_all_cards playername in
  let c_lst = Array.to_list playername.cards in
  ANSITerminal.print_string [ ANSITerminal.green ]
    (string_of_int num_cards);
  if num_cards > 7 then (
    let num_cards_to_remove =
      if num_cards mod 2 = 0 then num_cards / 2 else (num_cards + 1) / 2
    in
    let new_cards_arr = remove_n_from_lst c_lst num_cards_to_remove in
    pp_array2 new_cards_arr;
    playername.cards <- new_cards_arr;
    ANSITerminal.print_string [ ANSITerminal.green ]
      "Oh no you have more\n\
      \  than 7 cards in your hand and rolled a 7! The  robber has \
       just stolen\n\
      \  half your cards! Be careful next time!")

(* This function is used when a player builds a settlement which
   increases a player's settlements by 1*)
let update_settlements (player : player) : unit =
  player.settlements <- 1 + player.settlements;
  player.points <- player.points + 1

(* This function is used when a player upgrades a settlement to a city.
   Therfore the number of settlements must be decreased by 1 and the
   number of cities must be increased by 1. *)
let update_cities (player : player) : unit =
  player.cities <- player.cities + 1;
  player.settlements <- player.settlements - 1;
  player.points <- player.points + 1

(*Helper function that draws a random card from a deck *)
let random_card (deck_array : string array) =
  Random.self_init ();
  let x = Random.int (Array.length deck_array) in
  deck_array.(x)

(*Helper fucntion that checks to see if a dev card is a victory point
  and if so updates the player's points *)
let dev_card_point (player : player) (dev_card : string) : unit =
  if String.equal dev_card "victory" then
    player.points <- player.points + 1

(*[dev_card_remove player] removes the resource cards traded in for a
  dev card*)
let dev_card_remove (playername : player) =
  let c_list = Array.to_list playername.cards in
  let camel_lst = List.filter (fun x -> x = "camel") c_list in
  let wheat_lst = List.filter (fun x -> x = "wheat") c_list in
  let stone_lst = List.filter (fun x -> x = "stone") c_list in
  let brick_lst = List.filter (fun x -> x = "brick") c_list in
  let wood_lst = List.filter (fun x -> x = "wood") c_list in
  let new_camel_lst = List.tl camel_lst in
  let new_wheat_lst = List.tl wheat_lst in
  let new_stone_lst = List.tl stone_lst in
  let new_c_list =
    List.append new_camel_lst new_wheat_lst
    |> List.append new_stone_lst
    |> List.append brick_lst |> List.append wood_lst
  in
  let new_c_arr = Array.of_list new_c_list in
  playername.cards <- new_c_arr

(* [draw_dev_card player deck_array] is a function that draws a dev card
   from the dev card deck, adds the card to the player's dev card list,
   and updates the player's points if it is a victory points*)
let draw_dev_card (player : player) (deck_array : string array) =
  let dev_card = random_card deck_array in
  dev_card_point player dev_card;
  player.dev_cards <- Array.append [| dev_card |] player.dev_cards;
  dev_card_remove player;
  dev_card

(*[buy_dev_shelper playername] returns true if player has at least 1
  stone which is a resource necessary for buying a dev card*)
let buy_dev_shelper (playername : player) =
  count_cards (Array.to_list playername.cards) "stone" >= 1

(*[buy_dev_chelper playername] returns true if player has at least 1
  camel which is a resource necessary for buying a dev card*)
let buy_dev_chelper (playername : player) =
  count_cards (Array.to_list playername.cards) "camel" >= 1

(*[buy_dev_whelper playername] returns true if player has at least 1
  wheat which is a resource necessary for buying a dev card*)
let buy_dev_whelper (playername : player) =
  count_cards (Array.to_list playername.cards) "wheat" >= 1

(*[can_buy_devcard playername] returns a bool dependign on whther a
  player has the resources needed ot buy a dev card or not*)
let can_buy_devcard (playername : player) : bool =
  buy_dev_shelper playername
  && buy_dev_chelper playername
  && buy_dev_whelper playername

(* Function to be called every time a player s turn is over that checks
   to see whether that player won and if so a win message is displayed
   and the game ends*)
let end_game (player : player) : unit =
  if player.points >= 10 then failwith (player.p_name ^ "Game Over")

(* [draw_card_helper num] returns the terrain the player owns if the
   dice roll matches that of the terrain's tile number*)
let draw_card_helper (num : int) tile =
  match tile with f, terr -> if f = num then terr else ""

(* [draw_card playername num_rolled] returns array of all cards the
   player will get with given roll *)
let draw_card (playername : player) (num_rolled : int) =
  Array.map (draw_card_helper num_rolled) playername.tile_array

(* [get_cards_helper] is the string representation of the resource card
   that corresponds with a given terrain *)
let get_cards_helper = function
  | "fields" -> "wheat"
  | "forest" -> "wood"
  | "hills" -> "brick"
  | "pasture" -> "camel"
  | "mountains" -> "stone"
  | _ -> ""

(* [get_cards card_array ] is the string array of each card the player
   gets from the dice roll*)
let get_cards (card_array : Node.terrain_id array) : string array =
  Array.map get_cards_helper card_array

(* [add_cards playername card_array] adds the new cards a player draws
   from rolling into their card hand *)
let add_cards (playername : player) (card_array : string array) : unit =
  playername.cards <- Array.append playername.cards card_array

(*[move_knight playername node die_roll knight] is a function that moves
  that allows a player to move the knight when a 7 is rolled*)
let move_knight
    (playername : player)
    (node : Node.t)
    (die_roll : int)
    (knight : knight) =
  if die_roll = 7 then knight.on_node <- node else ()

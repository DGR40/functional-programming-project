open Node
open Cards
open Playerstate

(** [file_list f] creates a list of each line in file [f]. *)
let file_list f =
  let l = ref [] in
  let c = open_in f in
  try
    while true do
      l := input_line c :: !l
    done;
    !l
  with End_of_file ->
    close_in c;
    List.rev !l

(** [add_graph_settle s p i] puts a building of type [s] in on the graph
    at node [i] in the color of player [p]. *)
let add_graph_settle s p i : unit =
  let n = Board.node_array.(i - 1) in
  match Node.node_pix n with
  | x, y -> Graph.put_settlement x y s (Playerstate.color p)

(** [set_settle i build] places a building [build] on the board at node
    number [i]. *)
let set_settle i build p : unit =
  change_building Board.node_array (i - 1) build;
  add_graph_settle build p i;
  if build = "settlement" then
    Playerstate.add_to_tile_array p
      (get_tile_array Board.node_array.(i - 1))
      Board.node_array.(i - 1)

(** [pp_node s] pretty prints a node [s] if it exists. *)
let pp_node s =
  if s = "" then print_string "" else print_string (s ^ ", ")

(** [pp_helper e acc] is the pretty printed string of acc with e
    appended to it *)
let pp_helper e acc = e ^ acc

(** pp_array [arr] is the string version *)
let rec pp_array (arr : string array) =
  print_string (Array.fold_left pp_helper "" arr)

(** [pp_string s] pretty-prints string [s]. *)
let pp_string s = "\"" ^ s ^ "\""

(** [pp_list pp_elt lst] pretty-prints list [lst], using [pp_elt] to
    pretty-print each element of [lst]. *)
let pp_list pp_elt lst =
  let pp_elts lst =
    let rec loop n acc = function
      | [] -> acc
      | [ h ] -> acc ^ pp_elt h
      | h1 :: (h2 :: t as t') ->
          if n = 100 then acc ^ "..." (* stop printing long list *)
          else loop (n + 1) (acc ^ pp_elt h1 ^ "; ") t'
    in
    loop 0 "" lst
  in
  "[" ^ pp_elts lst ^ "]"

(** [build_opt] prints a list of available nodes to the terminal. *)
let build_opt f (a : Node.t array) =
  let arr = Array.map f a in
  Array.iter pp_node arr

(** [input_settle] checks for correct input then converts the input to a
    correspond to a node. *)

let rec input_settle () =
  print_string ">>";
  let x = read_line () in
  try int_of_string x
  with Failure _ ->
    print_string "not an int";
    input_settle ()

(** [prompt s] creates a new player in an initial state and changes the
    board according to the terminal input. *)
let prompt b p =
  ANSITerminal.print_string [ ANSITerminal.blue ]
    ("You can build a nice " ^ b ^ " right on Nodes: \n");
  let s, a =
    if b = "settlement" then ("", Board.node_array)
    else ("settlement", p.n_array)
  in
  build_opt (Node.non_empt s) a;
  print_string "\n";
  let i = input_settle () in
  set_settle i b p

(** [script st fi lst] prints a section the file list [lst] from line
    [st] to line [fi] to the terminal. *)
let script st fi lst =
  for i = st to fi do
    ANSITerminal.print_string [ ANSITerminal.blue ]
      (List.nth lst i ^ "\n")
  done

(** [count deck card] returns the number of cards of a given type in a
    given deck*)
let rec count (deck : string list) (card : string) : int =
  match deck with
  | [] -> 0
  | h :: t -> if h = card then 1 + count t card else count t card

(*[check cards card] checks to see if card exists in cards and returns a
  bool*)
let check cards card = Array.exists (fun x -> x = card) cards

(*[trade_card_remove playername resource] deletes 4 resource cards from
  the player's hand because of a trade*)
let trade_card_remove
    (playername : Playerstate.player)
    (resource : Cards.card) : unit =
  let c_list = Array.to_list playername.cards in
  let res_list = List.filter (fun x -> x = resource) c_list in
  let non_res_list = List.filter (fun x -> x <> resource) c_list in
  let res_list1 = List.tl res_list in
  let res_list2 = List.tl res_list1 in
  let res_list3 = List.tl res_list2 in
  let res_list4 = List.tl res_list3 in
  let res_arr = Array.of_list res_list4 in
  playername.cards <- Array.append res_arr (Array.of_list non_res_list)

(*[wood_pos_trade playername opt_list] adds wood to opt_list which is
  the list of possible trades*)
let wood_pos_trade
    (playername : Playerstate.player)
    (opt_list : string list) =
  let c_list = Array.to_list playername.cards in
  let w_count = count c_list "wood" in
  if w_count >= 4 then "wood" :: opt_list else opt_list

(*[stone_pos_trade playername opt_list] adds stone to opt_list which is
  the list of possible trades*)
let stone_pos_trade
    (playername : Playerstate.player)
    (opt_list : string list) =
  let c_list = Array.to_list playername.cards in
  let s_count = count c_list "stone" in
  if s_count >= 4 then "stone" :: opt_list else opt_list

(*[camel_pos_trade playername opt_list] adds camel to opt_list which is
  the list of possible trades*)
let camel_pos_trade
    (playername : Playerstate.player)
    (opt_list : string list) =
  let c_list = Array.to_list playername.cards in
  let c_count = count c_list "camel" in
  if c_count >= 4 then "camel" :: opt_list else opt_list

(*[wheat_pos_trade playername opt_list] adds wheat to opt_list which is
  the list of possible trades*)
let wheat_pos_trade
    (playername : Playerstate.player)
    (opt_list : string list) =
  let c_list = Array.to_list playername.cards in
  let w_count = count c_list "wheat" in
  if w_count >= 4 then "wheat" :: opt_list else opt_list

(*[brick_pos_trade playername opt_list] adds wheat to opt_list which is
  the list of possible trades*)
let brick_pos_trade
    (playername : Playerstate.player)
    (opt_list : string list) =
  let c_list = Array.to_list playername.cards in
  let w_count = count c_list "brick" in
  if w_count >= 4 then "brick" :: opt_list else opt_list

(*[can_make_trade playername] determines if a player can make a trade or
  not and returns a bool*)
let can_make_trade (playername : Playerstate.player) =
  let c_list = Array.to_list playername.cards in
  count c_list "wood" >= 4
  || count c_list "stone" >= 4
  || count c_list "camel" >= 4
  || count c_list "wheat" >= 4
  || count c_list "brick" >= 4

(*[trade_options playername] returns all the possible resources that the
  player can trade in*)
let trade_options (playername : Playerstate.player) =
  let trade_opts = [] in
  wood_pos_trade playername trade_opts
  |> stone_pos_trade playername
  |> camel_pos_trade playername
  |> wheat_pos_trade playername
  |> brick_pos_trade playername

(*[trade_card_add playername resource] adds a card to the player's card
  hand because of the trade*)
let trade_card_add
    (playername : Playerstate.player)
    (resource : Cards.card) =
  playername.cards <- Array.append [| resource |] playername.cards

(* [trade_execute playername res_given res_taken] trades in 4
   cards(res_given) for 1 card(res_taken) and reflects this change in
   the player's card hand*)
let trade_execute
    (playername : Playerstate.player)
    (res_given : Cards.card)
    (res_taken : Cards.card) : unit =
  trade_card_remove playername res_given;
  trade_card_add playername res_taken

(* [input_trade_in playername] asks the player what resource they want
   to trade in and ensures the player chooses a viable option*)
let input_trade_in (playername : Playerstate.player) =
  let x = read_line () in
  if List.exists (fun y -> y = x) (trade_options playername) then
    ANSITerminal.print_string [ ANSITerminal.green ]
      "What resource would you like to recieve"
  else
    ANSITerminal.print_string [ ANSITerminal.green ]
      "You cannot trade that resource. Please choose another."

(**[can_build_settlement playername] is the function that determines
   whether a player on their given turn has the resources needed to
   build a settlement *)
let can_build_settlement (playername : Playerstate.player) : bool =
  let cards = playername.cards in
  check cards "wood" && check cards "wheat" && check cards "brick"
  && check cards "camel"

(*[can_build_city playername] is the function that determines whether a
  player on their given turn has the resources needed to build a city*)
let can_build_city (playername : Playerstate.player) : bool =
  if playername.settlements >= 1 then
    count (Array.to_list playername.cards) "stone" >= 3
    && count (Array.to_list playername.cards) "wheat" >= 2
  else false

(*[res_printer kind] is the string of the amount a player has of a
  single [kind] resource *)
let res_printer (kind : string) cards : string =
  string_of_int (count cards kind) ^ " " ^ kind

(*[new_res_printer kind] is the string of the amount of a new resource a
  player has just gotten from his/her roll *)
let new_res_printer (kind : string) cards emoji : string =
  let card_count = count cards kind in
  if card_count > 0 then
    string_of_int (count cards kind) ^ " " ^ kind ^ emoji ^ " | "
  else ""

(*[card_printer cards] prints the amount of each resource a player has *)
let card_printer cards : unit =
  let print =
    res_printer "stone" cards
    ^ " 🪨" ^ " | "
    ^ res_printer "wood" cards
    ^ " 🪵" ^ " | "
    ^ res_printer "brick" cards
    ^ " 🧱" ^ " | "
    ^ res_printer "wheat" cards
    ^ " 🌾" ^ " | "
    ^ res_printer "camel" cards
    ^ " 🐫" ^ "| \n\n"
  in
  ANSITerminal.print_string [ ANSITerminal.white ] print

(*[new_card_printer cards] prints the amount of each new resource a
  player has just gained from rolling *)
let new_card_printer cards : unit =
  let print =
    new_res_printer "stone" cards " 🪨"
    ^ new_res_printer "wood" cards " 🪵"
    ^ new_res_printer "brick" cards " 🧱"
    ^ new_res_printer "wheat" cards " 🌾"
    ^ new_res_printer "camel" cards " 🐫"
    ^ "\n\n"
  in
  ANSITerminal.print_string [ ANSITerminal.white ] print

(** [move_options p i] are the list of possible moves which the player
    can do depending on the current state. *)
let rec move_options p i =
  let opt = ref 0 in
  ANSITerminal.print_string [ ANSITerminal.green ]
    "\n\nNew resources gained: \n";
  new_card_printer
    (Array.to_list (Playerstate.get_cards (Playerstate.draw_card p i)));
  Playerstate.add_cards p
    (Playerstate.get_cards (Playerstate.draw_card p i));
  ANSITerminal.print_string [ ANSITerminal.green ]
    "\n\nCurrently have: \n";
  card_printer (Array.to_list p.cards);
  if can_build_settlement p then (
    ANSITerminal.print_string [ ANSITerminal.green ]
      "You can build a nice settlement right on these nodes:\n";
    build_opt (Node.non_empt "") Board.node_array;
    ANSITerminal.print_string [ ANSITerminal.green ]
      "\nType 'settlement' to build.\n\n")
  else opt := 1;
  if can_build_city p then (
    ANSITerminal.print_string [ ANSITerminal.green ]
      "You can build a nice city right on these nodes:\n";
    build_opt (Node.non_empt "settlement") p.n_array;
    ANSITerminal.print_string [ ANSITerminal.green ]
      "\nType 'city' to build. \n\n")
  else opt := 2;
  if can_make_trade p then
    ANSITerminal.print_string [ ANSITerminal.green ]
      "\n\n\
       You can trade in four of one resource for a resource of your \
       choice! \n\n\
      \      Type 'trade' to trade.\n"
  else opt := 3;
  if can_buy_devcard p then
    ANSITerminal.print_string [ ANSITerminal.green ]
      "\n\n\
       You can trade in four-of-a-kind for a development card! \n\
       Type 'dev' to trade.\n"
  else opt := 4;
  if !opt = 0 then
    ANSITerminal.print_string [ ANSITerminal.green ]
      "\n\nWhat would you like to do next?\n\n"
  else
    ANSITerminal.print_string [ ANSITerminal.green ]
      "\n\nPress enter to move on. \n\n";
  ANSITerminal.print_string [ ANSITerminal.green ]
    "\nType 'stop' to exit the game. ";
  print_string "\n>> ";
  let x = read_line () in
  move_action p i x

(** [move_action p i x] gives the player [p] options depending on the
    input [x]. *)
and move_action p i = function
  | ("settlement" | "city") as s ->
      if s = "city" then Playerstate.update_cities p;
      ANSITerminal.print_string [ ANSITerminal.green ] "Which node?";
      let x = input_settle () in
      set_settle x s p;
      ANSITerminal.print_string [ ANSITerminal.blue ]
        (s ^ " built on node " ^ string_of_int x ^ "!");
      print_string "\n\n\n"
  | "trade" -> trades p i
  | "dev" ->
      let dev_str =
        "You got a " ^ draw_dev_card p make_dev_deck ^ "\n"
      in
      ANSITerminal.print_string [ ANSITerminal.green ] dev_str
  | "stop" -> failwith "End of Game"
  | "" -> ()
  | _ ->
      print_string "That was not one of the options\n";
      move_options p i

(*[trades p i] outputs the resource options that a player can trade in
  and then asks and reads the resource option which the player chooses
  to recieve*)
and trades p i =
  ANSITerminal.print_string [ ANSITerminal.green ]
    ("You can trade "
    ^ pp_list pp_string (trade_options p)
    ^ "\nType the resource you wish to give away!\n>>");
  let x1 = read_line () in
  ANSITerminal.print_string [ ANSITerminal.green ]
    "\n\
     Type the resource you wish to receive!\n\
     [stone, camel, wood, brick, wheat]\n\
     >>";
  let x2 = read_line () in
  trade_opt p i x1 x2;
  move_options p i

(*[trade_opt p i give recieve] executes the trade according to what the
  player wants to give and recieve as well as updates features of a
  player to reflect this change*)
and trade_opt p i give receive =
  trade_card_remove p give;
  trade_card_add p receive;
  ANSITerminal.print_string [ ANSITerminal.green ]
    "\nYou've made the trade!\n\n";
  move_options p i

(** [ask lst] presents the introduction of the game from the text file
    list [lst] and prompts the user ot start the game. *)
let rec ask lst p =
  print_string "> ";
  let x = read_line () in
  match x with
  | "a" ->
      script 7 16 lst;
      ask lst p
  | "b" ->
      script 19 23 lst;
      ask lst p
  | "c" ->
      script 26 34 lst;
      ask lst p
  | "d" ->
      script 38 42 lst;
      prompt "settlement" p;
      script 43 48 lst;
      prompt "settlement" p
  | "die" ->
      script 51 56 lst;
      ask lst p
  | "quit" ->
      print_string "\n";
      ask lst p
  | _ ->
      ANSITerminal.print_string [ ANSITerminal.blue ]
        "That was not one of the options\n\n";
      ask lst p

let continue = true

let turn_counter = ref 0

(*[choose_player coutn p1 p2] manages which player's turn it is and
  alternates to the next player once a player has ended his/her turn*)
let choose_player (count : int) (p1 : player) (p2 : player) : player =
  if count mod 2 = 0 then p1 else p2

(** [run_play cont] runs the gameplay until the game is over. *)
let rec run_play cont p1 p2 =
  while cont do
    print_string
      "🐫 🐫 🐫 🐫 🐫 🐫 🐫 🐫 🐫 🐫 🐫 🐫 \
       🐫 🐫 🐫 🐫 🐫 🐫 🐫 🐫 🐫 🐫 🐫 🐫 \
       🐫 🐫 🐫 🐫 🐫 🐫 \n";
    let p = choose_player !turn_counter p1 p2 in
    ANSITerminal.print_string [ ANSITerminal.green ]
      ("\n\nHello " ^ p.p_name ^ ".\n" ^ "You have "
     ^ string_of_int p.points ^ " points.\n"
     ^ " It is your turn.\nRolling die.....\n\n\n");

    let die_roll =
      Random.self_init ();
      1 + Random.int 6 + (1 + Random.int 6)
    in
    ANSITerminal.print_string [ ANSITerminal.green ]
      ("You rolled: " ^ string_of_int die_roll ^ "\n");
    Graph.make_points p true;
    turn_counter := !turn_counter + 1;
    move_options p die_roll
  done

(* [prompt_play] runs the beggining of the game where players input
   his/her names which they will be referred to as for the rest of the
   game *)
let prompt_play () =
  ANSITerminal.print_string [ ANSITerminal.red ]
    "Player 1, what is your name?\n";
  print_string ">>";
  let x1 = read_line () in
  let p1 = Playerstate.make_player x1 Graphics.red in
  ANSITerminal.print_string [ ANSITerminal.blue ]
    "Player 2, what is your name?\n";
  print_string ">>";
  let x2 = read_line () in
  let p2 = Playerstate.make_player x2 Graphics.blue in
  (p1, p2)

(* [main] runs the game from the most basic level as well as outputs the
   instructions of the game*)
let main () =
  Graph.main ();
  let p1, p2 = prompt_play () in
  let lst = file_list "instructions.txt" in
  script 0 4 lst;
  ask lst p1;
  ANSITerminal.print_string [ ANSITerminal.blue ]
    "Pass the screen to the next player.\n\n\n";
  script 0 4 lst;
  ask lst p2;
  ANSITerminal.print_string [ ANSITerminal.blue ] "Ready to play!\n\n\n";
  run_play true p1 p2

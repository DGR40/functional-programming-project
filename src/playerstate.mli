(** Player is the player of Settlers of OCaml in the terminal and their related functionality. *)

(** Type for the player of the game. *)
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

(** [color] takes in a player and returns the color of the player*)
val color : player -> Graphics.color


(** [make_player playername] creates a new player named [playername] of
    type player. *)
val make_player : string -> Graphics.color -> player
(** [add_to_tile_array playername node] adds a node to a player's
   tile_array when player builds a settlment/city*)
val add_to_tile_array :
  player -> (int * Node.terrain_id) array -> Node.t -> unit

(** [update_cities p] is the number of settlements when a player [p] upgrades a settlement to a city. *)
val update_cities : player -> unit

(** [count cards] is the number of cards of a given type in a given deck*)
val count_cards : string list -> string -> int

val seven_rule : player -> int -> unit

(** [update settlements] is used when a player builds a settlement which
   increases a player's settlements by 1*)
val update_settlements : player -> unit
(** [random_card arr] draws a random card from a deck [arr]. *)
val random_card : string array -> string
(** [dev_card_point] checks to see if a dev card is a victory point
  and if so updates the player's points. *)
val dev_card_point : player -> string -> unit
(**[dev_card_remove player] removes the resource cards traded in for a
  dev card*)
val dev_card_remove : player -> unit
(** [draw_dev_card player deck_array] is a function that draws a dev card
   from the dev card deck, adds the card to the player's dev card list,
   and updates the player's points if it is a victory points*)
val draw_dev_card : player -> string array -> string
   (***[can_buy_devcard playername] returns a bool dependign on whther a
   player has the resources needed ot buy a dev card or not*)
val can_buy_devcard : player -> bool

(** [draw_card_helper num] returns the terrain the player owns if the
   dice roll matches that of the terrain's tile number*)
val draw_card : player -> int -> Node.terrain_id array

(** [get_cards card_array ] is the string array of each card the player
   gets from the dice roll*)
val get_cards : Node.terrain_id array -> string array

(** [add_cards playername card_array] adds the new cards a player draws
   from rolling into their card hand *)
val add_cards : player -> string array -> unit

(**[count_all_cards playername] counts all the resource cards in a
  player's hand*)
val count_all_cards : player -> int

(** [end_game] called every time a players turn is over that checks
   to see whether that player won and if so a win message is displayed
   and the game ends*)
val end_game : player -> unit

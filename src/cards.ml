open Array

(* card is a string representing a resource card *)
type card = string

(* dev_card is a string representing a development card *)
type dev_card = string

(**Generates a deck of dev cards comprised of 3 different types of
   development cards: victory, knight, and dev*)
let make_dev_deck : dev_card array =
  [|
    "victory";
    "victory";
    "victory";
    "victory";
    "victory";
    "knight";
    "knight";
    "knight";
    "knight";
    "knight";
    "knight";
    "knight";
    "knight";
    "knight";
    "knight";
    "knight";
    "knight";
    "knight";
    "knight";
    "dev";
    "dev";
    "dev";
    "dev";
    "dev";
    "dev";
  |]

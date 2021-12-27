(** Cards defines the two different types of cards and a function which
    creates a deck. *)

(** [card] is a regular terrain card. *)
type card = string

(** [dev_card] is a development card. *)
type dev_card = string

(** [make_dev_deck] is the deck of development cards. *)
val make_dev_deck : dev_card array

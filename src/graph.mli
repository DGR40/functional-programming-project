(** Graph creates the GUI for the game. *)
val main : unit -> unit

(** [put_settlement x y str c] places a building [str] at point ([x],
    [y]) in the player's color [c]. *)
val put_settlement : int -> int -> string -> Graphics.color -> unit

(** [make_points p cont] adds the points for player [p] to the graphic
    when [cont] is true. *)
val make_points : Playerstate.player -> bool -> unit

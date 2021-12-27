open Graphics

let forest = rgb 9 92 26

let mountains = rgb 172 185 194

let pasture = rgb 149 189 119

let hills = rgb 217 154 78

let fields = rgb 219 190 110

let desert = rgb 232 215 169

(** [colors_board_1] is the list of the tile types for board.ml. *)
let colors_board_1 =
  [
    mountains;
    pasture;
    forest;
    fields;
    hills;
    pasture;
    hills;
    fields;
    forest;
    desert;
    forest;
    mountains;
    forest;
    mountains;
    fields;
    pasture;
    hills;
    fields;
    pasture;
  ]

(** [colors_board_2] is the list of the tile types for board2.ml. *)
let colors_board_2 =
  [
    forest;
    fields;
    mountains;
    hills;
    pasture;
    hills;
    mountains;
    mountains;
    pasture;
    desert;
    fields;
    pasture;
    hills;
    forest;
    fields;
    forest;
    fields;
    forest;
    pasture;
  ]

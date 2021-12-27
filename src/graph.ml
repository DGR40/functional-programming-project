open Graphics
open Playerstate
open Colors
open Board
open Board2

(** [board] is a list of the hexagon points for all 19 tiles of the
    board. *)
let board =
  [
    [|
      (80, 310);
      (115, 330);
      (150, 310);
      (150, 270);
      (115, 250);
      (80, 270);
    |];
    [|
      (150, 310);
      (185, 330);
      (220, 310);
      (220, 270);
      (185, 250);
      (150, 270);
    |];
    [|
      (220, 310);
      (255, 330);
      (290, 310);
      (290, 270);
      (255, 250);
      (220, 270);
    |];
    [|
      (45, 250); (80, 270); (115, 250); (115, 210); (80, 190); (45, 210);
    |];
    [|
      (115, 250);
      (150, 270);
      (185, 250);
      (185, 210);
      (150, 190);
      (115, 210);
    |];
    [|
      (185, 250);
      (220, 270);
      (255, 250);
      (255, 210);
      (220, 190);
      (185, 210);
    |];
    [|
      (255, 250);
      (290, 270);
      (325, 250);
      (325, 210);
      (290, 190);
      (255, 210);
    |];
    [|
      (10, 150); (45, 130); (80, 150); (80, 190); (45, 210); (10, 190);
    |];
    [|
      (80, 150);
      (115, 130);
      (150, 150);
      (150, 190);
      (115, 210);
      (80, 190);
    |];
    [|
      (150, 190);
      (185, 210);
      (220, 190);
      (220, 150);
      (185, 130);
      (150, 150);
    |];
    [|
      (220, 150);
      (255, 130);
      (290, 150);
      (290, 190);
      (255, 210);
      (220, 190);
    |];
    [|
      (290, 150);
      (325, 130);
      (360, 150);
      (360, 190);
      (325, 210);
      (290, 190);
    |];
    [|
      (45, 90); (80, 70); (115, 90); (115, 130); (80, 150); (45, 130);
    |];
    [|
      (115, 90); (150, 70); (185, 90); (185, 130); (150, 150); (115, 130);
    |];
    [|
      (185, 90); (220, 70); (255, 90); (255, 130); (220, 150); (185, 130);
    |];
    [|
      (255, 90); (290, 70); (325, 90); (325, 130); (290, 150); (255, 130);
    |];
    [| (80, 30); (115, 10); (150, 30); (150, 70); (115, 90); (80, 70) |];
    [|
      (150, 30); (185, 10); (220, 30); (220, 70); (185, 90); (150, 70);
    |];
    [|
      (220, 30); (255, 10); (290, 30); (290, 70); (255, 90); (220, 70);
    |];
  ]

(** [node_nums] is the numbers of the nodes on the corners of the tiles. *)
let node_nums =
  [
    [| "1"; "2"; "3"; "11"; "10"; "9" |];
    [| ""; "4"; "5"; "13"; "12"; "" |];
    [| ""; "6"; "7"; "15"; "14"; "13" |];
    [| "8"; ""; ""; "20"; "19"; "18" |];
    [| ""; ""; ""; ""; ""; "" |];
    [| ""; ""; ""; "24"; ""; "" |];
    [| ""; ""; "16"; "26"; "25"; "" |];
    [| "28"; ""; ""; ""; ""; "17" |];
    [| ""; ""; ""; ""; ""; "" |];
    [| "21"; "22"; "23"; ""; "33"; "32" |];
    [| "34"; "35"; "36"; ""; ""; "" |];
    [| ""; "37"; "38"; "27"; ""; "" |];
    [| "39"; "40"; "41"; "31"; "30"; "29" |];
    [| ""; "42"; "43"; ""; ""; "" |];
    [| ""; "44"; "45"; ""; ""; "" |];
    [| ""; "46"; "47"; ""; ""; "" |];
    [| "48"; "49"; "50"; ""; ""; "" |];
    [| ""; "51"; "52"; ""; ""; "" |];
    [| ""; "53"; "54"; ""; ""; "" |];
  ]

(** [legend_space] is the points and labels for the legend on the
    graphic. *)
let legend_space =
  [
    (410, 320, ": stone", mountains);
    (410, 280, ": forest", forest);
    (410, 240, ": pasture", pasture);
    (410, 200, ": hills", hills);
    (410, 160, ": fields", fields);
    (410, 120, ": desert", desert);
  ]

(** [write n p] adds the die number [s] to the point [p]: (x, y) on the
    given node [n]. *)
let write n p =
  match (n, p) with
  | s, (x, y) ->
      moveto x y;
      draw_string s

(** [node_num poly n] writes the node numbers of the list [n] on the
    tiles of list [poly] *)
let node_num poly n = Array.iter2 write n poly

(** [add_num n] moves to the center of the tile [n] and adds the die
    number to the center of it. *)
let add_num = function
  | x, y, n ->
      moveto x y;
      draw_string n

(** [write_legend l] is the addition of the legend onto the graphic
    given the circle color and label. *)
let write_legend = function
  | x, y, n, c ->
      draw_circle x y 10;
      set_color c;
      fill_circle x y 10;
      moveto (x + 15) y;
      draw_string n

(** [put_settlement x y b c] adds the building [b] at point ([x], [y])
    in the player's color [c]. *)
let put_settlement x y (b : string) c : unit =
  match b with
  | "city" ->
      set_color c;
      draw_circle x y 5;
      fill_circle x y 5;
      (*set_color desert;*)
      draw_circle x y 2;
      fill_circle x y 2
  | "settlement" ->
      set_color c;
      draw_circle x y 5;
      fill_circle x y 5
  | _ -> failwith "impossible"

(** [move_player p] is the y value of the points location depending on
    the player [p]. *)
let move_player (p : player) = if p.color = blue then 20 else 40

(* make_points creates and updates the score for a player on the graph
   of the board *)
let make_points (p : player) (update : bool) =
  if update then (
    set_color white;
    fill_rect 405 (move_player p) 30 10;
    set_color black;
    moveto 405 (move_player p);
    draw_string (p.p_name ^ "'s points: " ^ string_of_int p.points))
  else set_color black;
  moveto 405 (move_player p);
  draw_string (p.p_name ^ "'s points: " ^ string_of_int p.points)

(** [fill p c] draws the shape [poly] and colors it [c]. *)
let fill poly col =
  set_color white;
  draw_poly poly;
  set_color col;
  fill_poly poly

(** [board_num] returns a random choice of boards*)
let board_num =
  Random.self_init ();
  Random.int 2

(** [colors_choice] returns the list of colors for each tile depending
    on the choice of board in [board_num]*)
let colors_choice i = if i = 0 then colors_board_1 else colors_board_2

(** [center_nums_choice] returns the list of center die numbers
    depending on the choice of board in [board_num]*)
let center_nums_choice i =
  if i = 0 then center_nums_board_1 else center_nums_board_2

(** [draw] is the drawing of all the features in the graphics. *)
let draw () =
  List.iter2 fill board (colors_choice board_num);
  set_color white;
  List.iter add_num (center_nums_choice board_num);
  set_color black;
  List.iter2 node_num board node_nums;
  List.iter write_legend legend_space

(** [main] creates the graph and sets the title of the graph, while
    running the function which draws the graph*)
let main () =
  open_graph " 600x400";
  set_window_title "Settlers of OCaml";
  draw ()

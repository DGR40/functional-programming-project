open Node

(** [center_nums_board_2] is the corresponding die numbers for each of
    the tiles of board.ml. *)
let center_nums_board_2 =
  [
    (110, 285, "6");
    (180, 285, "5");
    (250, 285, "9");
    (75, 225, "4");
    (145, 225, "3");
    (215, 225, "8");
    (285, 225, "10");
    (40, 165, "6");
    (110, 165, "5");
    (170, 165, "robber");
    (250, 165, "9");
    (320, 165, "12");
    (75, 105, "3");
    (145, 105, "2");
    (215, 105, "10");
    (285, 105, "11");
    (110, 45, "11");
    (180, 45, "4");
    (250, 45, "8");
  ]

let n0 = node_maker "" [||] [] "" (0, 0)

let n1 =
  node_maker "1"
    [| (ref n0, 0); (ref n0, 0); (ref n0, 0) |]
    [ ("forest", 6) ] "" (85, 315)

let n2 =
  node_maker "2"
    [| (ref n0, 0); (ref n0, 0); (ref n0, 0) |]
    [ ("forest", 6) ] "" (115, 330)

let n3 =
  node_maker "3"
    [| (ref n0, 0); (ref n0, 0); (ref n0, 0) |]
    [ ("forest", 6); ("fields", 5) ]
    "" (150, 310)

let n4 =
  node_maker "4"
    [| (ref n0, 0); (ref n0, 0); (ref n0, 0) |]
    [ ("fields", 5) ] "" (185, 330)

let n5 =
  node_maker "5"
    [| (ref n0, 0); (ref n0, 0); (ref n0, 0) |]
    [ ("fields", 5); ("mountains", 9) ]
    "" (220, 310)

let n6 =
  node_maker "6"
    [| (ref n0, 0); (ref n0, 0); (ref n0, 0) |]
    [ ("mountains", 9) ] "" (255, 330)

let n7 =
  node_maker "7"
    [| (ref n0, 0); (ref n0, 0); (ref n0, 0) |]
    [ ("mountains", 9) ] "" (290, 310)

let n8 =
  node_maker "8"
    [| (ref n0, 0); (ref n0, 0); (ref n0, 0) |]
    [ ("hills", 4) ] "" (45, 250)

let n9 =
  node_maker "9"
    [| (ref n0, 0); (ref n0, 0); (ref n0, 0) |]
    [ ("hills", 4); ("forest", 6) ]
    "" (80, 270)

let n10 =
  node_maker "10"
    [| (ref n0, 0); (ref n0, 0); (ref n0, 0) |]
    [ ("hills", 4); ("forest", 6); ("pasture", 3) ]
    "" (115, 250)

let n11 =
  node_maker "11"
    [| (ref n0, 0); (ref n0, 0); (ref n0, 0) |]
    [ ("forest", 6); ("fields", 5); ("hills", 8) ]
    "" (150, 270)

let n12 =
  node_maker "12"
    [| (ref n0, 0); (ref n0, 0); (ref n0, 0) |]
    [ ("pasture", 3); ("fields", 5); ("hills", 8) ]
    "" (185, 250)

let n13 =
  node_maker "13"
    [| (ref n0, 0); (ref n0, 0); (ref n0, 0) |]
    [ ("fields", 5); ("hills", 8); ("mountains", 9) ]
    "" (220, 270)

let n14 =
  node_maker "14"
    [| (ref n0, 0); (ref n0, 0); (ref n0, 0) |]
    [ ("hills", 8); ("mountains", 9); ("mountains", 10) ]
    "" (255, 250)

let n15 =
  node_maker "15"
    [| (ref n0, 0); (ref n0, 0); (ref n0, 0) |]
    [ ("mountains", 9); ("mountains", 10) ]
    "" (290, 270)

let n16 =
  node_maker "16"
    [| (ref n0, 0); (ref n0, 0); (ref n0, 0) |]
    [ ("mountains", 10) ] "" (325, 250)

let n17 =
  node_maker "17"
    [| (ref n0, 0); (ref n0, 0); (ref n0, 0) |]
    [ ("mountains", 6) ] "" (15, 195)

let n18 =
  node_maker "18"
    [| (ref n0, 0); (ref n0, 0); (ref n0, 0) |]
    [ ("mountains", 6); ("hills", 4) ]
    "" (45, 210)

let n19 =
  node_maker "19"
    [| (ref n0, 0); (ref n0, 0); (ref n0, 0) |]
    [ ("mountains", 6); ("hills", 4); ("pasture", 5) ]
    "" (80, 190)

let n20 =
  node_maker "20"
    [| (ref n0, 0); (ref n0, 0); (ref n0, 0) |]
    [ ("hills", 4); ("pasture", 5); ("pasture", 3) ]
    "" (115, 210)

let n21 =
  node_maker "21"
    [| (ref n0, 0); (ref n0, 0); (ref n0, 0) |]
    [ ("pasture", 5); ("pasture", 3); ("desert", 0) ]
    "" (150, 190)

let n22 =
  node_maker "22"
    [| (ref n0, 0); (ref n0, 0); (ref n0, 0) |]
    [ ("pasture", 3); ("desert", 0); ("hills", 8) ]
    "" (185, 210)

let n23 =
  node_maker "23"
    [| (ref n0, 0); (ref n0, 0); (ref n0, 0) |]
    [ ("desert", 0); ("hills", 8); ("fields", 9) ]
    "" (220, 190)

let n24 =
  node_maker "24"
    [| (ref n0, 0); (ref n0, 0); (ref n0, 0) |]
    [ ("hills", 8); ("fields", 9); ("mountains", 10) ]
    "" (255, 210)

let n25 =
  node_maker "25"
    [| (ref n0, 0); (ref n0, 0); (ref n0, 0) |]
    [ ("fields", 9); ("mountains", 10); ("pasture", 12) ]
    "" (290, 190)

let n26 =
  node_maker "26"
    [| (ref n0, 0); (ref n0, 0); (ref n0, 0) |]
    [ ("mountains", 10); ("pasture", 12) ]
    "" (325, 210)

let n27 =
  node_maker "27"
    [| (ref n0, 0); (ref n0, 0); (ref n0, 0) |]
    [ ("pasture", 12) ] "" (360, 190)

let n28 =
  node_maker "28"
    [| (ref n0, 0); (ref n0, 0); (ref n0, 0) |]
    [ ("mountains", 6) ] "" (295, 195)

let n29 =
  node_maker "29"
    [| (ref n0, 0); (ref n0, 0); (ref n0, 0) |]
    [ ("mountains", 6); ("hills", 3) ]
    "" (45, 130)

let n30 =
  node_maker "30"
    [| (ref n0, 0); (ref n0, 0); (ref n0, 0) |]
    [ ("mountains", 6); ("hills", 3); ("pasture", 5) ]
    "" (85, 155)

let n31 =
  node_maker "31"
    [| (ref n0, 0); (ref n0, 0); (ref n0, 0) |]
    [ ("hills", 3); ("pasture", 5); ("forest", 2) ]
    "" (115, 130)

let n32 =
  node_maker "32"
    [| (ref n0, 0); (ref n0, 0); (ref n0, 0) |]
    [ ("pasture", 5); ("forest", 2); ("desert", 0) ]
    "" (150, 150)

let n33 =
  node_maker "33"
    [| (ref n0, 0); (ref n0, 0); (ref n0, 0) |]
    [ ("forest", 2); ("desert", 0); ("fields", 10) ]
    "" (185, 130)

let n34 =
  node_maker "34"
    [| (ref n0, 0); (ref n0, 0); (ref n0, 0) |]
    [ ("desert", 0); ("fields", 10); ("fields", 9) ]
    "" (220, 150)

let n35 =
  node_maker "35"
    [| (ref n0, 0); (ref n0, 0); (ref n0, 0) |]
    [ ("fields", 10); ("fields", 9); ("forest", 11) ]
    "" (255, 130)

let n36 =
  node_maker "36"
    [| (ref n0, 0); (ref n0, 0); (ref n0, 0) |]
    [ ("fields", 9); ("forest", 11); ("pasture", 12) ]
    "" (290, 150)

let n37 =
  node_maker "37"
    [| (ref n0, 0); (ref n0, 0); (ref n0, 0) |]
    [ ("forest", 11); ("pasture", 12) ]
    "" (325, 130)

let n38 =
  node_maker "38"
    [| (ref n0, 0); (ref n0, 0); (ref n0, 0) |]
    [ ("pasture", 12) ] "" (360, 150)

let n39 =
  node_maker "39"
    [| (ref n0, 0); (ref n0, 0); (ref n0, 0) |]
    [ ("hills", 3) ] "" (45, 90)

let n40 =
  node_maker "40"
    [| (ref n0, 0); (ref n0, 0); (ref n0, 0) |]
    [ ("hills", 3); ("fields", 11) ]
    "" (80, 70)

let n41 =
  node_maker "41"
    [| (ref n0, 0); (ref n0, 0); (ref n0, 0) |]
    [ ("hills", 3); ("fields", 11); ("forest", 2) ]
    "" (115, 90)

let n42 =
  node_maker "42"
    [| (ref n0, 0); (ref n0, 0); (ref n0, 0) |]
    [ ("fields", 11); ("forest", 2); ("forest", 4) ]
    "" (150, 70)

let n43 =
  node_maker "43"
    [| (ref n0, 0); (ref n0, 0); (ref n0, 0) |]
    [ ("forest", 2); ("forest", 4); ("fields", 10) ]
    "" (185, 90)

let n44 =
  node_maker "44"
    [| (ref n0, 0); (ref n0, 0); (ref n0, 0) |]
    [ ("forest", 4); ("fields", 10); ("pasture", 8) ]
    "" (220, 70)

let n45 =
  node_maker "45"
    [| (ref n0, 0); (ref n0, 0); (ref n0, 0) |]
    [ ("fields", 10); ("pasture", 8); ("forest", 11) ]
    "" (255, 90)

let n46 =
  node_maker "46"
    [| (ref n0, 0); (ref n0, 0); (ref n0, 0) |]
    [ ("pasture", 8); ("forest", 11) ]
    "" (290, 70)

let n47 =
  node_maker "47"
    [| (ref n0, 0); (ref n0, 0); (ref n0, 0) |]
    [ ("forest", 11) ] "" (325, 90)

let n48 =
  node_maker "48"
    [| (ref n0, 0); (ref n0, 0); (ref n0, 0) |]
    [ ("fields", 11) ] "" (80, 30)

let n49 =
  node_maker "49"
    [| (ref n0, 0); (ref n0, 0); (ref n0, 0) |]
    [ ("fields", 11) ] "" (115, 10)

let n50 =
  node_maker "50"
    [| (ref n0, 0); (ref n0, 0); (ref n0, 0) |]
    [ ("fields", 11); ("forest", 4) ]
    "" (150, 30)

let n51 =
  node_maker "50"
    [| (ref n0, 0); (ref n0, 0); (ref n0, 0) |]
    [ ("forest", 4) ] "" (185, 10)

let n52 =
  node_maker "52"
    [| (ref n0, 0); (ref n0, 0); (ref n0, 0) |]
    [ ("forest", 4); ("pasture", 8) ]
    "" (220, 30)

let n53 =
  node_maker "53"
    [| (ref n0, 0); (ref n0, 0); (ref n0, 0) |]
    [ ("pasture", 8) ] "" (255, 10)

let n54 =
  node_maker "54"
    [| (ref n0, 0); (ref n0, 0); (ref n0, 0) |]
    [ ("pasture", 8) ] "" (290, 30)

let node_array = Array.make 54 n0

(* [run] creates the board to be used in gameplay*)
let run () =
  assign_neighbor n1 [ (ref n0, 0); (ref n9, 0); (ref n2, 0) ];
  node_array.(0) <- n1;
  assign_neighbor n2 [ (ref n1, 0); (ref n0, 0); (ref n3, 0) ];
  node_array.(1) <- n2;
  assign_neighbor n3 [ (ref n2, 0); (ref n11, 0); (ref n4, 0) ];
  node_array.(2) <- n3;
  assign_neighbor n4 [ (ref n3, 0); (ref n0, 0); (ref n5, 0) ];
  node_array.(3) <- n4;
  assign_neighbor n5 [ (ref n4, 0); (ref n13, 0); (ref n6, 0) ];
  node_array.(4) <- n5;
  assign_neighbor n6 [ (ref n5, 0); (ref n0, 0); (ref n7, 0) ];
  node_array.(5) <- n6;
  assign_neighbor n7 [ (ref n6, 0); (ref n15, 0); (ref n0, 0) ];
  node_array.(6) <- n7;
  assign_neighbor n8 [ (ref n0, 0); (ref n17, 0); (ref n9, 0) ];
  node_array.(7) <- n8;
  assign_neighbor n9 [ (ref n8, 0); (ref n1, 0); (ref n10, 0) ];
  node_array.(8) <- n9;

  assign_neighbor n10 [ (ref n9, 0); (ref n20, 0); (ref n11, 0) ];
  node_array.(9) <- n10;
  assign_neighbor n11 [ (ref n10, 0); (ref n3, 0); (ref n12, 0) ];
  node_array.(10) <- n11;
  assign_neighbor n12 [ (ref n11, 0); (ref n22, 0); (ref n13, 0) ];
  node_array.(11) <- n12;
  assign_neighbor n13 [ (ref n12, 0); (ref n5, 0); (ref n14, 0) ];
  node_array.(12) <- n13;
  assign_neighbor n14 [ (ref n13, 0); (ref n24, 0); (ref n15, 0) ];
  node_array.(13) <- n14;

  assign_neighbor n15 [ (ref n14, 0); (ref n7, 0); (ref n16, 0) ];
  node_array.(14) <- n15;
  assign_neighbor n16 [ (ref n15, 0); (ref n26, 0); (ref n0, 0) ];
  node_array.(15) <- n16;
  assign_neighbor n17 [ (ref n0, 0); (ref n28, 0); (ref n18, 0) ];
  node_array.(16) <- n17;
  assign_neighbor n18 [ (ref n17, 0); (ref n8, 0); (ref n19, 0) ];
  node_array.(17) <- n18;

  assign_neighbor n19 [ (ref n18, 0); (ref n30, 0); (ref n20, 0) ];
  node_array.(18) <- n19;
  assign_neighbor n20 [ (ref n19, 0); (ref n10, 0); (ref n21, 0) ];
  node_array.(19) <- n20;
  assign_neighbor n21 [ (ref n20, 0); (ref n32, 0); (ref n22, 0) ];
  node_array.(20) <- n21;
  assign_neighbor n22 [ (ref n21, 0); (ref n12, 0); (ref n23, 0) ];
  node_array.(21) <- n22;
  assign_neighbor n23 [ (ref n22, 0); (ref n34, 0); (ref n24, 0) ];
  node_array.(22) <- n23;
  assign_neighbor n24 [ (ref n23, 0); (ref n14, 0); (ref n25, 0) ];
  node_array.(23) <- n24;
  assign_neighbor n25 [ (ref n24, 0); (ref n36, 0); (ref n26, 0) ];
  node_array.(24) <- n25;

  assign_neighbor n26 [ (ref n25, 0); (ref n16, 0); (ref n27, 0) ];
  node_array.(25) <- n26;
  assign_neighbor n27 [ (ref n26, 0); (ref n38, 0); (ref n0, 0) ];
  node_array.(26) <- n27;
  assign_neighbor n28 [ (ref n0, 0); (ref n17, 0); (ref n29, 0) ];
  node_array.(27) <- n28;
  assign_neighbor n29 [ (ref n28, 0); (ref n39, 0); (ref n30, 0) ];
  node_array.(28) <- n29;

  assign_neighbor n30 [ (ref n29, 0); (ref n19, 0); (ref n31, 0) ];
  node_array.(29) <- n30;
  assign_neighbor n31 [ (ref n30, 0); (ref n41, 0); (ref n32, 0) ];
  node_array.(30) <- n31;
  assign_neighbor n32 [ (ref n31, 0); (ref n21, 0); (ref n33, 0) ];
  node_array.(31) <- n32;
  assign_neighbor n33 [ (ref n32, 0); (ref n43, 0); (ref n34, 0) ];
  node_array.(32) <- n33;
  assign_neighbor n34 [ (ref n33, 0); (ref n23, 0); (ref n35, 0) ];
  node_array.(33) <- n34;
  assign_neighbor n35 [ (ref n34, 0); (ref n45, 0); (ref n36, 0) ];
  node_array.(34) <- n35;
  assign_neighbor n36 [ (ref n35, 0); (ref n25, 0); (ref n0, 0) ];
  node_array.(35) <- n36;

  assign_neighbor n37 [ (ref n36, 0); (ref n47, 0); (ref n38, 0) ];
  node_array.(36) <- n37;
  assign_neighbor n38 [ (ref n37, 0); (ref n27, 0); (ref n0, 0) ];
  node_array.(37) <- n38;
  assign_neighbor n39 [ (ref n0, 0); (ref n29, 0); (ref n40, 0) ];
  node_array.(38) <- n39;
  assign_neighbor n40 [ (ref n39, 0); (ref n48, 0); (ref n41, 0) ];
  node_array.(39) <- n40;

  assign_neighbor n41 [ (ref n40, 0); (ref n31, 0); (ref n42, 0) ];
  node_array.(40) <- n41;
  assign_neighbor n42 [ (ref n41, 0); (ref n50, 0); (ref n43, 0) ];
  node_array.(41) <- n42;
  assign_neighbor n43 [ (ref n42, 0); (ref n33, 0); (ref n44, 0) ];
  node_array.(42) <- n43;
  assign_neighbor n44 [ (ref n43, 0); (ref n52, 0); (ref n45, 0) ];
  node_array.(43) <- n44;
  assign_neighbor n45 [ (ref n44, 0); (ref n35, 0); (ref n46, 0) ];
  node_array.(44) <- n45;

  assign_neighbor n46 [ (ref n45, 0); (ref n54, 0); (ref n47, 0) ];
  node_array.(45) <- n46;
  assign_neighbor n47 [ (ref n45, 0); (ref n37, 0); (ref n0, 0) ];
  node_array.(46) <- n47;
  assign_neighbor n48 [ (ref n0, 0); (ref n40, 0); (ref n49, 0) ];
  node_array.(47) <- n48;
  assign_neighbor n49 [ (ref n48, 0); (ref n0, 0); (ref n50, 0) ];
  node_array.(48) <- n49;
  assign_neighbor n50 [ (ref n49, 0); (ref n42, 0); (ref n51, 0) ];
  node_array.(49) <- n50;
  assign_neighbor n51 [ (ref n50, 0); (ref n0, 0); (ref n52, 0) ];
  node_array.(50) <- n51;
  assign_neighbor n52 [ (ref n51, 0); (ref n44, 0); (ref n53, 0) ];
  node_array.(51) <- n52;
  assign_neighbor n53 [ (ref n52, 0); (ref n0, 0); (ref n54, 0) ];
  node_array.(52) <- n53;
  assign_neighbor n54 [ (ref n53, 0); (ref n46, 0); (ref n0, 0) ];
  node_array.(53) <- n54

let () = run ()

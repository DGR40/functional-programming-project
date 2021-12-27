type terrain_id = string

type building_id = string

type tile = {
  num : int;
  terrain : terrain_id;
}

type t = {
  id : string;
  pixel : int * int;
  mutable neighbors : (t ref * int) array;
  mutable tile : tile list;
  mutable building : building_id;
}

(** [tile_maker] creates a list of tiles based on the (terrian id * int)
    list *)
let rec tile_maker (terr_lst : (terrain_id * int) list) =
  match terr_lst with
  | [] -> []
  | h :: t ->
      begin
        match h with
        | l, n -> { num = n; terrain = l }
      end
      :: tile_maker t

(** [get_id n] is the id of a given node [n]. *)
let get_id (n : t) = n.id

let check_building (n : t) s : bool =
  if String.equal n.building s then false else true

(** [non_empt s] is the id of the node if there is a building on node
    [s]. If not [non_empt s] is the empty string *)
let non_empt s n = if check_building n s then "" else get_id n

let node_maker node_id lst terr build pix =
  let t' = tile_maker terr in
  {
    id = node_id;
    pixel = pix;
    neighbors = lst;
    tile = t';
    building = build;
  }

(** [assign_neighbor] assigns a node to a node *)
let assign_neighbor (n : t) (lst : (t ref * int) list) =
  for i = 0 to List.length lst - 1 do
    n.neighbors.(i) <- List.nth lst i
  done

(* [change_building] takes in an array of nodes, a specific node's
   index, and a new building and updates the building field to a new
   building*)
let change_building arr (num : int) (b : building_id) =
  let n = arr.(num) in
  n.building <- b

let node_pix n = n.pixel

(* recursively runs through a list of tiles and turns it into an array *)
let rec t_array (t : tile list) =
  match t with
  | [] -> [||]
  | { num = i; terrain = terr } :: l ->
      Array.append [| (i, terr) |] (t_array l)

(* pass in a node, returns a tile array *)
let get_tile_array node = t_array node.tile

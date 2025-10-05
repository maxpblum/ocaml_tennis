open Game_logic
open Box_drawing
open Keys

let court_width = 70
let court_length = 34

let basic_court =
  init_box court_length court_width " "
  (* Far court base line *)
  |> replace_at_coords "-" (hor_line_pairs 0 0 (court_width - 1))
  (* Near court base line *)
  |> replace_at_coords "-" (hor_line_pairs (court_length - 1) 0 (court_width - 1))
  (* Left boundary *)
  |> replace_at_coords "|" (ver_line_pairs 0 0 (court_length - 1))
  (* Right boundary *)
  |> replace_at_coords "|" (ver_line_pairs (court_width - 1) 0 (court_length - 1))
  (* Service dividers *)
  |> replace_at_coords "|" (ver_line_pairs (court_width / 2) (court_length / 4) (3 * court_length / 4))
  (* Net *)
  |> replace_at_coords "=" (hor_line_pairs (court_length / 2) 0 (court_width - 1))
  (* Far court service line *)
  |> replace_at_coords "-" (hor_line_pairs (court_length / 4) 0 (court_width - 1))
  (* Near court base line *)
  |> replace_at_coords "-" (hor_line_pairs (3 * court_length / 4) 0 (court_width - 1))

let player_pic = {|
  O   
 /|\-o
  |   
 / \  
|}

let pic_of_key (key : string) = [
  "*-*";
  ["|"; key; "|"] |> String.concat "";
  "*-*";
] |> String.concat "\n"

type court = Near | Far

(* The row*col pair in the center of a given court position *)
let pos_ctr_coords (crt : court) (pos: position) : (int*int) =
  match crt with
  | Near -> (match pos with 
    | DeepAd   -> (((11 * court_length) / 12), ((1 * court_width) / 6))
    | DeepCtr  -> (((11 * court_length) / 12), ((3 * court_width) / 6))
    | DeepDc   -> (((11 * court_length) / 12), ((5 * court_width) / 6))
    | MidAd    -> (((09 * court_length) / 12), ((1 * court_width) / 6))
    | MidCtr   -> (((09 * court_length) / 12), ((3 * court_width) / 6))
    | MidDc    -> (((09 * court_length) / 12), ((5 * court_width) / 6))
    | ShortAd  -> (((07 * court_length) / 12), ((1 * court_width) / 6))
    | ShortCtr -> (((07 * court_length) / 12), ((3 * court_width) / 6))
    | ShortDc  -> (((07 * court_length) / 12), ((5 * court_width) / 6))
  )
  | Far -> (match pos with
    | DeepAd   -> (((01 * court_length) / 12), ((5 * court_width) / 6))
    | DeepCtr  -> (((01 * court_length) / 12), ((3 * court_width) / 6))
    | DeepDc   -> (((01 * court_length) / 12), ((1 * court_width) / 6))
    | MidAd    -> (((03 * court_length) / 12), ((5 * court_width) / 6))
    | MidCtr   -> (((03 * court_length) / 12), ((3 * court_width) / 6))
    | MidDc    -> (((03 * court_length) / 12), ((1 * court_width) / 6))
    | ShortAd  -> (((05 * court_length) / 12), ((5 * court_width) / 6))
    | ShortCtr -> (((05 * court_length) / 12), ((3 * court_width) / 6))
    | ShortDc  -> (((05 * court_length) / 12), ((1 * court_width) / 6))
  )

let draw_player (crt : court) (pos : position) (box : string list list) =
  let (centerrow, centercol) = pos_ctr_coords crt pos in
  let (startrow, startcol) = center_pic player_pic centerrow centercol in
  draw_pic {startrow; startcol; pic = player_pic} box

let draw_shot_choosers (turn : player) (box : string list list) =
  let get_key = match turn with
  | One -> pos_to_p1_key
  | Two -> pos_to_p2_key
  in
  let draw_key (pos : position) (center_row : int) (center_col : int) (box : string list list) =
    let pic = pos |> get_key |> pic_of_key in
    let (startrow, startcol) = center_pic pic center_row center_col in
    draw_pic {startrow; startcol; pic} box
  in
  box
  |> draw_key DeepAd   (01 * court_length / 12) (5 * court_width / 6)
  |> draw_key DeepCtr  (01 * court_length / 12) (3 * court_width / 6)
  |> draw_key DeepDc   (01 * court_length / 12) (1 * court_width / 6)
  |> draw_key MidAd    (03 * court_length / 12) (5 * court_width / 6)
  |> draw_key MidCtr   (03 * court_length / 12) (3 * court_width / 6)
  |> draw_key MidDc    (03 * court_length / 12) (1 * court_width / 6)
  |> draw_key ShortAd  (05 * court_length / 12) (5 * court_width / 6)
  |> draw_key ShortCtr (05 * court_length / 12) (3 * court_width / 6)
  |> draw_key ShortDc  (05 * court_length / 12) (1 * court_width / 6)

let str_of_state (st : state) : string =
  basic_court |> draw_player Far st.p2_pos |> draw_player Near st.p1_pos |> draw_shot_choosers st.turn |> str_of_box

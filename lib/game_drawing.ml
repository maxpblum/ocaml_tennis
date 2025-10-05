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

let draw_status (status : string) (box : string list list) =
  draw_pic {startrow = court_length; startcol = 0; pic = status} box

type position =
  | DeepAd
  | DeepCtr
  | DeepDc  (* deep deuce court *)
  | MidAd
  | MidCtr
  | MidDc
  | ShortAd
  | ShortCtr
  | ShortDc

type shot_selection = {
  current_player_pos : position;
  opponent_pos : position;
  shot : position;  (* where in the opponent's court you are trying to hit the ball *)
}

type probabilities = {
  (* How likely am I to hit it out / in the net? *)
  miss : int;
  (* ASSUMING the shot goes in, how likely is it to win the point outright, i.e. by the opponent not getting their racket on it? *)
  winner : int; (* both are percentages, e.g., 25 for 25% *)
}

(* Had Gemini help me write the probability code. *)
(* ****** BEGIN GEMINI CODE ******* *)

(**
 * Converts a position on the court to an (x, y) coordinate.
 * x: 0=Ad, 1=Center, 2=Deuce
 * y: 0=Short (Net), 1=Mid, 2=Deep (Baseline)
 *)
let pos_to_coords (pos : position) : (int * int) =
  match pos with
  | ShortAd  -> (0, 0)
  | ShortCtr -> (1, 0)
  | ShortDc  -> (2, 0)
  | MidAd    -> (0, 1)
  | MidCtr   -> (1, 1)
  | MidDc    -> (2, 1)
  | DeepAd   -> (0, 2)
  | DeepCtr  -> (1, 2)
  | DeepDc   -> (2, 2)

(**
 * Calculates the Manhattan distance between two positions on our grid.
 * This represents how many "squares" an opponent would have to move.
 *)
let distance (pos1 : position) (pos2 : position) : int =
  let (x1, y1) = pos_to_coords pos1 in
  let (x2, y2) = pos_to_coords pos2 in
  abs (x1 - x2) + abs (y1 - y2)

(**
 * Clamps an integer value between a min and max.
 * Used to ensure our probabilities stay within a reasonable range (e.g., 5% to 95%).
 *)
let clamp (value : int) (min_val : int) (max_val : int) : int =
  max min_val (min max_val value)

(**
 * Checks if a value `x` is between `a` and `b`, inclusive.
 *)
let is_between x a b =
  let min_ab = min a b in
  let max_ab = max a b in
  x >= min_ab && x <= max_ab

(****************************************************************************)
(* THE MAIN FUNCTION                               *)
(****************************************************************************)

let shot_probabilities (selection : shot_selection) : probabilities =

  (* --- 1. COORDINATE SETUP --- *)
  let player_coords = pos_to_coords selection.current_player_pos in
  let opponent_coords = pos_to_coords selection.opponent_pos in
  let shot_coords = pos_to_coords selection.shot in

  let (player_x, _) = player_coords in
  let (opponent_x, opponent_y) = opponent_coords in
  let (shot_x, shot_y) = shot_coords in


  (* --- 2. CALCULATE MISS PROBABILITY --- *)
  (* This is based on the risk of the target location ('shot'). *)
  
  (* A base miss chance for the safest shot (MidCtr). *)
  let base_miss = 10 in

  (* Penalty for hitting near the sidelines. *)
  let sideline_penalty =
    if shot_x = 0 || shot_x = 2 then 15 else 0
  in

  (* Penalty for hitting near the net (higher risk) or baseline (less risk). *)
  let depth_penalty =
    match shot_y with
    | 0 -> 25 (* Short shots (drop shots) are very risky *)
    | 1 -> 0  (* Mid-court is safest *)
    | 2 -> 10 (* Deep shots are riskier than mid, but less than drop shots *)
  in
  
  let total_miss = base_miss + sideline_penalty + depth_penalty in
  let final_miss = clamp total_miss 5 95 in


  (* --- 3. CALCULATE WINNER PROBABILITY --- *)
  (* This is based on the 'shot' location relative to the 'opponent_pos'. *)

  (* A base chance for any shot to be a winner (e.g., opponent trips). *)
  let base_winner = 5 in

  (* Factor 1: Distance from opponent. The farther the shot, the better. *)
  (* Max distance on our grid is 4. Let's scale it by a factor. *)
  let distance_from_opponent = distance selection.shot selection.opponent_pos in
  let distance_bonus = distance_from_opponent * 8 in (* Max bonus: 4 * 8 = 32 *)

  (* Factor 2 & 3: Shot is behind or in front of the opponent. *)
  let vertical_position_bonus =
    if shot_y > opponent_y then
      30 (* Hitting deep behind the opponent is a huge advantage. *)
    else if shot_y < opponent_y then
      15 (* A good drop shot when the opponent is deep is also very effective. *)
    else
      0
  in

  (* Factor 4: Trajectory is near the opponent. *)
  (* We penalize shots where the ball's horizontal path crosses over the opponent's
     current horizontal position, making it easier to intercept. *)
  let trajectory_penalty =
    if is_between opponent_x player_x shot_x then
      -15 (* Opponent is in the path of the ball (e.g., for a cross-court shot) *)
    else
      0
  in

  let total_winner = base_winner + distance_bonus + vertical_position_bonus + trajectory_penalty in
  let final_winner = clamp total_winner 1 90 in


  (* --- 4. RETURN RESULT --- *)
  { miss = final_miss; winner = final_winner }

(* ******** END GEMINI CODE ********* *)

let str_of_state (st : state) : string =
  [
    basic_court
    |> draw_player (if st.turn = One then Near else Far) st.p1_pos
    |> draw_player (if st.turn = Two then Near else Far) st.p2_pos
    |> draw_shot_choosers st.turn
    |> str_of_box;
    "\n";
    st.status;
  ] |> String.concat ""

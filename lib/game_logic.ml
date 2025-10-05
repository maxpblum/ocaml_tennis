type player = One | Two

type position =
  | DeepAd  | DeepCtr  | DeepDc
  | MidAd   | MidCtr   | MidDc
  | ShortAd | ShortCtr | ShortDc

let _ = DeepAd
let _ = DeepCtr
let _ = DeepDc
let _ = MidAd
let _ = MidCtr
let _ = MidDc
let _ = ShortAd
let _ = ShortCtr
let _ = ShortDc

type state = {
  turn : player;
  p1_pos : position;
  p2_pos : position;
  status : string;
}

let str_of_player = function
  | One -> "one"
  | Two -> "two"

let next_move_status (pl : player) =
  ["Player "; (str_of_player pl); "'s turn! Press a key to choose your shot."] |> String.concat ""

let initial_state = {
  turn = One;
  p1_pos = DeepAd;
  p2_pos = DeepAd;
  status = next_move_status One;
}

type player_turn_input = Shot of position | Ignorable

type make_status_params = {
  shot : position;
  shot_by : player;
  next_player : player;
}

let str_of_pos : position -> string = function
| DeepAd -> "deep in the ad court"
| DeepCtr -> "deep in the center court"
| DeepDc -> "deep in the deuce court"
| MidAd -> "medium-depth in the ad court"
| MidCtr -> "medium-depth in the center court"
| MidDc -> "medium-depth in the deuce court"
| ShortAd -> "short in the ad court"
| ShortCtr -> "short in the center court"
| ShortDc -> "short in the deuce court"

let make_status (params : make_status_params) =
  [
    "Player ";
    str_of_player params.shot_by;
    " hit the ball ";
    str_of_pos params.shot;
    " and player ";
    str_of_player params.next_player;
    " made it to the ball in time.\n";
    next_move_status params.next_player;
  ] |> String.concat ""

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
    | _ -> 100 (* Shouldn't happen, so let's do something crazy so we'll notice. *)
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

let next_state (input : player_turn_input) (st : state) : state = match input with
  | Ignorable -> st
  | Shot shot ->
    let {turn; p1_pos; p2_pos; _} = st in
    let next_turn = if turn = One then Two else One in
    (* If this player is the one who hit the shot, stay put
       (even though that's unrealistic; just for simplicity for now.)
       If the other player hit it, move to the ball.
    *)
    let next_p1_pos = if turn = One then p1_pos else shot in
    let next_p2_pos = if turn = Two then p2_pos else shot in
    {
      turn = next_turn;
      p1_pos = next_p1_pos;
      p2_pos = next_p2_pos;
      status = make_status {
        shot = shot;
        shot_by = turn;
        next_player = next_turn;
      }
    }

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

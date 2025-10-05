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
}

let initial_state = {
  turn = One;
  p1_pos = DeepAd;
  p2_pos = DeepAd;
}

(* temporary function to test moving things around and drawing them *)
let cycle_pos : position -> position = function
| DeepAd -> DeepCtr
| DeepCtr -> DeepDc
| DeepDc -> MidAd
| MidAd -> MidCtr
| MidCtr -> MidDc
| MidDc -> ShortAd
| ShortAd -> ShortCtr
| ShortCtr -> ShortDc
| ShortDc -> DeepAd

type player_turn_input = Shot of position | Ignorable

let next_state (input : player_turn_input) (st : state) : state = match input with
  | Ignorable -> st
  | Shot _ ->
    let {turn; p1_pos; p2_pos} = st in
    let next_turn = if turn = One then Two else One in
    let next_p1_pos = cycle_pos p1_pos in
    let next_p2_pos = cycle_pos p2_pos in
    { turn = next_turn; p1_pos = next_p1_pos; p2_pos = next_p2_pos}

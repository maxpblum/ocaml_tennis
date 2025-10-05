open Game_logic

let p1_key_to_input : string -> player_turn_input = function
  | "q" -> Shot DeepDc
  | "w" -> Shot DeepCtr
  | "e" -> Shot DeepAd
  | "a" -> Shot MidDc
  | "s" -> Shot MidCtr
  | "d" -> Shot MidAd
  | "z" -> Shot ShortDc
  | "x" -> Shot ShortCtr
  | "c" -> Shot ShortAd
  | _ -> Ignorable

let pos_to_p1_key : position -> string = function
  | DeepDc -> "q"
  | DeepCtr -> "w"
  | DeepAd -> "e"
  | MidDc -> "a"
  | MidCtr -> "s"
  | MidAd -> "d"
  | ShortDc -> "z"
  | ShortCtr -> "x"
  | ShortAd -> "c"

let pos_to_p2_key : position -> string = function
  | DeepDc -> "u"
  | DeepCtr -> "i"
  | DeepAd -> "o"
  | MidDc -> "j"
  | MidCtr -> "k"
  | MidAd -> "l"
  | ShortDc -> "m"
  | ShortCtr -> ","
  | ShortAd -> "."

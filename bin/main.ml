(* Empty row (list of single-character strings) *)
let rec init_row (init_val : string) : int -> string list = function
| 0 -> []
| cols -> init_val :: (cols - 1 |> init_row init_val)

(* Empty char matrix (list of lists; outer list is rows, inner list is single-character strings within that row) *)
let rec init_box (rows : int) (cols : int) (init_val : string): string list list = match rows with
| 0 -> []
| rows -> (init_row init_val cols) :: (init_box (rows - 1) cols init_val)

let str_of_box (box : string list list) : string = box |> List.map (String.concat "") |> String.concat "\n"

let rec replace_one_char_in_row (ch : string) (idx : int) : string list -> string list = function
  | [] -> []
  | x :: xs -> match idx with
    | 0 -> ch :: xs
    | idx -> x :: (replace_one_char_in_row ch (idx - 1) xs)

let rec replace_one_char_in_box (ch : string) (row_idx : int) (col_idx : int) : string list list -> string list list = function
  | [] -> []
  | x :: xs -> match row_idx with
    | 0 -> (replace_one_char_in_row ch col_idx x) :: xs
    | row_idx -> x :: (replace_one_char_in_box ch (row_idx - 1) col_idx xs)

(* Implementation for range function. endnum should always be >= startnum,
   and this will return, effectively, the range startnum through endnum
   (inclusive) concatenated in front of the members of accum. *)
let rec range_impl (accum: int list) (startnum : int) (endnum : int) =
if endnum = startnum then startnum :: accum
else range_impl (endnum :: accum) startnum (endnum - 1)

(* Get a range from startnum to endnum inclusive as a list. *)
let range : int -> int -> int list = range_impl []

(* Get all coordinate pairs along a line in row r, from col startcol to endcol inclusive. *)
let hor_line_pairs (rownum : int) (startcol : int) (endcol : int) =
  range startcol endcol |> List.map (fun c -> (rownum, c))

(* Get all coordinate pairs along a line in row c, from row startrow to endrow inclusive. *)
let ver_line_pairs (colnum : int) (startrow : int) (endrow : int) =
  range startrow endrow |> List.map (fun r -> (r, colnum))

(* Within a box, replace the chars at all coordinates with the given character. *)
let rec replace_at_coords (ch: string) (coords_list : (int*int) list) (box : string list list) : string list list =
  match coords_list with
    | [] -> box
    | (rownum, colnum) :: coords_list ->
        let newbox = replace_one_char_in_box ch rownum colnum box in
        replace_at_coords ch coords_list newbox

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


type player = One | Two

type position =
  | DeepAd  | DeepCtr  | DeepDc
  | MidAd   | MidCtr   | MidDc
  | ShortAd | ShortCtr | ShortDc

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

let player_pic = {|
 o 
\|/
/ \
|}

type draw_pic_impl_params = {
  startrow : int;
  startcol : int;
  newlines_passed : int;
  next_offset_in_current_col : int;
  next_idx : int;
  pic : string;
  box : string list list
}
let rec draw_pic_impl (params : draw_pic_impl_params) : string list list =
  if params.next_idx = String.length params.pic then params.box
  else
  match String.sub params.pic params.next_idx 1 with
  | "\n" -> draw_pic_impl {params with newlines_passed = params.newlines_passed + 1; next_idx = params.next_idx + 1; next_offset_in_current_col = 0}
  | ch ->
      let newbox =
        replace_one_char_in_box ch (params.startrow + params.newlines_passed) (params.startcol + params.next_offset_in_current_col) params.box
      in
      draw_pic_impl {params with box = newbox; next_idx = params.next_idx + 1; next_offset_in_current_col = params.next_offset_in_current_col + 1}

type draw_pic_params = {startrow : int; startcol : int; pic : string}
let draw_pic (params : draw_pic_params) (box : string list list) : string list list =
  draw_pic_impl {
    startrow = params.startrow;
    startcol = params.startcol;
    pic = params.pic;
    box = box;
    newlines_passed = 0;
    next_idx = 0;
    next_offset_in_current_col = 0;
}

type court = Near | Far

let _ = DeepAd
let _ = DeepCtr
let _ = DeepDc
let _ = MidAd
let _ = MidCtr
let _ = MidDc
let _ = ShortAd
let _ = ShortCtr
let _ = ShortDc

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

(* Get starting row and col indices for where to draw a given picture-string given a center position. *)
let center_pic (pic : string) (row : int) (col : int) =
  let rows = pic |> String.to_seq |> Seq.filter (fun c -> c = '\n') |> List.of_seq |> List.length in
  let cols = String.split_on_char '\n' pic |> fun rs -> List.nth rs 0 |> String.length in
  (row - (rows / 2), col - (cols / 2))

let draw_player (crt : court) (pos : position) (box : string list list) =
  let (centerrow, centercol) = pos_ctr_coords crt pos in
  let (startrow, startcol) = center_pic player_pic centerrow centercol in
  draw_pic {startrow; startcol; pic = player_pic} box

(* The moves a player can make, i.e. either end the game or choose what part of the opponent's court to hit the ball to. *)
type player_turn_input =
(*  | Deep_Cr *)
(*  | Deep_Ad *)
(*  | Deep_Dc *)
(*  | Mid_Cr *)
  | Mid_Ad
(*  | Mid_Dc *)
(*  | Short_Cr *)
(*  | Short_Ad *)
(*  | Short_Dc *)
(*  | Exit *)

(* Unimplemented. *)
let input_from_char (_ : player) (_ : string) : player_turn_input = Mid_Ad

let next_state (_ : player_turn_input) (st : state) : state =
  let {turn; p1_pos; p2_pos} = st in
  let next_turn = if turn = One then Two else One in
  let next_p1_pos = cycle_pos p1_pos in
  let next_p2_pos = cycle_pos p2_pos in
  { turn = next_turn; p1_pos = next_p1_pos; p2_pos = next_p2_pos}

let str_of_state (st : state) : string =
  basic_court |> draw_player Far st.p2_pos |> draw_player Near st.p1_pos |> str_of_box

let draw_state_cmds (state : state) = Terml.([
  Command.Terminal Terminal.(ClearScreen All);
  Command.Cursor Cursor.(MoveTo (1, 1));
  Command.Print Style.(make (str_of_state state) (styled ()));
])

let () = Terml.(
  let restore = Terminal.enable_raw_mode () in
  let channel = Events.poll () in
  let rec process_next state =
    draw_state_cmds state |> Command.execute;
    let event = Event.sync (Event.receive channel) in
    match event with
    | Events.Key { code = Events.Escape; _ } -> ()
    | Events.Key { code = Char c; _ } -> state |> next_state (input_from_char state.turn c) |> process_next
    | _ -> process_next state
  in
  process_next initial_state;
  Command.execute [ Command.Terminal Terminal.LeaveAlternateScreen ];
  Terminal.disable_raw_mode restore
)

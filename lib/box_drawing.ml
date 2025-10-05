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

(* Get starting row and col indices for where to draw a given picture-string given a center position. *)
let center_pic (pic : string) (row : int) (col : int) =
  let rows = pic |> String.to_seq |> Seq.filter (fun c -> c = '\n') |> List.of_seq |> List.length in
  let cols = String.split_on_char '\n' pic |> fun rs -> List.nth rs 0 |> String.length in
  (row - (rows / 2), col - (cols / 2))

open Ocaml_tennis.Game_logic
open Ocaml_tennis.Game_drawing
open Ocaml_tennis.Keys

(* Unimplemented. *)
let input_from_char (pl : player) (ch : string) : player_turn_input =
  (if pl = One then p1_key_to_input else p2_key_to_input) ch

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

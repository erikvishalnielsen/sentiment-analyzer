open! Core

module Colors = struct
  let black = Graphics.rgb 000 000 000
  let white = Graphics.rgb 255 255 255
  let green = Graphics.rgb 000 255 000
  let red = Graphics.rgb 255 000 000
  let blue = Graphics.rgb 0 0 255
end

(* These constants are optimized for running on a low-resolution screen. Feel
   free to increase the scaling factor to tweak! *)
module Constants = struct
  let scaling_factor = 1.
  let gui_height = 600. *. scaling_factor |> Float.iround_down_exn
  let header_height = 75. *. scaling_factor |> Float.iround_down_exn
  let gui_width = 675. *. scaling_factor |> Float.iround_down_exn
  let block_size = 27. *. scaling_factor |> Float.iround_down_exn
end

let only_one : bool ref = ref false

let init_exn () =
  let open Constants in
  (* Should raise if called twice *)
  if !only_one
  then failwith "Can only call init_exn once"
  else only_one := true;
  Graphics.open_graph
    (Printf.sprintf
       " %dx%d"
       (gui_height + header_height)
       gui_width);
;;

let read_key () =
  if Graphics.key_pressed () then Some (Graphics.read_key ()) else None
;;

let draw_header ~game_state score =
  let open Constants in
  let header_color = Colors.black in
  Graphics.set_color header_color;
  Graphics.fill_rect 0 gui_height gui_width header_height;
  let header_text = "STOCK SENTIMENT" in
  Graphics.set_color Colors.white;
  Graphics.set_text_size 1000;
  Graphics.moveto 0 (gui_height + 25);
  Graphics.draw_string (Printf.sprintf " %s" header_text);
  Graphics.moveto (play_area_width - 75) (play_area_height + 25);
  Graphics.draw_string (Printf.sprintf "Score: %d" score)
;;

let draw_play_area () =
  let open Constants in
  Graphics.set_color Colors.black;
  Graphics.fill_rect 0 0 play_area_width play_area_height
;;


let render game =
  (* We want double-buffering. See
     https://v2.ocaml.org/releases/4.03/htmlman/libref/Graphics.html for more
     info!

     So, we set [display_mode] to false, draw to the background buffer, set
     [display_mode] to true and then synchronize. This guarantees that there
     won't be flickering! *)
  Graphics.display_mode false;
  draw_header ~game_state (Game.score game);
  draw_play_area ();
  draw_apple apple;
  draw_snake (Snake.head snake) (Snake.tail snake);
  Graphics.display_mode true;
  Graphics.synchronize ()
;;
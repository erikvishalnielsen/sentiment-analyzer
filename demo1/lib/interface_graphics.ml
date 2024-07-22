open! Core

module Colors = struct
  let black = Graphics.rgb 000 000 000
  let white = Graphics.rgb 255 255 255
  let _green = Graphics.rgb 000 255 000
  let _red = Graphics.rgb 255 000 000
  let _blue = Graphics.rgb 0 0 255
end

(* These constants are optimized for running on a low-resolution screen. Feel
   free to increase the scaling factor to tweak! *)
module Constants = struct
  let scaling_factor = 1.
  let gui_height = 600. *. scaling_factor |> Float.iround_down_exn
  let header_height = 75. *. scaling_factor |> Float.iround_down_exn
  let gui_width = 675. *. scaling_factor |> Float.iround_down_exn
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
  Interface.create ();
;;

let read_key () =
  if Graphics.key_pressed () then Some (Graphics.read_key ()) else None
;;

let draw_header () =
  let open Constants in
  let header_color = Colors.black in
  Graphics.set_color header_color;
  Graphics.fill_rect 0 gui_height gui_width header_height;
  let header_text = "STOCK SENTIMENT" in
  Graphics.set_color Colors.white;
  Graphics.set_text_size 1000;
  Graphics.moveto 0 (gui_height + 25);
  Graphics.draw_string (Printf.sprintf " %s" header_text);
  Graphics.moveto (gui_width - 75) (gui_height + 25);
  Graphics.draw_string (Printf.sprintf "STOCK SENTIMENT")
;;

let draw_play_area () =
  let open Constants in
  Graphics.set_color Colors.white;
  Graphics.fill_rect 0 0 gui_width gui_height
;;


let render () =
  (* We want double-buffering. See
     https://v2.ocaml.org/releases/4.03/htmlman/libref/Graphics.html for more
     info!

     So, we set [display_mode] to false, draw to the background buffer, set
     [display_mode] to true and then synchronize. This guarantees that there
     won't be flickering! *)
  Graphics.display_mode false;
  draw_header ();
  draw_play_area ();
  Graphics.display_mode true;
  Graphics.synchronize ()
;;
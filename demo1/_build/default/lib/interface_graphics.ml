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
  Graphics.moveto (gui_width / 2 - 45) (gui_height + 25);
  Graphics.draw_string (Printf.sprintf " %s" header_text);
;;

let draw_ticker_box clicked message =
  let text_color = if clicked then Graphics.cyan else 0x058BBD in
  Graphics.set_color text_color;
  Graphics.fill_rect 94 575 100 25;
  let header_text = "Stock:" in
  Graphics.set_color Colors.black;
  Graphics.set_text_size 200;
  Graphics.moveto 99 581;
  Graphics.draw_string (Printf.sprintf " %s %s" header_text message);
;;

let draw_timeline_box clicked message =
  let text_color = if clicked then Graphics.cyan else 0x058BBD in
  Graphics.set_color text_color;
  Graphics.fill_rect 288 575 100 25;
  let header_text = "Months:" in
  Graphics.set_color Colors.black;
  Graphics.set_text_size 200;
  Graphics.moveto 293 581;
  Graphics.draw_string (Printf.sprintf " %s %d" header_text message);
;;

let draw_calculate_box clicked =
  let text_color = if clicked then Graphics.green else 0x06A217 in
  Graphics.set_color text_color;
  Graphics.fill_rect 482 575 100 25;
  let header_text = "Calculate" in
  Graphics.set_color Colors.black;
  Graphics.set_text_size 200;
  Graphics.moveto 499 581;
  Graphics.draw_string (Printf.sprintf " %s" header_text);
;;


let draw_play_area () =
  let open Constants in
  Graphics.set_color Colors.white;
  Graphics.fill_rect 0 0 gui_width gui_height
;;

let draw_graph (interface : Interface.t) = 
  Graphics.set_line_width 5;
  if (interface.calcBox) then (
    let edges : (int * int) array = [| (100,interface.graph.height); (100,50); (interface.graph.width, 50) |] in
    let graphPts = (interface.graph).data in
    Graphics.draw_poly_line graphPts;
    Graphics.draw_poly_line edges;
    Graphics.moveto 10 (interface.graph.height / 2);
    Graphics.draw_string "Sentiment";
    Graphics.moveto ((interface.graph.width / 2) + 50) 10;
    Graphics.draw_string "Months";
  ) else ()
;;

let render (interface : Interface.t) =
  (* We want double-buffering. See
     https://v2.ocaml.org/releases/4.03/htmlman/libref/Graphics.html for more
     info!

     So, we set [display_mode] to false, draw to the background buffer, set
     [display_mode] to true and then synchronize. This guarantees that there
     won't be flickering! *)
  Graphics.display_mode false;
  draw_header ();
  draw_play_area ();
  draw_ticker_box interface.tickerBox interface.input_ticker;
  draw_timeline_box interface.timeBox interface.input_timeframe;
  draw_calculate_box interface.calcBox;
  draw_graph interface;
  Graphics.display_mode true;
  Graphics.synchronize ()
;;


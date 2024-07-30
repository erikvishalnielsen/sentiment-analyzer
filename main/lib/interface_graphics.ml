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
    (Printf.sprintf " %dx%d" (gui_height + header_height) gui_width);
  Interface.create ()
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
  Graphics.moveto ((gui_width / 2) - 45) (gui_height + 25);
  Graphics.draw_string (Printf.sprintf " %s" header_text)
;;

let draw_ticker_box clicked message =
  let text_color = if clicked then Graphics.cyan else 0x058BBD in
  Graphics.set_color text_color;
  Graphics.fill_rect 94 575 100 25;
  let header_text = "Stock:" in
  Graphics.set_color Colors.black;
  Graphics.set_text_size 200;
  Graphics.moveto 99 581;
  Graphics.draw_string (Printf.sprintf " %s %s" header_text message)
;;

let draw_timeline_box clicked message =
  let text_color = if clicked then Graphics.cyan else 0x058BBD in
  Graphics.set_color text_color;
  Graphics.fill_rect 288 575 100 25;
  let header_text = "Days:" in
  Graphics.set_color Colors.black;
  Graphics.set_text_size 200;
  Graphics.moveto 293 581;
  Graphics.draw_string (Printf.sprintf " %s %d" header_text message)
;;

let draw_calculate_box clicked =
  let text_color = if clicked then Graphics.green else 0x06A217 in
  Graphics.set_color text_color;
  Graphics.fill_rect 482 575 100 25;
  let header_text = "Calculate" in
  Graphics.set_color Colors.black;
  Graphics.set_text_size 200;
  Graphics.moveto 499 581;
  Graphics.draw_string (Printf.sprintf " %s" header_text)
;;

let draw_play_area () =
  let open Constants in
  Graphics.set_color Colors.white;
  Graphics.fill_rect 0 0 gui_width gui_height
;;

let toJsonFile (interface : Interface.t) =
  (* Convert the list to a JSON value *)
  let json_value =
    `List
      (List.map
         ~f:(fun s ->
           `String
             (Int.to_string (fst s).days_from_beginning
              ^ " "
              ^ Finviz_parser.getStockDate (fst s)
              ^ " "
              ^ snd s))
         interface.finViz.headlines)
  in
  (* Convert the JSON value to a string *)
  let json_string = Yojson.Basic.to_string json_value in
  (* Write the JSON string to a file *)
  let write_to_file filename content =
    Out_channel.write_all filename ~data:content
  in
  (* Specify the output file path *)
  let output_file = interface.input_ticker ^ "_finviz.json" in
  (* Write the JSON string to the output file *)
  write_to_file output_file json_string;
  (* Optional: Print the JSON string to console *)
  print_endline ("JSON content:\n" ^ json_string)
;;

let draw_graph (interface : Interface.t) =
  (* let open Interface_lib__Finviz_parser in *)
  if interface.calcBox
  then (
    let open Constants in
    (* Graphics.set_color Colors.black; *)
    Graphics.moveto ((interface.graphSentiment.width / 2) + 50) (gui_height / 2 );
    Graphics.draw_string "Loading";
    (* let finVizData = Finviz_parser.create_finviz_parser
       interface.input_ticker interface.input_timeframe in interface.finViz
       <- finVizData; *)
    let edges : (int * int) array =
      [| 100, interface.graphSentiment.height; 100, 50; interface.graphSentiment.width, 50 |]
    in
    let line : (int * int) array =
      [| 100, (interface.graphSentiment.height + 50) / 2
       ; interface.graphSentiment.width, (interface.graphSentiment.height + 50) / 2
      |]
    in
    let graphPtsSent = Array.of_list (List.rev (Array.to_list interface.graphSentiment.data)) in
    let graphPtsPrice = Array.of_list (List.rev (Array.to_list interface.graphFinance.data)) in
    Graphics.set_line_width 5;
    Graphics.draw_poly_line edges;
    Graphics.draw_poly_line line;
    Graphics.set_line_width 3;
    Graphics.set_color Colors._green;
    Graphics.draw_poly_line graphPtsPrice;
    Graphics.set_color Colors._red;
    Graphics.draw_poly_line graphPtsSent;
    Graphics.set_color Colors.black;
    Graphics.moveto 10 (((interface.graphSentiment.height + 50) / 2) - 5);
    Graphics.draw_string "Price";
    (* Graphics.moveto 90 (((interface.graphSentiment.height + 50) / 2) - 5);
    Graphics.draw_string "0"; *)
    Graphics.moveto 55 (interface.graphSentiment.height - 5);
    Graphics.draw_string (Float.to_string (snd interface.graphHiLo));
    Graphics.moveto 55 50;
    Graphics.draw_string (Float.to_string (fst interface.graphHiLo));
    Graphics.moveto 100 30;
    Graphics.draw_string "0";
    Graphics.moveto (interface.graphSentiment.width - 5) 30;
    Graphics.draw_string (Int.to_string interface.input_timeframe);
    Graphics.moveto ((interface.graphSentiment.width / 2) + 50) 10;
    Graphics.draw_string "Days";
    Graphics.moveto ((interface.graphSentiment.width / 2) + 25) (interface.graphSentiment.height + 5);
    Graphics.draw_string (interface.input_ticker ^ " Sentiment Graph");
    
    Graphics.moveto 480 520;
    Graphics.set_color Colors._green;
    Graphics.draw_string ("Green: Price ");
    Graphics.moveto 575 520;
    Graphics.set_color Colors._red;
    Graphics.draw_string(" Red: Sentiment");

    (* CORRELATIONS *)
    Graphics.set_color Colors._blue;
    Graphics.moveto 510 475;
    Graphics.draw_string ("Price move 1 day before");
    Graphics.moveto 510 455;
    Graphics.draw_string ("Sentiment: " ^ (Float.to_string (Float.round_significant (List.nth_exn interface.correlations 0) ~significant_digits:(3))));
    Graphics.moveto 510 425;
    Graphics.draw_string ("Price move same day as");
    Graphics.moveto 510 405;
    Graphics.draw_string ("Sentiment: " ^ (Float.to_string (Float.round_significant (List.nth_exn interface.correlations 1) ~significant_digits:(3))));
    Graphics.moveto 510 375;
    Graphics.draw_string ("Price move 1 day after");
    Graphics.moveto 510 355;
    Graphics.draw_string ("Sentiment: " ^ (Float.to_string (Float.round_significant (List.nth_exn interface.correlations 2) ~significant_digits:(3))));
    Graphics.moveto 510 325;
    Graphics.draw_string ("Price move 2 days after");
    Graphics.moveto 510 305;
    Graphics.draw_string ("Sentiment: " ^ (Float.to_string (Float.round_significant (List.nth_exn interface.correlations 3) ~significant_digits:(3))));
    (*  *)
    Graphics.set_color Colors.white;
    Graphics.moveto ((interface.graphSentiment.width / 2) + 50) (gui_height / 2 );
    Graphics.draw_string "Loading";
    (*  *)
    let _img_link =
      "https://eodhd.com/img/logos/US/" ^ interface.input_ticker ^ ".png"
    in
    ())
  else ()
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
  Graphics.set_window_title "S&E Trading";
  (* Graphics.auto_synchronize true; *)
  
  draw_graph interface;
  Interface.check_error interface;
  Graphics.display_mode true;
  Graphics.synchronize ()
;;
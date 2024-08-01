open! Core

module Colors = struct
  let black = Graphics.rgb 000 000 000
  let bronze = Graphics.rgb 255 87 51
  let white = Graphics.rgb 255 255 255
  let _green = Graphics.rgb 000 255 000
  let _red = Graphics.rgb 255 000 000
  let _blue = Graphics.rgb 0 0 255
end

(* These constants are optimized for running on a low-resolution screen. Feel
   free to increase the scaling factor to tweak! *)
module Constants = struct
  let scaling_factor = 1.
  let gui_height = 900. *. scaling_factor |> Float.iround_down_exn
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
    (Printf.sprintf " %dx%d" gui_width (gui_height + header_height));
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

let draw_button (button : Interface.Button.t) message =
  let open Constants in
  let text_color = if button.on then button.clicked_color else button.reg_color in
  Graphics.set_color text_color;
  Graphics.fill_rect button.x button.y button.width button.height;
  let header_text = button.message in
  Graphics.set_color Colors.black;
  Graphics.set_text_size 200;
  Graphics.moveto (button.x + 5) (gui_height - 19);
  if String.equal message "" then 
    Graphics.draw_string (Printf.sprintf " %s" header_text) 
  else
    Graphics.draw_string (Printf.sprintf " %s %s" header_text message)
;;

let draw_calculate_box (button : Interface.Button.t) =
  let open Constants in
  let text_color = if button.on then Graphics.green else 0x06A217 in
  Graphics.set_color text_color;
  Graphics.fill_rect 482 (gui_height - 25) 100 25;
  let header_text = "Calculate" in
  Graphics.set_color Colors.black;
  Graphics.set_text_size 200;
  Graphics.moveto 499 (gui_height - 19);
  Graphics.draw_string (Printf.sprintf " %s" header_text)
;; 

let draw_play_area () =
  let open Constants in
  Graphics.set_color Colors.white;
  Graphics.fill_rect 0 0 (gui_width) gui_height
;;

let draw_graph (interface : Interface.t) =
  (* let open Interface_lib__Finviz_parser in *)
  if (Interface.calcBox interface).on
  then (
    let open Constants in
    (* Graphics.set_color Colors.black; *)
    let width = (Interface.graphSentiment interface).width in
    let height = (Interface.graphSentiment interface).height in
    Graphics.moveto (width / 2 + 50) (gui_height / 2 );
    Graphics.draw_string "Loading";
    (* let finVizData = Finviz_parser.create_finviz_parser
       interface.input_ticker interface.input_timeframe in interface.finViz
       <- finVizData; *)
    let edges : (int * int) array =
      [| 100, height + 300; 100, 300 + 50; width, 300 + 50 |]
    in
    let line : (int * int) array =
      [| 100, (height + 650) / 2
       ; width, (height + 650) / 2
      |]
    in
    let graphPtsSent = Array.of_list (List.rev (Array.to_list (Interface.graphSentiment interface).data)) in
    let graphPtsPrice = Array.of_list (List.rev (Array.to_list (Interface.graphFinance interface).data)) in
    Graphics.set_line_width 5;
    Graphics.draw_poly_line edges;
    Graphics.draw_poly_line line;
    Graphics.set_line_width 3;
    Graphics.set_color Colors._green;
    Graphics.draw_poly_line graphPtsPrice;
    Graphics.set_color Colors._red;
    Graphics.draw_poly_line graphPtsSent;
    Graphics.set_color Colors.black;
    Graphics.moveto 10 (((height + 650) / 2) - 5);
    Graphics.draw_string "Price";
    (* Graphics.moveto 90 (((interface.graphSentiment.height + 50) / 2) - 5);
    Graphics.draw_string "0"; *)
    Graphics.moveto 55 (height + 595);
    Graphics.draw_string (Float.to_string (snd (Interface.graphHiLo interface)));
    Graphics.moveto 55 (50 + 300);
    Graphics.draw_string (Float.to_string (fst (Interface.graphHiLo interface)));
    Graphics.moveto 100 (30 + 300);
    Graphics.draw_string "0";
    Graphics.moveto (width - 5) (30 + 300);
    Graphics.draw_string (Int.to_string (Interface.input_timeframe interface));
    Graphics.moveto ((width / 2) + 50) (10 + 300);
    Graphics.draw_string "Days";
    Graphics.moveto ((width / 2) + 25) (height + 305);
    Graphics.draw_string ((Interface.input_ticker interface) ^ " Sentiment Graph");
    
    Graphics.moveto 480 820;
    Graphics.set_color Colors._green;
    Graphics.draw_string ("Green: Price ");
    Graphics.moveto 575 820;
    Graphics.set_color Colors._red;
    Graphics.draw_string(" Red: Sentiment");

    (* CORRELATIONS *)
    Graphics.set_color Colors._blue;
    Graphics.moveto 510 775;
    Graphics.draw_string ("Price move 1 day before");
    Graphics.moveto 510 755;
    Graphics.draw_string ("Sentiment: " ^ (Float.to_string (Float.round_significant (List.nth_exn( Interface.correlations interface) 0) ~significant_digits:(3))));
    Graphics.moveto 510 725;
    Graphics.draw_string ("Price move same day as");
    Graphics.moveto 510 705;
    Graphics.draw_string ("Sentiment: " ^ (Float.to_string (Float.round_significant (List.nth_exn (Interface.correlations interface) 1) ~significant_digits:(3))));
    Graphics.moveto 510 675;
    Graphics.draw_string ("Price move 1 day after");
    Graphics.moveto 510 655;
    Graphics.draw_string ("Sentiment: " ^ (Float.to_string (Float.round_significant (List.nth_exn (Interface.correlations interface) 2) ~significant_digits:(3))));
    Graphics.moveto 510 625;
    Graphics.draw_string ("Price move 2 days after");
    Graphics.moveto 510 605;
    Graphics.draw_string ("Sentiment: " ^ (Float.to_string (Float.round_significant (List.nth_exn (Interface.correlations interface) 3) ~significant_digits:(3))));
    
    Graphics.moveto 510 550;
    let regStr = (match (Interface.regressionEqtn interface) with
    | Some eqtn -> Regression.eqtnToString eqtn 
    | None -> ("-1/0 Days:", "Price Leading Useless")) in
    Graphics.set_color Colors.bronze;
    Graphics.draw_string ("Best Linear Eqtn @ " ^ (fst regStr));
    Graphics.moveto 510 530;
    Graphics.draw_string (snd regStr);
    let predStr = (match (Interface.regressionEqtn interface) with
    | Some eqtn -> (Regression.predictionToString eqtn)
    | None -> ("Prediction Not Possible", "When Price is Leading")) in 
    Graphics.moveto 510 500;
    Graphics.draw_string (fst predStr);
    Graphics.moveto 510 480;
    Graphics.draw_string (snd predStr);

    (* AI INFORMATION *)
    let ai_answers = Interface.graphInfo interface in
    let height_start = 280 in
    let _x = List.init (List.length ai_answers) ~f:(fun num ->
      let minus = num * 20 in
      Graphics.moveto 10 (height_start-minus);
      Graphics.draw_string (List.nth_exn ai_answers num);
    ) in


    (*  *)
    Graphics.set_color Colors.white;
    Graphics.moveto ((width / 2) + 50) (gui_height / 2 );
    Graphics.draw_string "Loading";
    (*  *)
    let _img_link =
      "https://eodhd.com/img/logos/US/" ^ (Interface.input_ticker interface) ^ ".png"
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
  draw_button (Interface.tickerBox interface) (Interface.input_ticker interface);
  draw_button (Interface.timeBox interface) (Int.to_string (Interface.input_timeframe interface));
  draw_button (Interface.calcBox interface) "";
  Graphics.set_window_title "S&E Trading";
  (* Graphics.auto_synchronize true; *)
  
  draw_graph interface;
  Interface.check_error interface;
  Graphics.display_mode true;
  Graphics.synchronize ()
;;
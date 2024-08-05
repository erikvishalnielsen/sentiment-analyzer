open! Core

module Graph = struct
  type t =
    { height : int
    ; width : int
    ; data : (int * int) array
    }
  [@@deriving sexp_of]
end

module Colors = struct
  let black = Graphics.rgb 000 000 000
  let white = Graphics.rgb 255 255 255
end

module Rectangle = struct
  type t =
    { x : int
    ; y : int
    ; width : int
    ; height : int
    ; mutable on : bool
    ; reg_color : int
    ; clicked_color : int
    }
  [@@deriving sexp_of]

  let draw_rectangle (t : t) =
    let box_color = if t.on then t.clicked_color else t.reg_color in
    Graphics.set_color box_color;
    Graphics.fill_rect t.x t.y t.width t.height
  ;;
end

module Button = struct
  type t =
    { rectangle : Rectangle.t
    ; button_text : string
    }
    [@@deriving sexp_of]

  let draw_button (t : t) =
    Rectangle.draw_rectangle t.rectangle;
    let button_text = t.button_text in
    Graphics.set_color Colors.black;
    Graphics.moveto
      (t.rectangle.x
       - (fst (Graphics.text_size button_text) / 2)
       + Int.of_float (34.0 *. (Float.of_int t.rectangle.width /. 80.)))
      (t.rectangle.y + 6);
    Graphics.draw_string (Printf.sprintf " %s" button_text)
  ;;
end

module Textbox = struct
  type t =
    { rectangle : Rectangle.t
    ; textbox_text : string
    ; mutable message : string
    }
    [@@deriving sexp_of]

  let draw_textbox (t : t) =
    Rectangle.draw_rectangle t.rectangle;
    Graphics.set_color Colors.black;
    Graphics.set_text_size 200;
    Graphics.moveto (t.rectangle.x + 5) (t.rectangle.y + 6);
    Graphics.draw_string (Printf.sprintf " %s %s" t.textbox_text t.message)
  ;;
end

type t =
  { mutable graphFinance : Graph.t
  ; mutable graphHiLo : float * float
  ; mutable graphSentiment : Graph.t
  ; mutable graphVolume : Graph.t
  ; mutable ticker_textbox : Textbox.t
  ; mutable time_textbox : Textbox.t
  ; mutable calc_button : Button.t
  ; mutable price_button : Button.t
  ; mutable sentiment_button : Button.t
  ; mutable volume_button : Button.t
  ; mutable displayError : string
  ; mutable correlations : float list
  ; mutable regressionEqtn : Regression.t option
  ; mutable graphInfo : string list
  }
[@@deriving sexp_of, fields]

let create_graph (data : (int * int) array) : Graph.t =
  { height = 500; width = 500; data }
;;

let create () =
  let data =
    Array.init 5 ~f:(fun num -> (num * 100) + 100, ((num + 5) * 25) + 50)
  in
  let interface =
    { graphFinance = create_graph data
    ; graphHiLo = 0.0, 0.0
    ; graphSentiment = create_graph data
    ; graphVolume = create_graph data
    ; ticker_textbox =
        { rectangle =
            { x = 94
            ; y = 875
            ; width = 100
            ; height = 25
            ; on = false
            ; reg_color = 0x058BBD
            ; clicked_color = 0x00FFFF
            }
        ; textbox_text = "Ticker:"
        ; message = ""
        }
    ; time_textbox =
        { rectangle =
            { x = 288
            ; y = 875
            ; width = 100
            ; height = 25
            ; on = false
            ; reg_color = 0x058BBD
            ; clicked_color = 0x00FFFF
            }
        ; textbox_text = "Days:"
        ; message = "0"
        }
    ; calc_button =
        { rectangle =
            { x = 482
            ; y = 875
            ; width = 100
            ; height = 25
            ; on = false
            ; reg_color = 0x06A217
            ; clicked_color = 0x00FF00
            }
        ; button_text = "Calculate"
        }
    ; price_button =
        { rectangle =
            { x = 550
            ; y = 430
            ; width = 80
            ; height = 25
            ; on = true
            ; reg_color = 0x06A217
            ; clicked_color = 0x00FF00
            }
        ; button_text = "Price"
        }
    ; sentiment_button =
        { rectangle =
            { x = 550
            ; y = 390
            ; width = 80
            ; height = 25
            ; on = true
            ; reg_color = 0x8B0000
            ; clicked_color = Graphics.red
            }
        ; button_text = "Sentiment"
        }
    ; volume_button =
        { rectangle =
            { x = 550
            ; y = 350
            ; width = 80
            ; height = 25
            ; on = true
            ; reg_color = Graphics.blue
            ; clicked_color = 0x00FFFF
            }
        ; button_text = "Volume"
        }
    ; displayError =
        ""
        (* ; finViz = { stock_ticker = "" ; time_period = 0 ; link = "" ;
           headlines = [ Finviz_parser.get_date "Jan-01-24", "" ] } *)
    ; correlations = []
    ; regressionEqtn = None
    ; graphInfo = []
    }
  in
  interface
;;

let get_list_of_widths numPts =
  let dist = 400.0 /. Int.to_float (numPts - 1) in
  List.init numPts ~f:(fun num ->
    100 + Int.of_float (Int.to_float num *. dist))
;;

let plot_datapoints (datum : Datapoints.t) =
  (* Get length of the data list *)
  (* Decide how I want to scale the price *)
  (* The Sentiment Score should just be scaled by 100 *)
  (* Create a function that takes the number of total points and gets the
     width between them *)
  let numPts = List.length datum.data in
  let width_list = get_list_of_widths numPts in
  Core.print_s
    [%message
      "highs/lows: "
        (Float.to_string datum.price_high)
        (Float.to_string datum.price_low)];
  let tickSize = 450.0 /. (datum.price_high -. datum.price_low) in
  let tickVolume = 450.0 /. (datum.deltavol_high -. datum.deltavol_low) in
  Core.print_s
    [%message
      "data: " (Float.to_string tickSize) (Float.to_string datum.price_low)];
  Core.print_s
    [%message
      "data volume: "
        (Float.to_string tickVolume)
        (Float.to_string datum.deltavol_low)];
  let height_multiplier_price price =
    350 + Int.of_float ((price -. datum.price_low) *. tickSize)
  in
  let height_multiplier_vol vol =
    350 + Int.of_float ((vol -. datum.deltavol_low) *. tickVolume)
  in
  let height_multiplier_sent sentiment =
    575 + Int.of_float (225.0 *. sentiment)
  in
  let pointListPrice = Array.init numPts ~f:(fun _f -> 0, 0) in
  let pointListSentiment = Array.init numPts ~f:(fun _f -> 0, 0) in
  let pointListVolume = Array.init numPts ~f:(fun _f -> 0, 0) in
  List.iteri datum.data ~f:(fun ind point ->
    Array.set
      pointListPrice
      ind
      (List.nth_exn width_list ind, height_multiplier_price point.price);
    Array.set
      pointListSentiment
      ind
      (List.nth_exn width_list ind, height_multiplier_sent point.sentiment);
    Array.set
      pointListVolume
      ind
      (List.nth_exn width_list ind, height_multiplier_vol point.delta_volume));
  pointListPrice, pointListSentiment, pointListVolume
;;

let first_element (x, _, _) = x
let second_element (_, y, _) = y
let third_element (_, _, z) = z

let handle_click (t : t) (pos : int * int) =
  let x_pos = fst pos in
  let y_pos = snd pos in
  (* Calculate: 482 575 100 25 *)
  if x_pos >= 482 && x_pos <= 582 && y_pos >= 875 && y_pos <= 900
  then (
    Graphics.set_color (Graphics.rgb 0 0 0);
    Graphics.moveto ((t.graphSentiment.width / 2) + 50) 600;
    Graphics.draw_string "Loading...";
    t.calc_button.rectangle.on
    <- (let todayDate = Date.today ~zone:Timezone.utc in
        Stock_day.createFindlJson
          t.ticker_textbox.message
          ~startDate:
            (Stock_day.convert_date_tostring
               (Date.add_days todayDate (-1 * (Int.of_string t.time_textbox.message))))
          ~endDate:
            (Stock_day.convert_date_tostring (Date.add_days todayDate (-1)))
          ~max_search:
            (Stock_day.convert_date_tostring
               (Date.add_days todayDate (-180)))
          ~total_days:(t.time_textbox.message);
        match
          Datapoints.json_to_datapoints t.ticker_textbox.message (Int.of_string t.time_textbox.message)
        with
        | Ok datapoints ->
          let correlations, regressionEqtn =
            Regression.regressionCorrelation datapoints
          in
          t.correlations <- correlations;
          t.regressionEqtn <- regressionEqtn;
          t.graphInfo <- datapoints.gemini_ans;
          t.graphHiLo <- datapoints.price_low, datapoints.price_high;
          let datapair
            : (int * int) array * (int * int) array * (int * int) array
            =
            plot_datapoints datapoints
          in
          t.graphFinance
          <- { height = 500; width = 500; data = first_element datapair };
          t.graphSentiment
          <- { height = 500; width = 500; data = second_element datapair };
          t.graphVolume
          <- { height = 500; width = 500; data = third_element datapair };
          Graphics.set_color (Graphics.rgb 255 255 255);
          Graphics.moveto ((t.graphSentiment.width / 2) + 50) 600;
          Graphics.draw_string "Loading";
          true
        | Error error ->
          t.displayError <- Error.to_string_hum error;
          false);
    t.ticker_textbox.rectangle.on <- false;
    t.ticker_textbox.rectangle.on <- false;
    Core.print_s [%message "calcbox"] (* Ticker: 94 575 100 25 *))
  else if x_pos >= 94 && x_pos <= 194 && y_pos >= 875 && y_pos <= 900
  then (
    t.calc_button.rectangle.on <- false;
    t.ticker_textbox.rectangle.on <- true;
    t.time_textbox.rectangle.on <- false;
    if not (String.equal t.displayError "")
    then (
      Graphics.set_color (Graphics.rgb 255 255 255);
      Graphics.moveto ((t.graphSentiment.width / 2) + 50) 300;
      Graphics.draw_string t.displayError);
    t.displayError <- "";
    Core.print_s [%message "tickerbox"] (* Timeline: 288 575 100 25 *))
  else if x_pos >= 288 && x_pos <= 384 && y_pos >= 875 && y_pos <= 900
  then (
    t.calc_button.rectangle.on <- false;
    t.ticker_textbox.rectangle.on <- false;
    t.time_textbox.rectangle.on <- true;
    if not (String.equal t.displayError "")
    then (
      Graphics.set_color (Graphics.rgb 255 255 255);
      Graphics.moveto ((t.graphSentiment.width / 2) + 50) 300;
      Graphics.draw_string t.displayError);
    t.displayError <- "";
    Core.print_s [%message "timebox"])
else if x_pos >= t.price_button.rectangle.x && x_pos <= t.price_button.rectangle.x + t.price_button.rectangle.width && y_pos >= t.price_button.rectangle.y && y_pos <= t.price_button.rectangle.y + t.price_button.rectangle.height then
  (
    if (t.price_button.rectangle.on) then t.price_button.rectangle.on <- false else t.price_button.rectangle.on <- true
  )
else if x_pos >= t.sentiment_button.rectangle.x && x_pos <= t.sentiment_button.rectangle.x + t.sentiment_button.rectangle.width && y_pos >= t.sentiment_button.rectangle.y && y_pos <= t.sentiment_button.rectangle.y + t.sentiment_button.rectangle.height then
  (
    if (t.sentiment_button.rectangle.on) then t.sentiment_button.rectangle.on <- false else t.sentiment_button.rectangle.on <- true
  )
else if x_pos >= t.volume_button.rectangle.x && x_pos <= t.volume_button.rectangle.x + t.volume_button.rectangle.width && y_pos >= t.volume_button.rectangle.y && y_pos <= t.volume_button.rectangle.y + t.volume_button.rectangle.height then
  (
    if (t.volume_button.rectangle.on) then t.volume_button.rectangle.on <- false else t.volume_button.rectangle.on <- true
  )
  else (
    (* t.calcBox <- t.calcBox; *)
    t.ticker_textbox.rectangle.on <- false;
    t.time_textbox.rectangle.on <- false;
    Core.print_s [%message "nothing"])
;;

let check_error t =
  if not (String.equal t.displayError "")
  then Graphics.set_color (Graphics.rgb 220 20 60);
  Graphics.moveto ((t.graphSentiment.width / 2) + 50) 600;
  Graphics.draw_string t.displayError
;;


open! Core

module Graph = struct
  type t =
    { height : int
    ; width : int
    ; data : (int * int) array
    }
  [@@deriving sexp_of]
end

module Button = struct
  type t =
    { x : int
    ; y : int
    ; width : int
    ; height : int
    ; mutable on : bool
    ; message : string
    ; reg_color : int
    ; clicked_color : int
    }
  [@@deriving sexp_of]
end

type t =
  { mutable input_ticker : string
  ; mutable input_timeframe : int
  ; mutable graphFinance : Graph.t
  ; mutable graphHiLo : float * float
  ; mutable graphSentiment : Graph.t
  ; mutable graphVolume : Graph.t
  ; mutable tickerBox : Button.t
  ; mutable timeBox : Button.t
  ; mutable calcBox : Button.t
  ; mutable displayError : string
      (* ; mutable finViz : Finviz_parser.Finviz_parser.t *)
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
    { input_ticker = ""
    ; input_timeframe = 0
    ; graphFinance = create_graph data
    ; graphHiLo = 0.0, 0.0
    ; graphSentiment = create_graph data
    ; graphVolume = create_graph data
    ; tickerBox =
        { x = 94
        ; y = 875
        ; width = 100
        ; height = 25
        ; on = false
        ; message = "Ticker:"
        ; reg_color = 0x058BBD
        ; clicked_color = 0x00FFFF
        }
    ; timeBox =
        { x = 288
        ; y = 875
        ; width = 100
        ; height = 25
        ; on = false
        ; message = "Days:"
        ; reg_color = 0x058BBD
        ; clicked_color = 0x00FFFF
        }
    ; calcBox =
        { x = 482
        ; y = 875
        ; width = 100
        ; height = 25
        ; on = false
        ; message = "Calculate"
        ; reg_color = 0x06A217
        ; clicked_color = 0x00FF00
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
    t.calcBox.on
    <- (let todayDate = Date.today ~zone:Timezone.utc in
        Stock_day.createFindlJson
          t.input_ticker
          ~startDate:
            (Stock_day.convert_date_tostring
               (Date.add_days todayDate (-1 * t.input_timeframe)))
          ~endDate:
            (Stock_day.convert_date_tostring (Date.add_days todayDate (-1)))
          ~max_search:
            (Stock_day.convert_date_tostring
               (Date.add_days todayDate (-180)));
        match
          Datapoints.json_to_datapoints t.input_ticker t.input_timeframe
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
    t.tickerBox.on <- false;
    t.timeBox.on <- false;
    Core.print_s [%message "calcbox"] (* Ticker: 94 575 100 25 *))
  else if x_pos >= 94 && x_pos <= 194 && y_pos >= 875 && y_pos <= 900
  then (
    t.calcBox.on <- false;
    t.tickerBox.on <- true;
    t.timeBox.on <- false;
    if not (String.equal t.displayError "")
    then (
      Graphics.set_color (Graphics.rgb 255 255 255);
      Graphics.moveto ((t.graphSentiment.width / 2) + 50) 300;
      Graphics.draw_string t.displayError);
    t.displayError <- "";
    Core.print_s [%message "tickerbox"] (* Timeline: 288 575 100 25 *))
  else if x_pos >= 288 && x_pos <= 384 && y_pos >= 875 && y_pos <= 900
  then (
    t.calcBox.on <- false;
    t.tickerBox.on <- false;
    t.timeBox.on <- true;
    if not (String.equal t.displayError "")
    then (
      Graphics.set_color (Graphics.rgb 255 255 255);
      Graphics.moveto ((t.graphSentiment.width / 2) + 50) 300;
      Graphics.draw_string t.displayError);
    t.displayError <- "";
    Core.print_s [%message "timebox"])
  else (
    t.calcBox <- t.calcBox;
    t.tickerBox.on <- false;
    t.timeBox.on <- false;
    Core.print_s [%message "nothing"])
;;

let check_error t =
  if not (String.equal t.displayError "")
  then Graphics.set_color (Graphics.rgb 220 20 60);
  Graphics.moveto ((t.graphSentiment.width / 2) + 50) 600;
  Graphics.draw_string t.displayError
;;

let set_input_timeframe t timeframe = t.input_timeframe <- timeframe
let set_input_ticker t ticker = t.input_ticker <- ticker

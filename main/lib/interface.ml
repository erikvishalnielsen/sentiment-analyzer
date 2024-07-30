open! Core

module Graph = struct
  type t =
    { height : int
    ; width : int
    ; data : (int * int) array
    }
  [@@deriving sexp_of]
end

type t =
  { mutable input_ticker : string
  ; mutable input_timeframe : int
  ; mutable graphFinance : Graph.t
  ; mutable graphHiLo : (float * float)
  ; mutable graphSentiment : Graph.t
  ; mutable tickerBox : bool
  ; mutable timeBox : bool
  ; mutable calcBox : bool
  ; mutable displayError : string
  ; mutable finViz : Finviz_parser.Finviz_parser.t
  ; mutable correlations : float list
  ; mutable regressionEqtn : Regression.t option
  }
[@@deriving sexp_of]

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
    ; graphHiLo = (0.0,0.0)
    ; graphSentiment = create_graph data
    ; tickerBox = false
    ; timeBox = false
    ; calcBox = false
    ; displayError = ""
    ; finViz =
        { stock_ticker = ""
        ; time_period = 0
        ; link = ""
        ; headlines = [ Finviz_parser.get_date "Jan-01-24", "" ]
        }
    ; correlations = []
    ; regressionEqtn = None
    }
  in
  interface
;;


let get_list_of_widths numPts = 
  let dist = 400.0 /. (Int.to_float (numPts - 1)) in
  List.init numPts ~f:(fun num -> (100 + (Int.of_float ((Int.to_float num) *. dist))))
;;

let plot_datapoints (datum : Datapoints.t) = 
  (* Get length of the data list *)
  (* Decide how I want to scale the price *)
  (* The Sentiment Score should just be scaled by 100 *)
  (* Create a function that takes the number of total points and gets the width between them *)

  let numPts = List.length datum.data in
  let width_list = get_list_of_widths numPts in


  Core.print_s [%message "highs/lows: " (Float.to_string datum.price_high) (Float.to_string datum.price_low)];
  let tickSize = 450.0 /. (datum.price_high -. datum.price_low) in
  Core.print_s [%message "data: " (Float.to_string tickSize) (Float.to_string datum.price_low)];
  let height_multiplier_price price = (50 + (Int.of_float ((price -. datum.price_low) *. tickSize))) in
  let height_multiplier_sent sentiment = (275 + (Int.of_float (225.0 *. sentiment))) in

  let pointListPrice = Array.init numPts ~f:(fun _f -> (0,0)) in
  let pointListSentiment = Array.init numPts ~f:(fun _f -> (0,0)) in
  List.iteri datum.data ~f:(fun ind point -> 
    Array.set pointListPrice ind ((List.nth_exn width_list ind), (height_multiplier_price point.price));
    Array.set pointListSentiment ind ((List.nth_exn width_list ind), (height_multiplier_sent point.sentiment));
  );

  (pointListPrice, pointListSentiment)
;;

let handle_click (t : t) (pos : int * int) =
  let x_pos = fst pos in
  let y_pos = snd pos in
  (* Calculate: 482 575 100 25 *)
  if x_pos >= 482 && x_pos <= 582 && y_pos >= 575 && y_pos <= 600
  then (
    Graphics.set_color (Graphics.rgb 0 0 0);
    Graphics.moveto ((t.graphSentiment.width / 2) + 50) (300);
    Graphics.draw_string "Loading...";
    t.calcBox
    <- 
    (let todayDate = Date.today ~zone:Timezone.utc in
        Finviz_parser.createFindlJson
          t.input_ticker
          ~startDate:
            (Finviz_parser.convert_date_tostring
               (Date.add_days todayDate (-1 * t.input_timeframe)))
          ~endDate:
            (Finviz_parser.convert_date_tostring
               (Date.add_days todayDate (-1)))
          ~max_search:
            (Finviz_parser.convert_date_tostring
               (Date.add_days todayDate (-180)));
        
        match Datapoints.json_to_datapoints (t.input_ticker) (t.input_timeframe) with 
        | Ok datapoints ->
        let (correlations, regressionEqtn) = (Regression.regressionCorrelation datapoints) in
        t.correlations <- correlations;
        t.regressionEqtn <- regressionEqtn;
        t.graphHiLo <- (datapoints.price_low, datapoints.price_high);
        let datapair : ((int * int) array * (int * int) array) = plot_datapoints datapoints in 
        t.graphFinance <- {height = 500; width = 500; data = fst datapair};
        t.graphSentiment <- {height = 500; width = 500; data = snd datapair};
        Graphics.set_color (Graphics.rgb 255 255 255);
        Graphics.moveto ((t.graphSentiment.width / 2) + 50) (300);
        Graphics.draw_string "Loading";
        true
        | Error error -> t.displayError <- Error.to_string_hum error;
          false);
    t.tickerBox <- false;
    t.timeBox <- false;
    Core.print_s [%message "calcbox"] (* Ticker: 94 575 100 25 *))
  else if x_pos >= 94 && x_pos <= 194 && y_pos >= 575 && y_pos <= 600
  then (
    t.calcBox <- false;
    t.tickerBox <- true;
    t.timeBox <- false;
    if (not (String.equal t.displayError "")) then 
      (Graphics.set_color (Graphics.rgb 255 255 255);
      Graphics.moveto ((t.graphSentiment.width / 2) + 50) 300;
      Graphics.draw_string t.displayError);
    t.displayError <- "";
    Core.print_s [%message "tickerbox"] (* Timeline: 288 575 100 25 *))
  else if x_pos >= 288 && x_pos <= 384 && y_pos >= 575 && y_pos <= 600
  then (
    t.calcBox <- false;
    t.tickerBox <- false;
    t.timeBox <- true;
    if (not (String.equal t.displayError "")) then 
      (Graphics.set_color (Graphics.rgb 255 255 255);
      Graphics.moveto ((t.graphSentiment.width / 2) + 50) 300;
      Graphics.draw_string t.displayError);
    t.displayError <- "";
    Core.print_s [%message "timebox"])
  else (
    t.calcBox <- t.calcBox;
    t.tickerBox <- false;
    t.timeBox <- false;
    Core.print_s [%message "nothing"])
;;

let check_error t =
  if (not (String.equal t.displayError "")) then 
  Graphics.set_color (Graphics.rgb 220 20 60);
        Graphics.moveto ((t.graphSentiment.width / 2) + 50) (300);
        Graphics.draw_string t.displayError;


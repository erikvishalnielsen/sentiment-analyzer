open! Core
open Yojson.Basic.Util
open Core_unix

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
  ; mutable bid_textbox : Textbox.t
  ; mutable ask_textbox : Textbox.t
  ; mutable calc_button : Button.t
  ; mutable price_button : Button.t
  ; mutable sentiment_button : Button.t
  ; mutable volume_button : Button.t
  ; mutable submit_button : Button.t
  ; mutable check_button : Button.t
  ; mutable displayError : string
  ; mutable correlations : float list
  ; mutable regressionEqtn : Regression.t option
  ; mutable graphInfo : string list
  ; mutable datapts : Datapoints.t
  ; mutable receiptText : (bool * string)
  ; mutable guessText : (bool * string)
  ; mutable checker_textbox : Textbox.t 
  ; mutable earnings_live_button : Button.t
  ; mutable earnings_link_text : Textbox.t
  ; mutable earnings_link_submit : Button.t
  ; mutable live_channel : In_channel.t option [@sexp.opaque]
  ; mutable process : Process_info.t option
  }
[@@deriving fields]

let run_python_script_concurrently t script_name link =
  let process = (create_process ~prog:"/bin/python3" ~args:[script_name; link]) in
  t.process <- Some process;
  t.live_channel <- Some (in_channel_of_descr process.stdout);
;;

let close_python_script t = 
  match t.live_channel with 
  | Some channel -> In_channel.close channel
  | None -> ()
;;

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
    ; bid_textbox =
        { rectangle =
            { x = 50
            ; y = 50
            ; width = 100
            ; height = 25
            ; on = false
            ; reg_color = 0xF8A30A
            ; clicked_color = 0xFFD96A
            }
        ; textbox_text = "Bid:"
        ; message = "0"
        }
    ; ask_textbox =
    { rectangle =
        { x = 200
        ; y = 50
        ; width = 100
        ; height = 25
        ; on = false
        ; reg_color = 0xF8A30A
        ; clicked_color = 0xFFD96A
        }
      ; textbox_text = "Ask:"
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
            ; clicked_color = 0x90EE90
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
            ; reg_color = 0xD70040
            ; clicked_color = 0xF88379
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
            ; reg_color = 0x6495ED
            ; clicked_color = 0x7DF9FF
            }
        ; button_text = "Volume"
        }
    ; submit_button =
    { rectangle =
        { x = 350
        ; y = 50
        ; width = 100
        ; height = 25
        ; on = false
        ; reg_color = 0xDB1456
        ; clicked_color = 0xED4A80
        }
    ; button_text = "Submit Guess"
    }
    ; check_button =
    { rectangle =
        { x = 350
        ; y = 15
        ; width = 100
        ; height = 25
        ; on = false
        ; reg_color = 0xDB1456
        ; clicked_color = 0xED4A80
        }
    ; button_text = "Check Winner"
    }
    ; displayError =
        ""
        (* ; finViz = { stock_ticker = "" ; time_period = 0 ; link = "" ;
           headlines = [ Finviz_parser.get_date "Jan-01-24", "" ] } *)
    ; correlations = []
    ; regressionEqtn = None
    ; graphInfo = []
    ; datapts = { Datapoints.data = [] ; price_high = 0.0 ; price_low = 0.0 ; deltavol_high = 0.0 ; deltavol_low = 0.0 ; gemini_ans = [] }
    ; receiptText = (false, "")
    ; guessText = (false, "")
    ; checker_textbox = { rectangle =
      { x = 50
      ; y = 15
      ; width = 250
      ; height = 25
      ; on = false
      ; reg_color = 0xCF29BB
      ; clicked_color = 0xFA73EA
      }
    ; textbox_text = "Receipt #:"
    ; message = "0"
    }
    ; earnings_live_button =
    { rectangle =
        { x = 268
        ; y = 840
        ; width = 140
        ; height = 25
        ; on = false
        ; reg_color = 0xDB1456
        ; clicked_color = 0xED4A80
        }
      ; button_text = "Earnings Call Live"
    }
    ; earnings_link_text = 
    { rectangle =
      { x = 50
      ; y = 800
      ; width = 475
      ; height = 25
      ; on = false
      ; reg_color = 0xCF29BB
      ; clicked_color = 0xFA73EA
      }
    ; textbox_text = "YouTube Link:"
    ; message = ""
    }
    ; earnings_link_submit =
    { rectangle =
        { x = 550
        ; y = 800
        ; width = 100
        ; height = 25
        ; on = false
        ; reg_color = 0xDB1456
        ; clicked_color = 0xED4A80
        }
      ; button_text = "Submit Link"
    }
    ; live_channel = None
    ; process = None
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

let generate_random_8_digit_key () : int =
  let min_key = 10000000 in
  let max_key = 99999999 in
  Random.int (max_key - min_key + 1) + min_key
;;

let json_to_hashtbl (json : Yojson.Basic.t) : (int, float list) Hashtbl.t =
  (* Convert JSON associative list to OCaml list *)
  let assoc_list = json |> to_assoc in
  (* Create a new hashtable *)
  let tbl = Hashtbl.create (module Int) in
  (* Populate the hashtable *)
  List.iter assoc_list ~f:(fun (key, value) ->
    let int_key = int_of_string key in
    let float_value = List.map (to_list (value)) ~f:(fun elt -> to_float elt) in
    Hashtbl.add_exn tbl ~key:int_key ~data:float_value;
  );
  tbl
;;

let hashtbl_to_yojson (tbl: (int, float list) Hashtbl.t) : Yojson.Basic.t =
  (* Convert hashtable to associative list *)
  let kv_list = Hashtbl.fold ~f:(fun ~key ~data:value acc -> acc @ [(key, value)]) tbl ~init:[] in
  (* Convert associative list to JSON associative list *)
  `Assoc (List.map kv_list ~f:(fun (k, v) -> (string_of_int k, `List (List.map v ~f:(fun elt -> `Float elt)))))
;;

let handle_submit (t : t) (latest_price : float) = 
  let spread = latest_price *. 0.03 in
  let model_bid_ask = (match t.regressionEqtn with 
  | None -> [latest_price -. spread; latest_price +. spread]
  | Some reg -> [Float.round_decimal ~decimal_digits:2 (reg.one_day_prediction -. spread); Float.round_decimal ~decimal_digits:2 (reg.one_day_prediction +. spread)]) in
  let user_bid_ask = [Float.round_decimal ~decimal_digits:2 (Float.of_string t.bid_textbox.message); Float.round_decimal ~decimal_digits:2 (Float.of_string t.ask_textbox.message)] in
  
  let _todayBusinessDate = Date.to_string (Date.add_business_days_rounding_backward ~is_holiday:(Date.is_weekend) (Date.today ~zone:Timezone.utc) 0) in
  let tomorrowBusinessDate = Date.to_string (Date.add_business_days_rounding_backward ~is_holiday:(Date.is_weekend) (Date.today ~zone:Timezone.utc) 1) in
  let filename = tomorrowBusinessDate ^ "_predictions.json" in
  let mapKey = generate_random_8_digit_key () in

  (match (Sys_unix.file_exists filename) with 
  | `No -> (
    let oc = Out_channel.create filename in
    let hash = Hashtbl.create (module Int) in
    Hashtbl.add_exn hash ~key:mapKey ~data:(model_bid_ask @ user_bid_ask);
    Yojson.Basic.pretty_to_channel oc (hashtbl_to_yojson hash);
  )
  | `Yes -> (
    let jsonData = Yojson.Basic.from_file filename in
    let currHash = json_to_hashtbl jsonData in
    Hashtbl.add_exn currHash ~key:mapKey ~data:(model_bid_ask @ user_bid_ask);
    Sys_unix.remove filename;
    let oc = Out_channel.create filename in
    Yojson.Basic.pretty_to_channel oc (hashtbl_to_yojson currHash);
  )
  | `Unknown -> failwith "erroneus error");

  let output_str = ("Save this key for tomorrow: " ^ (Int.to_string mapKey)) in
  output_str
;;

let handle_receipt (t : t) (receiptKey : string) (latest_price : float) = 
  let todayBusinessDate = Date.to_string (Date.add_business_days_rounding_backward ~is_holiday:(Date.is_weekend) (Date.today ~zone:Timezone.utc) 0) in
  let filename = todayBusinessDate ^ "_predictions.json" in

  let output = (match (Sys_unix.file_exists filename) with 
  | `No -> (
    "No Predictions for Yesterday"
  )
  | `Yes -> (
    let jsonData = Yojson.Basic.from_file filename in
    let currHash = json_to_hashtbl jsonData in
    let x = Hashtbl.find currHash (Int.of_string receiptKey) in
    match x with 
    | Some preds -> (
      (* FIRST TWO ARE COMPUTER, SECOND TWO ARE USER *)
      let compCorrect = (Float.(>) latest_price (List.nth_exn preds 0) && Float.(<) latest_price (List.nth_exn preds 1)) in
      let userCorrect = (Float.(>) latest_price (List.nth_exn preds 2) && Float.(<) latest_price (List.nth_exn preds 3)) in
      if(compCorrect && not userCorrect) then 
        "Computer correct. User incorrect."
      else if(userCorrect && not compCorrect) then 
        "User correct. Computer incorrect."
      else if(not userCorrect && not compCorrect) then 
        "Both user and computer incorrect."
      else 
        (
        let userSpread = (List.nth_exn preds 3) -. (List.nth_exn preds 2) in
        let compSpread = (List.nth_exn preds 1) -. (List.nth_exn preds 0) in
        if(Float.(>) userSpread compSpread) then "Computer wins with tighter spread." else "User wins with tighter spread."
        );
    )
    | None -> "Receipt Number Doesn't Exist"
  )
  | `Unknown -> failwith "erroneus error") in

  output
;;

let handle_click (t : t) (pos : int * int) =
  let x_pos = fst pos in
  let y_pos = snd pos in
  (* let datapts : Datapoints.t ref = ref ({ Datapoints.data = [] ; price_high = 0.0 ; price_low = 0.0 ; deltavol_high = 0.0 ; deltavol_low = 0.0 ; gemini_ans = [] }) in *)
  (* Calculate: 482 575 100 25 *)
  if x_pos >= t.earnings_live_button.rectangle.x && x_pos <= t.earnings_live_button.rectangle.x + t.earnings_live_button.rectangle.width && y_pos >= t.earnings_live_button.rectangle.y && y_pos <= t.earnings_live_button.rectangle.y + t.earnings_live_button.rectangle.height then
  (
    if not (t.earnings_live_button.rectangle.on) then (
      t.earnings_live_button.rectangle.on <- true;
      t.calc_button.rectangle.on <- false
      )
  ) else if x_pos >= t.earnings_link_text.rectangle.x && x_pos <= t.earnings_link_text.rectangle.x + t.earnings_link_text.rectangle.width && y_pos >= t.earnings_link_text.rectangle.y && y_pos <= t.earnings_link_text.rectangle.y + t.earnings_link_text.rectangle.height then
    (
      if not (t.earnings_link_text.rectangle.on) then (
        t.earnings_link_text.rectangle.on <- true;
        t.earnings_link_submit.rectangle.on <- false;
      )
  ) else if x_pos >= t.earnings_link_submit.rectangle.x && x_pos <= t.earnings_link_submit.rectangle.x + t.earnings_link_submit.rectangle.width && y_pos >= t.earnings_link_submit.rectangle.y && y_pos <= t.earnings_link_submit.rectangle.y + t.earnings_link_submit.rectangle.height then
    (
      if not (t.earnings_link_submit.rectangle.on) then (
        t.earnings_link_submit.rectangle.on <- true;
        t.earnings_link_text.rectangle.on <- false;
        run_python_script_concurrently t "/home/ubuntu/sentiment-analyzer/main/lib/get_live_data.py" t.earnings_link_text.message
      ) else (
        close_python_script t;
        t.earnings_link_submit.rectangle.on <- false;
      )
  );

  if x_pos >= 482 && x_pos <= 582 && y_pos >= 875 && y_pos <= 900
  then (
    Graphics.set_color (Graphics.rgb 0 0 0);
    Graphics.moveto ((t.graphSentiment.width / 2) + 50) 600;
    Graphics.draw_string "Loading...";
    t.earnings_live_button.rectangle.on <- false;
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
          t.datapts <- datapoints;
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
          t.displayError <- Core.Error.to_string_hum error;
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
else if x_pos >= t.bid_textbox.rectangle.x && x_pos <= t.bid_textbox.rectangle.x + t.bid_textbox.rectangle.width && y_pos >= t.bid_textbox.rectangle.y && y_pos <= t.bid_textbox.rectangle.y + t.bid_textbox.rectangle.height then
  (
    if not (t.bid_textbox.rectangle.on) then (
      t.bid_textbox.rectangle.on <- true;
      t.checker_textbox.rectangle.on <- false;
      t.check_button.rectangle.on <- false;
      t.ask_textbox.rectangle.on <- false;
      t.submit_button.rectangle.on <- false)
  )
else if x_pos >= t.ask_textbox.rectangle.x && x_pos <= t.ask_textbox.rectangle.x + t.ask_textbox.rectangle.width && y_pos >= t.ask_textbox.rectangle.y && y_pos <= t.ask_textbox.rectangle.y + t.ask_textbox.rectangle.height then
  (
    if not (t.ask_textbox.rectangle.on) then (
      t.ask_textbox.rectangle.on <- true;
      t.checker_textbox.rectangle.on <- false;
      t.check_button.rectangle.on <- false;
      t.bid_textbox.rectangle.on <- false;
      t.submit_button.rectangle.on <- false)
  )
else if x_pos >= t.checker_textbox.rectangle.x && x_pos <= t.checker_textbox.rectangle.x + t.checker_textbox.rectangle.width && y_pos >= t.checker_textbox.rectangle.y && y_pos <= t.checker_textbox.rectangle.y + t.checker_textbox.rectangle.height then
  (
    if not (t.checker_textbox.rectangle.on) then (
      t.checker_textbox.rectangle.on <- true;
      t.ask_textbox.rectangle.on <- false;
      t.check_button.rectangle.on <- false;
      t.bid_textbox.rectangle.on <- false;
      t.submit_button.rectangle.on <- false)
  )
else if x_pos >= t.check_button.rectangle.x && x_pos <= t.check_button.rectangle.x + t.check_button.rectangle.width && y_pos >= t.check_button.rectangle.y && y_pos <= t.check_button.rectangle.y + t.check_button.rectangle.height then
  (
    if not (t.check_button.rectangle.on) then (
      t.check_button.rectangle.on <- true;
      t.checker_textbox.rectangle.on <- false;
      t.ask_textbox.rectangle.on <- false;
      t.bid_textbox.rectangle.on <- false;
      t.submit_button.rectangle.on <- false;
      let receiptKey = t.checker_textbox.message in
      let latest = (List.nth_exn (t.datapts.data) ((List.length t.datapts.data)-1)).price in
      let display = handle_receipt t receiptKey latest in
      t.guessText <- (true, display);
      )
  )
else if x_pos >= t.submit_button.rectangle.x && x_pos <= t.submit_button.rectangle.x + t.submit_button.rectangle.width && y_pos >= t.submit_button.rectangle.y && y_pos <= t.submit_button.rectangle.y + t.submit_button.rectangle.height then
  (
    if not (t.submit_button.rectangle.on) then (
      t.submit_button.rectangle.on <- true;
      t.checker_textbox.rectangle.on <- false;
      t.check_button.rectangle.on <- false;
      t.bid_textbox.rectangle.on <- false;
      t.ask_textbox.rectangle.on <- false;
      Core.print_s [%message "Datapts len: " (Int.to_string ((List.length t.datapts.data)-1))];
      let latest = (List.nth_exn (t.datapts.data) ((List.length t.datapts.data)-1)).price in
      let display = handle_submit t latest in
      t.receiptText <- (true, display))
  )
  else (
    (* t.calcBox <- t.calcBox; *)
    t.ticker_textbox.rectangle.on <- false;
    t.time_textbox.rectangle.on <- false;
    t.check_button.rectangle.on <- false;
    t.bid_textbox.rectangle.on <- false;
    t.ask_textbox.rectangle.on <- false;
    t.submit_button.rectangle.on <- false;
    Core.print_s [%message "nothing"])
;;

let check_error t =
  if not (String.equal t.displayError "")
  then Graphics.set_color (Graphics.rgb 220 20 60);
  Graphics.moveto ((t.graphSentiment.width / 2) + 50) 600;
  Graphics.draw_string t.displayError
;;


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
  ; mutable graph : Graph.t
  ; mutable tickerBox : bool
  ; mutable timeBox : bool
  ; mutable calcBox : bool
  ; mutable finViz : Finviz_parser.Finviz_parser.t
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
    ; graph = create_graph data
    ; tickerBox = false
    ; timeBox = false
    ; calcBox = false
    ; finViz =
        { stock_ticker = ""
        ; time_period = 0
        ; link = ""
        ; headlines = [ Finviz_parser.get_date "Jan-01-24", "" ]
        }
    }
  in
  interface
;;

let handle_click t (pos : int * int) =
  let x_pos = fst pos in
  let y_pos = snd pos in
  (* Calculate: 482 575 100 25 *)
  if x_pos >= 482 && x_pos <= 582 && y_pos >= 575 && y_pos <= 600
  then (
    t.calcBox
    <- (let todayDate = Date.today ~zone:Timezone.utc in
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
        true);
    t.tickerBox <- false;
    t.timeBox <- false;
    Core.print_s [%message "calcbox"] (* Ticker: 94 575 100 25 *))
  else if x_pos >= 94 && x_pos <= 194 && y_pos >= 575 && y_pos <= 600
  then (
    t.calcBox <- false;
    t.tickerBox <- true;
    t.timeBox <- false;
    Core.print_s [%message "tickerbox"] (* Timeline: 288 575 100 25 *))
  else if x_pos >= 288 && x_pos <= 384 && y_pos >= 575 && y_pos <= 600
  then (
    t.calcBox <- false;
    t.tickerBox <- false;
    t.timeBox <- true;
    Core.print_s [%message "timebox"])
  else (
    t.calcBox <- t.calcBox;
    t.tickerBox <- false;
    t.timeBox <- false;
    Core.print_s [%message "nothing"])
;;

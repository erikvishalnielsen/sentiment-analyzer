open! Core

module Graph = struct
  type t = {
    height : int;
    width : int;
    data : (int * int) array;
  }
  [@@deriving sexp_of]
end

type t =
  { mutable input_ticker : string;
    mutable input_timeframe : int;
    mutable graph : Graph.t;
    mutable tickerBox : bool;
    mutable timeBox : bool; 
    mutable calcBox : bool;
  }
[@@deriving sexp_of]

let create_graph (data : (int * int) array) : Graph.t = 
  {height = 500; width = 500; data = data}
;;

let create () =
  let data = Array.init 5 ~f:(fun num -> ((num * 100) + 100, ((num + 5) * 25) + 50)) in
  let interface = {input_ticker = "" ; input_timeframe = 0 ; graph = (create_graph data) ; tickerBox = false ;
    timeBox = false ; calcBox = false} in
  interface
;;

let handle_click t (pos : int * int) =
  let x_pos = fst pos in
  let y_pos = snd pos in
  (* Calculate: 482 575 100 25 *)
  if (x_pos >= 482 && x_pos <= 582 && y_pos >= 575 && y_pos <= 600) then
    (t.calcBox <- true;
    t.tickerBox <- false;
    t.timeBox <- false;
    Core.print_s [%message "calcbox"]
    )
  (* Ticker: 94 575 100 25 *)
  else if (x_pos >= 94 && x_pos <= 194 && y_pos >= 575 && y_pos <= 600) then
    (t.calcBox <- false;
    t.tickerBox <- true;
    t.timeBox <- false;
    Core.print_s [%message "tickerbox"]
    )
  (* Timeline: 288 575 100 25 *)
  else if (x_pos >= 288 && x_pos <= 384 && y_pos >= 575 && y_pos <= 600) then
    (t.calcBox <- false;
    t.tickerBox <- false;
    t.timeBox <- true;
    Core.print_s [%message "timebox"]
    )
  else
    (t.calcBox <- t.calcBox;
    t.tickerBox <- false;
    t.timeBox <- false;
    Core.print_s [%message "nothing"]
    )
  
;;
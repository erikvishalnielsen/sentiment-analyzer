open! Core

module Graph : sig
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
    mutable calcBox : bool
  }
[@@deriving sexp_of]

val create : unit -> t

val create_graph : (int * int) array -> Graph.t

val handle_click : t -> (int * int) -> unit

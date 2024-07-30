open! Core

module Graph : sig
  type t =
    { height : int
    ; width : int
    ; data : (int * int) array
    }
  [@@deriving sexp_of]
end

type t [@@deriving sexp_of]

val create : unit -> t
val create_graph : (int * int) array -> Graph.t
val handle_click : t -> int * int -> unit
val check_error : t -> unit

val input_ticker : t -> string
val input_timeframe : t -> int
val graphFinance : t -> Graph.t
val graphHiLo : t -> (float * float)
val graphSentiment : t -> Graph.t
val tickerBox : t -> bool
val timeBox : t -> bool
val calcBox : t -> bool
val displayError : t -> string
  (* ; mutable finViz : Finviz_parser.Finviz_parser.t *)
val correlations : t -> float list
val regressionEqtn : t -> Regression.t option

val set_input_timeframe : t -> int -> unit

val set_input_ticker : t -> string -> unit
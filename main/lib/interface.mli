open! Core

module Graph : sig
  type t =
    { height : int
    ; width : int
    ; data : (int * int) array
    }
  [@@deriving sexp_of]
end

module Rectangle : sig
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

  val draw_rectangle : t -> unit
end

module Button : sig
  type t =
    { rectangle : Rectangle.t
    ; button_text : string
    }

  val draw_button : t -> unit
end

module Textbox : sig
  type t =
    { rectangle : Rectangle.t
    ; textbox_text : string
    ; mutable message : string
    }

  val draw_textbox : t -> unit
end

type t [@@deriving sexp_of]

val create : unit -> t
val create_graph : (int * int) array -> Graph.t
val handle_click : t -> int * int -> unit
val check_error : t -> unit
val graphFinance : t -> Graph.t
val graphHiLo : t -> float * float
val graphSentiment : t -> Graph.t
val graphVolume : t -> Graph.t
val ticker_textbox : t -> Textbox.t
val time_textbox : t -> Textbox.t
val bid_textbox : t -> Textbox.t
val ask_textbox : t -> Textbox.t
val calc_button : t -> Button.t
val price_button : t -> Button.t
val sentiment_button : t -> Button.t
val submit_button : t -> Button.t
val volume_button : t -> Button.t
val displayError : t -> string
val graphInfo : t -> string list
val recieptText : t -> (bool * string)
(* ; mutable finViz : Finviz_parser.Finviz_parser.t *)

val correlations : t -> float list
val regressionEqtn : t -> Regression.t option

open! Core

module Datapoint = struct
  type t =
    { date : Finviz_parser.Stock_date.t
    ; price : float
    ; sentiment : float
    }
  [@@deriving compare, equal, sexp_of, jsonaf]
end

type t =
  { mutable data : Datapoint.t list
  ; mutable sentiment_high : float
  ; mutable sentiment_low : float
  ; mutable price_high : float
  ; mutable price_low : float
  }
[@@deriving sexp_of]

let json_to_datapoints (interface : Interface.t) (json : string) = 
  parse json 
open! Core

module Datapoint : sig
  type t =
    { date : Stock_day.Stock_date.t
    ; price : float
    ; sentiment : float
    ; delta_volume : float
    }
  [@@deriving compare, equal, sexp_of]
end

type t =
  { mutable data : Datapoint.t list
  ; mutable price_high : float
  ; mutable price_low : float
  ; mutable deltavol_high : float
  ; mutable deltavol_low : float
  ; gemini_ans : string list
  }
[@@deriving sexp_of]

val json_to_datapoints : string -> int -> t Or_error.t

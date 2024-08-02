open! Core

type t =
  { a : float
  ; b : float
  ; days : int
  ; delta_price : float
  ; one_day_prediction : float
  ; today_price : float
  }
[@@deriving sexp_of]

val eqtnToString : t -> string * string
val predictionToString : t -> string * string
val regressionCorrelation : Datapoints.t -> float list * t option

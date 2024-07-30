open! Core

type t =
    { a : float;
      b : float;
      days : int
    } [@@deriving sexp_of]

val eqtnToString : t option -> string

val regressionCorrelation : Datapoints.t -> (float list * t option)
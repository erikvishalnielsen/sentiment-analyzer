open! Core

type t =
    { a : float;
      b : float;
    } [@@deriving sexp_of]

val regressionCorrelation : Datapoints.t -> (float list * t option)
open! Core

type t =
  { input_ticker : string;
    input_timeframe : int;
  }
[@@deriving sexp_of]
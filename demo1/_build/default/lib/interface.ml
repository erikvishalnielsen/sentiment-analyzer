open! Core

type t =
  { mutable input_ticker : string;
    mutable input_timeframe : int;
  }
[@@deriving sexp_of]

let create () =
  let interface = {input_ticker = "" ; input_timeframe = 0} in
  interface
;;
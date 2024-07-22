open! Core

type t =
  { mutable input_ticker : string;
    mutable input_timeframe : int;
  }
[@@deriving sexp_of]

val create : unit -> t
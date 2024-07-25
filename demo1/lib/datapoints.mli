open! Core

module Datapoint : sig
  type t =
  { date : Finviz_parser.Stock_date.t
  ; price : float
  ; sentiment : float
  }
[@@deriving compare, equal, sexp_of]
end

type t =
    { mutable data : Datapoint.t list
    } [@@deriving sexp_of]
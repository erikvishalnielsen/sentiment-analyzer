open! Core

module Stock_date : sig
  type t =
    { date : Date.t
    ; days_from_beginning : int
    }
  [@@deriving compare, sexp_of, equal]
end

val getStockDate : Stock_date.t -> string

module Finviz_parser : sig
  type t =
    { stock_ticker : string
    ; time_period : int
    ; link : string
    ; headlines : (string * Stock_date.t) list
    }
end

module Total_book : sig
  type t =
    { bookName : string
    ; stockTable : (string, Finviz_parser.t) Hashtbl.t
    }
end

module Datapoint : sig
  type t =
  { date : Stock_date.t
  ; price : float
  ; sentiment : float
  }
[@@deriving compare, equal, sexp_of]
end

module Datapoints : sig
  type t = 
    { mutable data : Datapoint.t list;
      mutable price_high : float;
      mutable price_low : float
    } [@@deriving sexp_of]
end

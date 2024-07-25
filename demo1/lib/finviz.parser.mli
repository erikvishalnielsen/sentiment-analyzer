open! Core

module Stock_date : sig
  type t =
    { date : Date.t
    ; days_from_beginning : int
    }
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

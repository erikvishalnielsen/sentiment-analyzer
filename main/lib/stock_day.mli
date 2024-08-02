open! Core

module Stock_date : sig
  type t =
    { date : Date.t
    ; days_from_beginning : int
    }
  [@@deriving compare, sexp_of, equal]
end

val getStockDate : Stock_date.t -> string

type t =
  { stock_ticker : string
  ; time_period : int
  ; link : string
  }

val get_date_from_json : string -> Stock_date.t

val createFindlJson
  :  string
  -> startDate:string
  -> endDate:string
  -> max_search:string
  -> total_days:string
  -> unit

val convert_date_tostring : Date.t -> string

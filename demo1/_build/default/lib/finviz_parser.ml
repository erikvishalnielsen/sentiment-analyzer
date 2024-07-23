open! Core

module Stock_date = struct
  type t = {
    date : Date.t;
    days_from_beginning : int
  }
end

module Finviz_parser = struct
  type t = {
    stock_ticker : string;
    time_period : int;
    link : string; 
    headlines : (string * Stock_date.t) list
  }
end

module Total_book = struct
  type t = {
    bookName : string;
    stockTable : (string, Finviz_parser.t) Hashtbl.t 
  }
end

let get_finviz_link ticker = 
  "https://finviz.com/quote.ashx?t=" ^ (String.uppercase ticker) ^ "&p=d";
;;

let create_finviz_parser ticker time = 
  let newlink = get_finviz_link ticker in
  let newParser = {Finviz_parser.stock_ticker = ticker ; time_period = time ; link = newlink ; headlines = []} in
  newParser
;;

let getMonth (month : string) : Month.t = 
  match month with 
  | "Jan" -> Jan
  | "Feb" -> Feb
  | "Mar" -> Mar
  | "Apr" -> Apr
  | "May" -> May
  | "Jun" -> Jun
  | "Jul" -> Jul
  | "Aug" -> Aug
  | "Sep" -> Sep
  | "Oct" -> Oct
  | "Nov" -> Nov
  | "Dec" -> Dec
  | _ -> failwith "Not a valid month!"
;;

let get_date (date : string) : Stock_date.t = 
  let dateList = String.split ~on:'-' date in
  (*let tz : Core_private.Time_zone.t = Est;
  let currDate = Date.today ~zone:*)
  let date : Date.t = Date.create_exn ~y:(Int.of_string (List.nth_exn dateList 2)) ~m:(getMonth (List.nth_exn dateList 1)) ~d:(Int.of_string (List.nth_exn dateList 0)) in
  {date = date ; days_from_beginning = 0}
;;

(* let get_relevant_info (url : string) =

  let headlines = [] in
  let unsorted_data = Lambda_soup.get_list_items (Lambda_soup.Curl.get_exn url) in
  
;; *)
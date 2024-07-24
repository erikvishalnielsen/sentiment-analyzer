open! Core

module Stock_date = struct
  type t = {
    date : Date.t;
    days_from_beginning : int
  }
  [@@deriving sexp_of]
end

let getStockDate (t : Stock_date.t) : string = 
  Date.to_string t.date;
;;

module Finviz_parser = struct
  type t = {
    stock_ticker : string;
    time_period : int;
    link : string; 
    headlines : (Stock_date.t * string) list
  }
  [@@deriving sexp_of]
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
  let currDate = Date.today ~zone:(Timezone.utc) in
  let date : Date.t = Date.create_exn ~y:(Int.of_string ("20" ^ (List.nth_exn dateList 2))) ~m:(getMonth (List.nth_exn dateList 0)) ~d:(Int.of_string (List.nth_exn dateList 1)) in
  {date = date ; days_from_beginning = Date.diff currDate date}
;;

let convert_date_tostring (date : Date.t) : string = 
  let str = Date.to_string date in
  str
;;  

let%expect_test "convert_date" =
  print_s
    [%sexp
      (convert_date_tostring (Date.today ~zone:(Timezone.utc)) : string)];
  [%expect {|true|}] 
;;

let get_relevant_info (url : string) : (Stock_date.t * string) list =
  Core.print_s [%message "Link: " url];
  let data_with_times = Lambda_soup.get_list_items (Lambda_soup.Curl.get_exn url) in
  let date = 
  ref (if ((String.length (fst (List.hd_exn data_with_times))) > 8) then {Stock_date.date = (Date.today ~zone:(Timezone.utc)) ; days_from_beginning = 0} else get_date (String.slice (fst (List.hd_exn data_with_times)) 0 9)) in
  List.map data_with_times ~f:(fun a -> if (String.length (fst a) > 8) && not (String.equal (fst a) (fst (List.hd_exn data_with_times))) then date := get_date (String.slice (fst a) 0 9); (!date, snd a))
;;

(*let%expect_test "web scraper - relevant info test" =
  print_s
    [%sexp
      (get_relevant_info "https://finviz.com/quote.ashx?t=GOOGL&p=d" : (Stock_date.t * string) list)];
  [%expect {|true|}] 
;;*)

let create_finviz_parser ticker time = 
  let newlink = get_finviz_link ticker in
  let newParser = {Finviz_parser.stock_ticker = ticker ; time_period = time ; link = newlink ; headlines = (get_relevant_info newlink) } in
  newParser
;;

(* let createFindlJson ticker ~(startDate : string) ~(endDate : string) : unit = 
  let open Core_unix in 
  let executeCmd (command : string) = (
    let ic, oc, ec = Core_unix.open_process_full command ~env:(Core_unix.environment ()) in
    let output = really_input_string ic (in_channel_length ic) in
    let status = Core_unix.close_process_full (ic, oc, ec) in
    output, status)
  in
  let command = "/bin/python3 /home/ubuntu/sentiment-analyzer/demo1/lib/sentiment_ml.py " + ticker + " " + startDate + " " + endDate in  (* Example command: list files in long format *)
  let output, status = executeCmd command in
  Printf.printf "Command status: %s\n" (Core_unix.Exit.to_string status)
;; *)
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

let get_relevant_info (url : string) : (Stock_date.t * string) list =
  Core.print_s [%message "Link: " url];
  let data_with_times = Lambda_soup.get_list_items (Lambda_soup.Curl.get_exn url) in
  let date = 
  ref (if ((String.length (fst (List.hd_exn data_with_times))) > 8) then {Stock_date.date = (Date.today ~zone:(Timezone.utc)) ; days_from_beginning = 0} else get_date (String.slice (fst (List.hd_exn data_with_times)) 0 9)) in
  List.map data_with_times ~f:(fun a -> if (String.length (fst a) > 8) && not (String.equal (fst a) (fst (List.hd_exn data_with_times))) then date := get_date (String.slice (fst a) 0 9); (!date, snd a))
;;

let%expect_test "web scraper - relevant info test" =
  print_s
    [%sexp
      (get_relevant_info "https://finviz.com/quote.ashx?t=GOOGL&p=d" : (Stock_date.t * string) list)];
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Not_found_s "List.find_exn: not found")
  Raised at Base__List.find_exn.find_exn in file "src/list.ml", line 323, characters 12-27
  Called from Interface_lib__Lambda_soup.get_list_items in file "lib/lambda_soup.ml", line 40, characters 19-131
  Called from Interface_lib__Finviz_parser.get_relevant_info in file "lib/finviz_parser.ml", line 62, characters 24-81
  Called from Interface_lib__Finviz_parser.(fun) in file "lib/finviz_parser.ml", line 71, characters 7-68
  Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 234, characters 12-19

  Trailing output
  ---------------
  ("Link: " https://finviz.com/quote.ashx?t=GOOGL&p=d)
  ("Contents: "
   "This IP address has performed an unusual high number of requests and has been temporarily rate limited. If you believe this to be in error, please contact us at support@finviz.com") |}] 
;;

let create_finviz_parser ticker time = 
  let newlink = get_finviz_link ticker in
  let newParser = {Finviz_parser.stock_ticker = ticker ; time_period = time ; link = newlink ; headlines = (get_relevant_info newlink) } in
  newParser
;;
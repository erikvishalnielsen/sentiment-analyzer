open! Core

module Stock_date = struct
  type t =
    { date : Date.t
    ; days_from_beginning : int
    }
  [@@deriving sexp_of, compare, equal]
end

let getStockDate (t : Stock_date.t) : string = Date.to_string t.date

module Finviz_parser = struct
  type t =
    { stock_ticker : string
    ; time_period : int
    ; link : string
    ; headlines : (Stock_date.t * string) list
    }
  [@@deriving sexp_of]
end

module Total_book = struct
  type t =
    { bookName : string
    ; stockTable : (string, Finviz_parser.t) Hashtbl.t
    }
end

let get_finviz_link ticker =
  "https://finviz.com/quote.ashx?t=" ^ String.uppercase ticker ^ "&p=d"
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
  let currDate = Date.today ~zone:Timezone.utc in
  let date : Date.t =
    Date.create_exn
      ~y:(Int.of_string ("20" ^ List.nth_exn dateList 2))
      ~m:(getMonth (List.nth_exn dateList 0))
      ~d:(Int.of_string (List.nth_exn dateList 1))
  in
  { date; days_from_beginning = Date.diff currDate date }
;;

let convert_date_tostring (date : Date.t) : string =
  let str = Date.to_string date in
  str
;;

let%expect_test "convert_date" =
  print_s
    [%sexp (convert_date_tostring (Date.today ~zone:Timezone.utc) : string)];
  [%expect {|true|}]
;;

let get_relevant_info (url : string) : (Stock_date.t * string) list =
  Core.print_s [%message "Link: " url];
  let data_with_times =
    Lambda_soup.get_list_items (Lambda_soup.Curl.get_exn url)
  in
  let date =
    ref
      (if String.length (fst (List.hd_exn data_with_times)) > 8
       then
         { Stock_date.date = Date.today ~zone:Timezone.utc
         ; days_from_beginning = 0
         }
       else get_date (String.slice (fst (List.hd_exn data_with_times)) 0 9))
  in
  List.map data_with_times ~f:(fun a ->
    if String.length (fst a) > 8
       && not (String.equal (fst a) (fst (List.hd_exn data_with_times)))
    then date := get_date (String.slice (fst a) 0 9);
    !date, snd a)
;;

(*let%expect_test "web scraper - relevant info test" = print_s [%sexp
  (get_relevant_info "https://finviz.com/quote.ashx?t=GOOGL&p=d" :
  (Stock_date.t * string) list)]; [%expect {|true|}] ;;*)

let create_finviz_parser ticker time =
  let newlink = get_finviz_link ticker in
  let newParser =
    { Finviz_parser.stock_ticker = ticker
    ; time_period = time
    ; link = newlink
    ; headlines = get_relevant_info newlink
    }
  in
  newParser
;;

let executeCmd (command : string) = Core_unix.system command

let createFindlJson
  ticker
  ~(startDate : string)
  ~(endDate : string)
  ~(max_search : string)
  : unit
  =
  Core.print_s [%message "Dates: " startDate endDate];
  let command =
    Printf.sprintf
      "/bin/python3 \
       /home/ubuntu/sentiment-analyzer/demo1/lib/sentiment_ml.py %s %s %s \
       %s"
      ticker
      startDate
      endDate
      max_search
  in
  let status = executeCmd command in
  Printf.printf
    "Command status: %s\n"
    (Core_unix.Exit_or_signal.to_string_hum status)
;;

(* let create_json (ticker : string) days = let api_token =
   "66a11a43014e19.64423066" in let today = convert_date_tostring (Date.today
   ~zone:Timezone.utc) in let last_date = convert_date_tostring
   (Date.add_days (Date.today ~zone:Timezone.utc) days) in let url =
   String.concat [ "https://eodhd.com/api/news?s=" ; ticker ;
   ".US&offset=0&limit=1000&api_token=" ; api_token ; "&from=" ; today ;
   "&to=" ; last_date ; "&fmt=json" ] in url ;; *)

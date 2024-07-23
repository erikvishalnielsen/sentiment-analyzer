open! Core

module Curl = struct
  let writer accum data =
    Buffer.add_string accum data;
    String.length data
  ;;

  let get_exn url =
    let error_buffer = ref "" in
    let result = Buffer.create 16384 in
    let fail error = failwithf "Curl failed on %s: %s" url error () in
    try
      let connection = Curl.init () in
      Curl.set_errorbuffer connection error_buffer;
      Curl.set_writefunction connection (writer result);
      Curl.set_followlocation connection true;
      Curl.set_url connection url;
      Curl.perform connection;
      let result = Buffer.contents result in
      Curl.cleanup connection;
      result
    with
    | Curl.CurlException (_reason, _code, _str) -> fail !error_buffer
    | Failure s -> fail s
  ;;
end

(* Gets the "title" node of an HTML page. *)
let get_title contents : string =
  let open Soup in
  parse contents $ "title" |> R.leaf_text
;;

(* Gets all of the list items contained in an HTML page. *)
let get_list_items contents : (string * string) list =
  let open Soup in
  parse contents
  $$ "table" |> to_list |> List.filter ~f:(fun table -> List.exists (classes table) ~f:(fun a -> String.equal a "news-table")) |> List.hd_exn
  $$ "tr"
  |> to_list
  |> List.filter ~f:(fun tr -> List.exists (classes tr) ~f:(fun a -> String.equal a "has-label"))
  |> List.map ~f:(fun li -> let tds = li $$ "td" |> to_list in 
    match tds with 
    | first::second::[] -> (String.strip (String.concat (texts first)), String.strip(String.concat (texts second)))
    | thing -> print_s [%message (String.concat (List.map thing ~f:(fun a -> String.concat (texts a))) : string)]; failwith "Something's wrong"
  )


    (* texts li |> String.concat ~sep:"" |> String.strip) *)
;;

(* Gets the first item of all unordered lists contained in an HTML page. *)
let get_first_item_of_all_unordered_lists contents : string list =
  let open Soup in
  parse contents
  $$ "ul"
  |> to_list
  |> List.map ~f:(fun ul -> (ul $$ "li") |> to_list |> List.hd_exn |> texts |> String.concat ~sep:"" |> String.strip)
;;

(* Gets the first item of the second unordered list in an HTML page. *)
let get_first_item_of_second_unordered_list contents : string =
  let list = get_first_item_of_all_unordered_lists contents in
  List.nth_exn list 1
;; 

(* Gets all bolded text from an HTML page. *)
let get_bolded_text contents : string list =
  let open Soup in
  parse contents
  $$ "b"
  |> to_list
  |> List.map ~f:(fun b -> texts b |> String.concat ~sep:"" |> String.strip)
;;

let%expect_test "web scraper" =
  
  print_s
    [%sexp
      (get_list_items (Curl.get_exn "https://finviz.com/quote.ashx?t=AMZN&p=d") : (string * string) list)];
  [%expect {|true|}] 
;;
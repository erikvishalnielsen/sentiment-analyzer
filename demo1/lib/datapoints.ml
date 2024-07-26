open! Core

module Datapoint = struct
  type t =
  { date : Finviz_parser.Stock_date.t
  ; price : float
  ; sentiment : float
  }
[@@deriving compare, equal, sexp_of]
end

type t =
    { mutable data : Datapoint.t list;
      mutable price_high : float;
      mutable price_low : float
    } [@@deriving sexp_of]

let json_to_datapoints (interface : Interface.t) =
  let open Jsonaf in
  let filename = "date/" ^ interface.input_ticker ^ "_sentiment_price.json" in 
  let in_channel = Core.In_channel.create filename in
  let lines = In_channel.input_all in_channel in 
  In_channel.close in_channel;
  print_s [%message lines];
  let json = match Jsonaf.parse lines with
  | Ok json -> json
  | Error _ -> failwith "something wrong" in

  match json with 
  | `Array[sentiment_dict; price_dict] ->  
    (let hash = match sentiment_dict with 
    | `Object sentiments -> match (snd sentiments) with 
      | `Array sentiment_list ->  match 
      | _ -> failwith "something's wrong"
    | _ -> failwith "something's wrong")
  | _ -> failwith "something's wrong"
;;
(* 

(* Open the JSON file and read its contents *)
let read_json_file filename =
  let ic = open_in filename in
  let json_content = really_input_string ic (in_channel_length ic) in
  close_in ic;
  json_content

(* Parse JSON content *)
let parse_json json_string =
  let json = Jsonaf.of_string json_string in
  json

(* Example usage *)
let () =
  (* Replace "data.json" with the path to your JSON file *)
  let json_string = read_json_file "data.json" in
  let json = parse_json json_string in
  (* Do something with the parsed JSON *)
  (* For example, printing the JSON *)
  Printf.printf "Parsed JSON: %s\n" (Jsonaf.to_string json)
*)
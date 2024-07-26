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

let json_to_tuple (interface : Interface.t) =
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
  | `Array [sentiment_dict; price_dict] -> (
    let sentiments =
      match sentiment_dict with
      | `Object sentiments_list -> 
        List.map sentiments_list ~f:(fun (date, sentiments_array) ->
          match sentiments_array with
          | `Array sentiments_list ->
            (date, List.map sentiments_list ~f:(function
              | `Number x -> Float.of_string x
              | _ -> failwith "Expected a float in sentiments_list"))
          | _ -> failwith "Expected an array for date in sentiment_dict")
      | _ -> failwith "Expected an object for sentiment_dict"
    in
    let prices = 
      match price_dict with
      | `Object price_list -> 
        List.map price_list ~f:(fun (date, price_array) ->
          match price_array with
          | `Array price_list ->
            (date, List.map price_list ~f:(function
              | `Number x -> Float.of_string x
              | _ -> failwith "Expected a float in price_list"))
          | _ -> failwith "Expected an array for date in price_dict")
      | _ -> failwith "Expected an object for price_dict"
          in
    (* Process price_dict if needed *)
    (sentiments, prices)
  )
  | _ -> failwith "Expected an array with two elements"
;;

let json_to_datapoints (interface : Interface.t) =
  let sentiments, price = json_to_tuple interface in
  
  let lastPrice = ref (List.nth_exn (snd (List.nth_exn price 0)) 1) in 

  let emptyList : Datapoint.t list = [] in
  let dataList : Datapoint.t list = List.foldi sentiments ~init:(emptyList) ~f:(fun ind currData currSentiment -> 
    let date = fst currSentiment in
    let found = List.find price ~f:(fun item -> String.equal (fst item) date) in
    match found with
    | Some item -> (
      let newData : Datapoint.t = {date = Finviz_parser.get_date_from_json date ; price = List.nth_exn (snd item) 1 ; 
      sentiment = (List.fold (snd currSentiment) ~init:(0.0) ~f:(fun sum currItem -> sum +. currItem)) /. (Int.to_float (List.length (snd currSentiment))) } in
      lastPrice := (List.nth_exn (snd item) 1);
      (currData @ [newData]);
    )
    | None -> (
      let newData : Datapoint.t = {date = Finviz_parser.get_date_from_json date ; price = !lastPrice ; 
      sentiment = (List.fold (snd currSentiment) ~init:(0.0) ~f:(fun sum currItem -> sum +. currItem)) /. (Int.to_float (List.length (snd currSentiment))) } in
      (currData @ [newData]);
    )
  ) in
  let dataptList : Datapoints.t = {data = dataList ; price_high = (match (List.max_elt dataList ~compare:(fun item1 item2 -> if (Float.(>.) (item1.price) (item2.price)) then 1 else -1)) with | Some item -> item.price | None -> failwith "error")
  ; price_low = match (List.min_elt dataList ~compare:(fun item1 item2 -> if (Float.(<.) (item1.price) (item2.price)) then 1 else -1)) with | Some item -> item.price | None -> failwith "error"} in
  dataptList
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
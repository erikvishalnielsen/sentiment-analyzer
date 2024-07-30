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

let json_to_tuple ticker =
  let filename = "data/" ^ ticker ^ "_" ^ (Date.to_string (Date.add_days (Date.today ~zone:Timezone.utc) (-1))) ^ "_sentiment_price.json" in 
  Core.print_s [%message "filename: " filename];
  try
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
      Ok (sentiments, prices)
    )
    | _ -> failwith "Expected an array with two elements"
with Sys_error error_type -> Error (Error.of_string error_type)
;;

let json_to_datapoints ticker days =

  match (json_to_tuple ticker) with 
  | Ok (sentimentsinit, priceinit) ->
    if days < 5 || days > (List.length sentimentsinit) - 1 || days > (List.length priceinit) - 1 then
      Error (Error.of_string "invalid number of days")
    else ( 
    let sentiments = List.rev (List.slice sentimentsinit 0 days) in
    let price = List.rev (List.slice priceinit 0 days) in
    
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
    let dataptList = {data = dataList ; price_high = (match (List.max_elt dataList ~compare:(fun item1 item2 -> if (Float.(>.) (item1.price) (item2.price)) then 1 else -1)) with | Some item -> item.price | None -> failwith "error")
    ; price_low = match (List.min_elt dataList ~compare:(fun item1 item2 -> if (Float.(>.) (item1.price) (item2.price)) then 1 else -1)) with | Some item -> item.price | None -> failwith "error"} in
    Ok dataptList)
  | Error error_type -> Error error_type
  
;;
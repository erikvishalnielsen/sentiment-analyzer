open! Core

module Datapoint = struct
  type t =
    { date : Stock_day.Stock_date.t
    ; price : float
    ; sentiment : float
    ; delta_volume : float
    }
  [@@deriving compare, equal, sexp_of]
end

type t =
  { mutable data : Datapoint.t list
  ; mutable price_high : float
  ; mutable price_low : float
  ; mutable deltavol_high : float
  ; mutable deltavol_low : float
  ; gemini_ans : string list
  }
[@@deriving sexp_of]

let json_to_tuple ticker =
  let filename =
    "data/"
    ^ ticker
    ^ "_"
    ^ Date.to_string (Date.add_days (Date.today ~zone:Timezone.utc) (-1))
    ^ "_sentiment_price.json"
  in
  Core.print_s [%message "filename: " filename];
  try
    let in_channel = Core.In_channel.create filename in
    let lines = In_channel.input_all in_channel in
    In_channel.close in_channel;
    let json =
      match Jsonaf.parse lines with
      | Ok json -> json
      | Error _ -> failwith "something wrong"
    in
    match json with
    | `Array [ sentiment_dict; price_dict; gemini_ans ] ->
      let sentiments =
        match sentiment_dict with
        | `Object sentiments_list ->
          List.map sentiments_list ~f:(fun (date, sentiments_array) ->
            match sentiments_array with
            | `Array sentiments_list ->
              ( date
              , List.map sentiments_list ~f:(function
                  | `Number x -> Float.of_string x
                  | _ -> failwith "Expected a float in sentiments_list") )
            | _ -> failwith "Expected an array for date in sentiment_dict")
        | _ -> failwith "Expected an object for sentiment_dict"
      in
      let prices =
        match price_dict with
        | `Object price_list ->
          List.map price_list ~f:(fun (date, price_array) ->
            match price_array with
            | `Array price_list ->
              ( date
              , List.map price_list ~f:(function
                  | `Number x -> Float.of_string x
                  | _ -> failwith "Expected a float in price_list") )
            | _ -> failwith "Expected an array for date in price_dict")
        | _ -> failwith "Expected an object for price_dict"
      in
      let gemini =
        match gemini_ans with
        | `Array gem_list ->
          List.map gem_list ~f:(function
            | `String x -> x
            | _ -> failwith "Expected a string in gemini_list")
        | _ -> failwith "Expected an array of answers in gemini_dict"
      in
      (* Process price_dict if needed *)
      Ok (sentiments, prices, gemini)
    | _ -> failwith "Expected an array with three elements"
  with
  | Sys_error error_type -> Error (Error.of_string "Invalid ticker")
;;

let json_to_datapoints ticker days =
  match json_to_tuple ticker with
  | Ok (sentimentsinit, priceinit, gemini) ->
    if days < 5
       || days > List.length sentimentsinit - 1
       || days > List.length priceinit - 1
    then Error (Error.of_string "Invalid number of days")
    else (
      let sentiments = List.rev (List.slice sentimentsinit 0 days) in
      let price = List.rev (List.slice priceinit 0 days) in
      let lastPrice = ref (List.nth_exn (snd (List.nth_exn price 0)) 1) in
      let lastVol = ref (List.nth_exn (snd (List.nth_exn price 0)) 2) in
      (* STARTING VOLUME VALUE *)
      let emptyList : Datapoint.t list = [] in
      let dataList : Datapoint.t list =
        List.foldi
          sentiments
          ~init:emptyList
          ~f:(fun ind currData currSentiment ->
            let date = fst currSentiment in
            let found =
              List.find price ~f:(fun item -> String.equal (fst item) date)
            in
            match found with
            | Some item ->
              let newData : Datapoint.t =
                { date = Stock_day.get_date_from_json date
                ; price = List.nth_exn (snd item) 1
                ; sentiment =
                    List.fold
                      (snd currSentiment)
                      ~init:0.0
                      ~f:(fun sum currItem -> sum +. currItem)
                    /. Int.to_float (List.length (snd currSentiment))
                ; delta_volume =
                    (if Float.( = ) !lastVol 0.0
                     then 0.0
                     else (List.nth_exn (snd item) 2 -. !lastVol) /. !lastVol)
                }
              in
              lastPrice := List.nth_exn (snd item) 1;
              lastVol := List.nth_exn (snd item) 2;
              currData @ [ newData ]
            | None ->
              let newData : Datapoint.t =
                { date = Stock_day.get_date_from_json date
                ; price = !lastPrice
                ; sentiment =
                    List.fold
                      (snd currSentiment)
                      ~init:0.0
                      ~f:(fun sum currItem -> sum +. currItem)
                    /. Int.to_float (List.length (snd currSentiment))
                ; delta_volume =
                    (if Float.( = ) !lastVol 0.0
                     then 0.0
                     else (0.0 -. !lastVol) /. !lastVol)
                }
              in
              lastVol := 0.0;
              currData @ [ newData ])
      in
      let dataptList =
        { data = dataList
        ; price_high =
            (match
               List.max_elt dataList ~compare:(fun item1 item2 ->
                 if Float.( >. ) item1.price item2.price then 1 else -1)
             with
             | Some item -> item.price
             | None -> failwith "error")
        ; price_low =
            (match
               List.min_elt dataList ~compare:(fun item1 item2 ->
                 if Float.( >. ) item1.price item2.price then 1 else -1)
             with
             | Some item -> item.price
             | None -> failwith "error")
        ; deltavol_low =
            (match
               List.min_elt dataList ~compare:(fun item1 item2 ->
                 if Float.( >. ) item1.delta_volume item2.delta_volume
                 then 1
                 else -1)
             with
             | Some item -> item.delta_volume
             | None -> failwith "error")
        ; deltavol_high =
            (match
               List.max_elt dataList ~compare:(fun item1 item2 ->
                 if Float.( >. ) item1.delta_volume item2.delta_volume
                 then 1
                 else -1)
             with
             | Some item -> item.delta_volume
             | None -> failwith "error")
        ; gemini_ans = gemini
        }
      in
      Ok dataptList)
  | Error error_type -> Error error_type
;;

open! Core

type t =
  { a : float
  ; b : float
  ; days : int
  ; delta_price : float
  ; one_day_prediction : float
  ; today_price : float
  }
[@@deriving sexp_of]

let getSlopePrice
  (datapt1 : Datapoints.Datapoint.t)
  (datapt2 : Datapoints.Datapoint.t)
  =
  datapt2.price -. datapt1.price
;;

let getSlopeSentiment
  (datapt1 : Datapoints.Datapoint.t)
  (datapt2 : Datapoints.Datapoint.t)
  =
  datapt2.sentiment -. datapt1.sentiment
;;

let eqtnToString (t : t) : string * string =
  let str =
    ( "+" ^ Int.to_string t.days ^ " Days:"
    , "Y = "
      ^ Float.to_string (Float.round_significant ~significant_digits:3 t.a)
      ^ " + "
      ^ Float.to_string (Float.round_significant ~significant_digits:3 t.b)
      ^ "(X)" )
  in
  str
;;

let predictionToString (t : t) =
  let str1 =
    "Next closing price: "
    ^ Float.to_string
        (Float.round_decimal t.one_day_prediction ~decimal_digits:2)
  in
  let str2 =
    "Price Delta of: "
    ^ Float.to_string (Float.round_decimal t.delta_price ~decimal_digits:2)
  in
  str1, str2
;;

let coefficient (ptList : (float * float) list) : float =
  let n = Int.to_float (List.length ptList) in
  let sumX = List.fold ptList ~init:0.0 ~f:(fun sum pt -> sum +. fst pt) in
  let sumXSquare =
    List.fold ptList ~init:0.0 ~f:(fun sum pt -> sum +. (fst pt *. fst pt))
  in
  let sumY = List.fold ptList ~init:0.0 ~f:(fun sum pt -> sum +. snd pt) in
  let sumYSquare =
    List.fold ptList ~init:0.0 ~f:(fun sum pt -> sum +. (snd pt *. snd pt))
  in
  let sumXY =
    List.fold ptList ~init:0.0 ~f:(fun sum pt -> sum +. (fst pt *. snd pt))
  in
  let r =
    ((n *. sumXY) -. (sumX *. sumY))
    /. sqrt
         (((n *. sumXSquare) -. (sumX *. sumX))
          *. ((n *. sumYSquare) -. (sumY *. sumY)))
  in
  r
;;

let regressionEqtn
  (ptList : (float * float) list list)
  (corrs : float list)
  latestPrice
  (data : Datapoints.t)
  : t option
  =
  let maxCorr = ref (-2.0) in
  let maxInd =
    List.foldi
      corrs
      ~f:(fun index maxInd cor ->
        if Float.( >. ) cor !maxCorr
        then (
          maxCorr := cor;
          index)
        else maxInd)
      ~init:(-1)
  in
  match maxInd with
  | 0 | 1 -> None
  | _ ->
    (* GET REGRESSION EQUATION HERE *)
    let dayList = [ -1; 0; 1; 2 ] in
    let bestList = List.nth_exn ptList maxInd in
    let meanX =
      List.fold bestList ~init:0.0 ~f:(fun sum pt -> sum +. fst pt)
      /. Float.of_int (List.length bestList)
    in
    let meanY =
      List.fold bestList ~init:0.0 ~f:(fun sum pt -> sum +. snd pt)
      /. Float.of_int (List.length bestList)
    in
    let numerator =
      List.fold bestList ~init:0.0 ~f:(fun sum pt ->
        sum +. ((fst pt -. meanX) *. (snd pt -. meanY)))
    in
    let denaminator =
      List.fold bestList ~init:0.0 ~f:(fun sum pt ->
        sum +. ((fst pt -. meanX) *. (fst pt -. meanX)))
    in
    let b_init = numerator /. denaminator in
    let a_init = meanY -. (b_init *. meanX) in
    let days_init = List.nth_exn dayList maxInd in
    let deltaSentiment : float =
      (List.nth_exn data.data (List.length data.data - days_init)).sentiment
      -. (List.nth_exn data.data (List.length data.data - days_init - 1))
           .sentiment
    in
    Core.print_s [%message "Delta Sentiment: " (Float.to_string deltaSentiment)];
    let deltaPrice = a_init +. (b_init *. deltaSentiment) in
    let newPrice = latestPrice +. deltaPrice in
    let regression : t =
      { a = a_init
      ; b = b_init
      ; days = List.nth_exn dayList maxInd
      ; delta_price = deltaPrice
      ; one_day_prediction = newPrice
      ; today_price = latestPrice
      }
    in
    Some regression
;;

let regressionCorrelation (data : Datapoints.t) : float list * t option =
  let modifierList = [ -1; 0; 1; 2 ] in
  (* THE ITEMS IN THE LIST CORRESPOND TO THE DELAYS IN THE MODIFIER LIST! *)
  let dataLen = List.length data.data in
  let deltaData =
    List.foldi
      data.data
      ~f:(fun index deltas datapt ->
        if index > 0 && index < dataLen - 3
        then (
          let sentimentChange =
            getSlopeSentiment datapt (List.nth_exn data.data (index + 1))
          in
          let delts =
            List.map modifierList ~f:(fun modify ->
              let otherData1 = List.nth_exn data.data (index + modify) in
              let otherData2 = List.nth_exn data.data (index + modify + 1) in
              let priceSlope = getSlopePrice otherData1 otherData2 in
              sentimentChange, priceSlope)
          in
          deltas @ [ delts ])
        else deltas)
      ~init:[]
  in
  let correlationList =
    List.init 4 ~f:(fun num ->
      List.map deltaData ~f:(fun delt -> List.nth_exn delt num))
  in
  let correlations =
    List.fold
      correlationList
      ~f:(fun corrs floatList -> corrs @ [ coefficient floatList ])
      ~init:[]
  in
  let latestPrice =
    (List.nth_exn data.data (List.length data.data - 1)).price
  in
  let bestFitEtqn =
    regressionEqtn correlationList correlations latestPrice data
  in
  Core.print_s
    [%message
      "Correlations: " (List.to_string ~f:Float.to_string correlations)];
  correlations, bestFitEtqn
;;

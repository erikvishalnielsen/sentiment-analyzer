open! Core

let getSlopePrice (datapt1 : Datapoints.Datapoint.t) (datapt2 : Datapoints.Datapoint.t) = 
  (datapt2.price -. datapt1.price)
;;

let getSlopeSentiment (datapt1 : Datapoints.Datapoint.t) (datapt2 : Datapoints.Datapoint.t) = 
  (datapt2.sentiment -. datapt1.sentiment)
;;

let coefficient (ptList : (float * float) list) : float =
  let n = Int.to_float (List.length ptList) in
  let sumX = List.fold ptList ~init:(0.0) ~f:(fun sum pt -> (sum +. (fst pt))) in
  let sumXSquare = List.fold ptList ~init:(0.0) ~f:(fun sum pt -> (sum +. ((fst pt) *. (fst pt)))) in
  let sumY = List.fold ptList ~init:(0.0) ~f:(fun sum pt -> (sum +. (snd pt))) in
  let sumYSquare = List.fold ptList ~init:(0.0) ~f:(fun sum pt -> (sum +. ((snd pt) *. (snd pt)))) in
  let sumXY = List.fold ptList ~init:(0.0) ~f:(fun sum pt -> (sum +. ((fst pt) *. (snd pt)))) in


  let r = ((n *. sumXY) -. (sumX *. sumY)) /. (sqrt (((n *. sumXSquare) -. (sumX *. sumX)) *. ((n *. sumYSquare) -. (sumY *. sumY)))) in
  r
;;



let regressionCorrelation (data : Datapoints.t) : float list = 
  let modifierList = [-1;0;1;2] in
  (* THE ITEMS IN THE LIST CORRESPOND TO THE DELAYS IN THE MODIFIER LIST! *)

  let dataLen = List.length data.data in
  let deltaData = List.foldi data.data ~f:(fun index deltas datapt -> 
    if (index > 0 && index < dataLen-3) then (
      let sentimentChange = getSlopeSentiment datapt (List.nth_exn data.data (index+1)) in
      let delts = List.map modifierList ~f:(fun modify ->
        let otherData1 = List.nth_exn data.data (index + modify) in
        let otherData2 = List.nth_exn data.data (index + modify + 1) in
        let priceSlope = getSlopePrice otherData1 otherData2 in
        (sentimentChange, priceSlope)
      ) in
      deltas @ [delts];
    ) else deltas
  ) ~init:([]) in

  let correlationList = List.init 4 ~f:(fun num -> (List.map deltaData ~f:(fun delt -> List.nth_exn delt num))) in
  let correlations = List.fold correlationList ~f:(fun corrs floatList -> corrs @ [coefficient floatList]) ~init:([]) in
  Core.print_s [%message "Correlations: " (List.to_string ~f:(Float.to_string) correlations)];

  correlations
;;
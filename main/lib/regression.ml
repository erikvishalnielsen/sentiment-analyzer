open! Core

let getSlopePrice (datapt1 : Datapoints.Datapoint.t) (datapt2 : Datapoints.Datapoint.t) = 
  (datapt2.price -. datapt1.price)
;;

let getSlopeSentiment (datapt1 : Datapoints.Datapoint.t) (datapt2 : Datapoints.Datapoint.t) = 
  (datapt2.sentiment -. datapt1.sentiment)
;;

let regressionCorrelation (data : Datapoints.t) : float array = 
  let correlations = [|0.0;0.0;0.0;0.0|] in
  let modifierList = [-1;0;1;2] in

  let dataLen = List.length data.data in
  let _deltaData = List.foldi data.data ~f:(fun index deltas datapt -> 
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

  correlations
;;
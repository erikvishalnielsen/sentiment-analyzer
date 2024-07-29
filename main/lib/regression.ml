open! Core

let regressionCorrelation (data : Datapoints.t) : float array = 
  let correlations = [|0.0;0.0;0.0;0.0|] in
  let modifierList = [-1;0;1;2] in

  List.foldi data.data ~f:(fun)

  correlations
;;
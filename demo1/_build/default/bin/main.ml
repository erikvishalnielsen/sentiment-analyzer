open! Core
open! Interface_lib

let () =
  Run.run ();
  Core.never_returns (Async.Scheduler.go ())
;;
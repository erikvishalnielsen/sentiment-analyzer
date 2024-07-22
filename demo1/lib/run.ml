open! Core

(* This is the core logic that actually runs the game. We have implemented
   all of this for you, but feel free to read this file as a reference. *)
let every seconds ~f ~stop =
  let open Async in
  let rec loop () =
    if !stop
    then return ()
    else
      Clock.after (Time_float.Span.of_sec seconds)
      >>= fun () ->
      f ();
      loop ()
  in
  don't_wait_for (loop ())
;;

let handle_keys (_interface : Interface.t) ~game_over =
  every ~stop:(game_over) 0.001 ~f:(fun () ->
    match Interface_graphics.read_key () with
    | None -> ()
    | Some _key -> () 
    )
;;

let run () =
  (* let game = Interface_graphics.init_exn () in *)
  Interface_graphics.render ();
  handle_keys;
;;

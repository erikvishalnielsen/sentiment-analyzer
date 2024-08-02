open! Core

(* This is the core logic that actually runs the game. We have implemented
   all of this for you, but feel free to read this file as a reference. *)
let game_over = ref false

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

let handle_keys (interface : Interface.t) =
  every ~stop:game_over 0.001 ~f:(fun () ->
    match Interface_graphics.read_key () with
    | None -> ()
    | Some key ->
      if (Interface.time_textbox interface).rectangle.on
      then (
        match
          Char.is_digit key && Interface.input_timeframe interface < 100
        with
        | true ->
          Interface.set_input_timeframe
            interface
            (Int.of_string
               (String.concat
                  [ Int.to_string (Interface.input_timeframe interface)
                  ; String.of_char key
                  ]))
        | false ->
          (match Char.to_int key with
           | 8 ->
             let len =
               String.length
                 (Int.to_string (Interface.input_timeframe interface))
             in
             if not (len = 1 || len = 0)
             then
               Interface.set_input_timeframe
                 interface
                 (Int.of_string
                    (String.slice
                       (Int.to_string (Interface.input_timeframe interface))
                       0
                       (len - 1)))
             else Interface.set_input_timeframe interface 0
           | _ -> ()));
      if (Interface.ticker_textbox interface).rectangle.on
      then (
        match
          Char.is_alpha key
          && String.length (Interface.ticker_textbox interface).message < 6
        with
        | true ->
          Interface.set_input_ticker
            interface
            (String.concat
               [ Interface.input_ticker interface; String.of_char key ])
        | false ->
          (match Char.to_int key with
           | 8 ->
             let len = String.length (Interface.input_ticker interface) in
             if not (len = 1 || len = 0)
             then
               Interface.set_input_ticker
                 interface
                 (String.slice
                    (Interface.input_ticker interface)
                    0
                    (len - 1))
             else Interface.set_input_ticker interface ""
           | _ -> ())))
;;

let handle_clicks (interface : Interface.t) =
  every ~stop:game_over 0.001 ~f:(fun () ->
    if Graphics.button_down ()
    then Interface.handle_click interface (Graphics.mouse_pos ()))
;;

let handle_steps (game : Interface.t) =
  every ~stop:game_over 0.1 ~f:(fun () -> Interface_graphics.render game)
;;

let run () =
  let interface = Interface_graphics.init_exn () in
  Interface_graphics.render interface;
  handle_keys interface;
  handle_clicks interface;
  handle_steps interface
;;

(* if (Graphics.button_down ()) then check_stock_box *)

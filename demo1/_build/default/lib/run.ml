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
  every ~stop:(game_over) 0.001 ~f:(fun () ->
    match Interface_graphics.read_key () with
    | None -> ()
    | Some key -> (
      if (interface.timeBox) then (
        match (Char.is_digit key && (interface.input_timeframe) < 100) with 
        | true -> interface.input_timeframe <- Int.of_string (String.concat [Int.to_string (interface.input_timeframe) ; (String.of_char key)]);
        | false -> (
          match (Char.to_int key) with 
          | 8 -> (
            let len = String.length (Int.to_string interface.input_timeframe) in
            if not (len = 1 || len = 0) then (
            interface.input_timeframe <- Int.of_string (String.slice (Int.to_string (interface.input_timeframe)) 0 (len-1))) else (interface.input_timeframe <- 0);
          ) 
          | _ -> ()
        )
      );
      if (interface.tickerBox) then (
        match (Char.is_alpha key && (String.length interface.input_ticker) < 6) with 
        | true -> interface.input_ticker <- String.concat [(interface.input_ticker) ; (String.of_char key)];
        | false -> (
          match (Char.to_int key) with 
          | 8 -> (
            let len = String.length interface.input_ticker in
            if not (len = 1 || len = 0) then (
            interface.input_ticker <- (String.slice (interface.input_ticker) 0 (len-1))) else (interface.input_ticker <- "");
          ) 
          | _ -> ()
        )
      );
    ) 
    )
;;

let handle_clicks (interface : Interface.t) = 
  every ~stop:(game_over) 0.001 ~f:(fun () ->
    if (Graphics.button_down ()) then (Interface.handle_click interface (Graphics.mouse_pos ())))
;;

let handle_steps (game : Interface.t) =
  every ~stop:(game_over) 0.1 ~f:(fun () ->
    Interface_graphics.render game)
;;

let run () =
  let interface = Interface_graphics.init_exn () in
  Interface_graphics.render interface;
  handle_keys interface;
  handle_clicks interface;
  handle_steps interface;
  
  (* if (Graphics.button_down ()) then check_stock_box *)
;;

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
          Char.is_digit key && (Int.of_string (Interface.time_textbox interface).message) < 100
        with
        | true ->
          (Interface.time_textbox interface).message <- if (String.equal (Interface.time_textbox interface).message "0") then String.of_char key else 
               (String.concat
                  [ (Interface.time_textbox interface).message
                  ; String.of_char key
                  ])
        | false ->
          (match Char.to_int key with
           | 8 ->
             let len =
               String.length
              (Interface.time_textbox interface).message
             in
             if not (len = 1 || len = 0)
             then
              (Interface.time_textbox interface).message <-
                 (String.slice
                 (Interface.time_textbox interface).message
                       0
                       (len - 1))
             else (Interface.time_textbox interface).message <- "0"
           | _ -> ()));
      if (Interface.checker_textbox interface).rectangle.on
      then (
        match
          Char.is_digit key && (Int.of_string (Interface.checker_textbox interface).message) < 100000000
        with
        | true ->
          (Interface.checker_textbox interface).message <- if (String.equal (Interface.checker_textbox interface).message "0") then String.of_char key else 
               (String.concat
                  [ (Interface.checker_textbox interface).message
                  ; String.of_char key
                  ])
        | false ->
          (match Char.to_int key with
           | 8 ->
             let len =
               String.length
              (Interface.checker_textbox interface).message
             in
             if not (len = 1 || len = 0)
             then
              (Interface.checker_textbox interface).message <-
                 (String.slice
                 (Interface.checker_textbox interface).message
                       0
                       (len - 1))
             else (Interface.checker_textbox interface).message <- "0"
           | _ -> ()));
      if (Interface.bid_textbox interface).rectangle.on
      then (
        match
          Char.is_digit key
        with
        | true ->
          (Interface.bid_textbox interface).message <- if (String.equal (Interface.bid_textbox interface).message "0") then String.of_char key else 
               (String.concat
                  [ (Interface.bid_textbox interface).message
                  ; String.of_char key
                  ])
        | false ->
          (match Char.to_int key with
           | 8 ->
             let len =
               String.length
              (Interface.bid_textbox interface).message
             in
             if not (len = 1 || len = 0)
             then
              (Interface.bid_textbox interface).message <-
                 (String.slice
                 (Interface.bid_textbox interface).message
                       0
                       (len - 1))
             else (Interface.bid_textbox interface).message <- "0"
           | 46 -> (
            if(not (String.exists ~f:(fun c -> Char.equal c '.') (Interface.bid_textbox interface).message)) then (
              (Interface.bid_textbox interface).message <- (String.concat
              [ (Interface.bid_textbox interface).message
              ; "."
              ])
             ))
           | _ -> ()));
if (Interface.ask_textbox interface).rectangle.on
      then (
        match
          Char.is_digit key
        with
        | true ->
          (Interface.ask_textbox interface).message <- if (String.equal (Interface.ask_textbox interface).message "0") then String.of_char key else 
               (String.concat
                  [ (Interface.ask_textbox interface).message
                  ; String.of_char key
                  ])
        | false ->
          (match Char.to_int key with
           | 8 ->
             let len =
               String.length
              (Interface.ask_textbox interface).message
             in
             if not (len = 1 || len = 0)
             then
              (Interface.ask_textbox interface).message <-
                 (String.slice
                 (Interface.ask_textbox interface).message
                       0
                       (len - 1))
             else (Interface.ask_textbox interface).message <- "0"
           | 46 -> (
            if(not (String.exists ~f:(fun c -> Char.equal c '.') (Interface.ask_textbox interface).message)) then (
            (Interface.ask_textbox interface).message <- (String.concat
            [ (Interface.ask_textbox interface).message
            ; "."
            ])
           ))
           | _ -> ()));
      if (Interface.ticker_textbox interface).rectangle.on
      then (
        match
          Char.is_alpha key
          && String.length (Interface.ticker_textbox interface).message < 6
        with
        | true ->
          (Interface.ticker_textbox 
            interface).message <-
            (String.concat
               [ (Interface.ticker_textbox interface).message; String.uppercase (String.of_char key) ])
        | false ->
          (match Char.to_int key with
           | 8 ->
             let len = String.length (Interface.ticker_textbox interface).message in
             if not (len = 1 || len = 0)
             then
              (Interface.ticker_textbox interface).message <-
                 (String.slice
                 (Interface.ticker_textbox interface).message
                    0
                    (len - 1))
             else (Interface.ticker_textbox interface).message <- ""
           | _ -> ()));
    if (Interface.earnings_link_text interface).rectangle.on
      then (
        match
          not ((Char.to_int key) = 8)
        with
        | true ->
          (Interface.earnings_link_text
            interface).message <-
            (String.concat
               [ (Interface.earnings_link_text interface).message; String.uppercase (String.of_char key) ])
        | false ->
          (match Char.to_int key with
           | 8 ->
             let len = String.length (Interface.earnings_link_text interface).message in
             if not (len = 1 || len = 0)
             then
              (Interface.earnings_link_text interface).message <-
                 (String.slice
                 (Interface.earnings_link_text interface).message
                    0
                    (len - 1))
             else (Interface.earnings_link_text interface).message <- ""
           | _ -> ())));
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

open! Core

(** This module handles the graphics for the game. We have implemented this for
    you so you don't need to change anything here, but feel free to look around
    and once you have the game, feel free to alter this file to make things
    fancier! *)
val init_exn : unit -> unit

(** [read_key] returns a keyboard input, if it's available. *)
val read_key : unit -> char option

val render : unit -> unit

open! Core

module Curl : sig
  val get_exn : string -> string
end

val get_list_items : string -> (string * string) list

(** [Lambda_soup_utilities] exposes some simple commands that use the Lambda Soup library
    (https://github.com/aantron/lambdasoup) to parse HTML. *)

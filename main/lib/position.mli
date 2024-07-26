open! Core

(** A [t] represents a square on the playing area, identified by its row and
    column. *)
type t =
  { col : int
  ; row : int
  }
[@@deriving compare, equal, sexp_of]

(** [to_string] returns "col, row" *)
val to_string : t -> string

(** [list_to_string] returns "[ col1, row1; col2, row2; ... ]" *)
val list_to_string : t list -> string

(** [of_col_major_coord] takes a single (col, row) coordinate and returns a [t] *)
val of_col_major_coord : int * int -> t

(** [of_col_major_coords] takes a list of (col, row) coordinates and returns a list of
    [t]s *)
val of_col_major_coords : (int * int) list -> t list

open! Core

type t =
  { col : int
  ; row : int
  }
[@@deriving compare, equal, sexp_of]

let to_string { col; row } = Core.sprintf "%d, %d" col row

let list_to_string ts =
  List.map ts ~f:to_string
  |> String.concat ~sep:"; "
  |> Core.sprintf "[ %s ]"
;;

let of_col_major_coord (col, row) = { col; row }

let of_col_major_coords coords =
  List.map coords ~f:(fun (col, row) -> { col; row })
;;

open Core
open Core.List.Let_syntax

module Board = struct
  module Index = struct
    type t = (int * int) [@@deriving compare, sexp_of]
  end

  include Comparable.Make_plain(Index)
  type t = (int * int, int, comparator_witness) Core.Map.t

  let of_list (lst : int list list) : t = 
      let ls = List.(
        let open Let_syntax in
        let%map r = range 0 9 and c = range 0 9 in
        let row = nth_exn lst r in
        let v = nth_exn row c in
        (r, c), v
      ) in Map.of_alist_exn ls

  let print (board : t) : unit = for i = 0 to 8 do
      for j = 0 to 8 do
          printf "%i " (Core.Map.find_exn board (i, j));
      done;
      printf "\n";
  done
end

(* let board = [ *)
(*     [0; 0; 0; 0; 0; 0; 0; 0; 0]; *)
(*     [0; 0; 0; 0; 0; 0; 0; 0; 0]; *)
(*     [0; 0; 0; 0; 0; 0; 0; 0; 0]; *)
(*     [0; 0; 0; 0; 0; 0; 0; 0; 0]; *)
(*     [0; 0; 0; 0; 0; 0; 0; 0; 0]; *)
(*     [0; 0; 0; 0; 0; 0; 0; 0; 0]; *)
(*     [0; 0; 0; 0; 0; 0; 0; 0; 0]; *)
(*     [0; 0; 0; 0; 0; 0; 0; 0; 0]; *)
(*     [0; 0; 0; 0; 0; 0; 0; 0; 0] *)
(* ] |> Board.of_list *)

let hardboard = [
    [1; 0; 0; 0; 0; 0; 0; 0; 0];
    [2; 0; 0; 0; 0; 0; 0; 0; 0];
    [0; 0; 0; 8; 7; 0; 0; 0; 0];
    [0; 0; 0; 0; 9; 0; 0; 0; 0];
    [0; 0; 0; 0; 0; 0; 0; 0; 5];
    [0; 0; 0; 7; 0; 0; 0; 0; 6];
    [0; 0; 0; 0; 0; 0; 0; 0; 7];
    [0; 0; 0; 0; 0; 0; 0; 0; 9];
    [0; 0; 0; 0; 0; 0; 0; 0; 8]
] |> Board.of_list

let rows (board : Board.t) : int list list =
    let%map r = List.range 0 9 in
    let%map c = List.range 0 9 in
    Map.find_exn board (r, c)

let cols (board : Board.t) : int list list =
    let%map c = List.range 0 9 in
    let%map r = List.range 0 9 in
    Map.find_exn board (r, c)

let subsquares (board : Board.t) : int list list =
    let%map i = List.range 0 3 and j = List.range 0 3 in
    let%map r = List.range 0 3 and c = List.range 0 3 in 
    Map.find_exn board (i * 3 + r, j * 3 + c)

let nonempty: int list -> int list = List.filter ~f:((<>) 0)

let valid (grp : int list) : bool = List.(
    let nonempty = nonempty grp in
    let dedup = dedup_and_sort ~compare:Int.compare nonempty in
    length dedup = length nonempty
)

let complete (grp : int list) : bool = List.(
    equal Int.equal (dedup_and_sort ~compare:Int.compare grp) (range 1 10)
)

let check_board (pred : int list -> bool) (board : Board.t) : bool = List.(
    for_all (rows board) ~f:pred &&
    for_all (cols board) ~f:pred &&
    for_all (subsquares board) ~f:pred
)

let is_valid: Board.t -> bool = check_board valid
let is_complete: Board.t -> bool = check_board complete

let rec solve (board : Board.t) : Board.t Option.t =
    if not (is_valid board) then None
    else if is_complete board then Some board
    else Sequence.(
        let open Sequence.Let_syntax in
        let spot = hd_exn begin
            let%bind r = range 0 9 and c = range 0 9 in
            if Map.find_exn board (r, c) = 0 then singleton (r, c) else empty
        end in
        let adjacencies = let%map n = range 1 10 in Map.set board ~key:spot ~data:n in
        let results = map adjacencies ~f:solve in
        find results ~f:Option.is_some |> Option.join
    )

let () =
    let solved = solve hardboard in
    match solved with
    | None -> print_endline "No solution found"
    | Some b -> Board.print b

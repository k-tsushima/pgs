open Syntax

let id x = x
let dummy_id x = Dummy

let rec construct_dummy x = match x with
  | Int _ | Dummy -> Dummy
  | Unit -> Unit
  | Con(Int _, b) -> Con(Dummy, construct_dummy b)
  | Con(Dummy, b) -> Con(Dummy, construct_dummy b)
  | Con(Unit, b) -> Con(Unit, construct_dummy b)
  | Con(a, b) -> construct_dummy a

let first x = match x with
    | Con(a, b) -> a
    | _ -> assert false

let second x = match x with
    | Con(a, b) -> b
    | _ -> assert false

(* make_consecutive_list 5 = [5,4,3,2,1] *)
let rec make_consecutive_list n = match n with
	| 0 -> Unit
	| n -> Con(Int n, make_consecutive_list (n - 1))

let rec make_consecutive_list_2 n = match n with
	| 0 -> Unit
	| n -> Con(Int (n + 10000), make_consecutive_list_2 (n - 1))

(* make_smallest_nested_list 3 = [[[1]]] *)
let rec make_smallest_nested_list n = match n with
	| 1 -> Con(Int 1, Unit)
	| n -> Con(make_smallest_nested_list (n - 1), Unit)

(* make_consecutive_nested_list 5 = [[5],[4],[3],[2],[1]] *)
let rec make_consecutive_nested_list n = match n with
    | 0 -> Unit
    | n -> Con(Con(Int n, Unit), make_consecutive_nested_list (n - 1))
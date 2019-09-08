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

(* make_smallest_nested_list 3 = [[[1]]] *)
let rec make_smallest_nested_list n = match n with
	| 1 -> Con(Int 1, Unit)
	| n when n > 0 -> Con(make_smallest_nested_list (n - 1), Unit)
	| _ -> assert false

(* make_same_elements_list 2 3 = [2,2,2] *)
let rec make_same_elements_list x n = match n with
	| 1 -> Con(Int x, Unit)
	| n when n > 0 -> Con(Int x, make_same_elements_list x (n - 1))
	| _ -> assert false

(* make_consecutive_list 5 = [5,4,3,2,1] *)
let rec make_consecutive_list n = match n with
	| 0 -> Unit
	| n -> Con(Int n, make_consecutive_list (n - 1))
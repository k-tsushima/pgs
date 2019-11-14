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

let rec make_list m n = 
  if m = n then Con(Int m, Unit)
  else if m < n then Con(Int m, make_list (m + 1) n)
  else Con(Int m, make_list (m - 1) n)

let rec make_nested_list m k n =
  match m with 
  | 1 -> make_list k n 
  | m -> Con(make_nested_list (m - 1) k n, Unit)

let rec make_list_for_phead m n = 
  match m with
  | 1 -> make_list 1 n
  | m -> Con(make_list_for_phead (m - 1) n, Con(make_nested_list (m - 1) (n * m - n + 1) (n * m), Unit))

let rec make_binary_list n = 
  match n with
  | 1 -> Con(Int 1, Con(Int 1, Unit))
  | n -> let t = make_binary_list (n - 1) in 
    Con(t, Con (t, Unit))

let rec make_list_of_binary_list n = 
  match n with 
  | 0 -> Unit
  | n -> Con(make_binary_list n, make_list_of_binary_list (n - 1))

let rec make_binary_tree n i =
  match n with 
  | 1 -> make_list 1 10
  | n -> let t = make_binary_tree (n - 1) i in 
    Con(t, Con (t, Unit))

let rec make_list_of_binary_tree n = 
  match n with 
  | 0 -> Unit
  | n -> Con(make_binary_tree (n/2 + 10) n, make_list_of_binary_tree (n - 1))

let rec make_list_of_binary_tree_2 n = 
  match n with 
  | 0 -> Unit
  | n -> Con(make_binary_tree n (2 * n), make_list_of_binary_tree (n - 1))
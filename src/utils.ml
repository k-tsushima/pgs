open Syntax

let id x = x
let dummy_id x = Dummy

let rec replace_dummy data x = match data with
  | Dummy -> x
  | Con(d1, d2) -> Con(replace_dummy d1 x, replace_dummy d2 x)
  | Int(_) | Unit -> data

let rec construct_dummy x = match x with
  | Int _ | Dummy -> Dummy
  | Unit -> Unit
  | Con(a, b) -> Con(construct_dummy a, construct_dummy b)

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

(* removeInt Con(Int 1, Con(Int 2, Unit)) = Con(x, Con(x, Unit)) *)
let rec removeInt dat (x:data) = match dat with
  | Con(a, b) -> Con(removeInt(a) x, removeInt(b) x)
  | Int(y) -> x
  | Unit -> Unit
  | Dummy -> Dummy

let rec getData dat (x:data) = match (dat,x) with 
  | (Unit, z) -> None
  | (Int(y),z) -> Some(z)
  | (Con(a, b), Con(c,d)) -> 
    let a' = getData(a) c in
    let b' = getData(b) d in
    (
      match (a',b') with
      | (None, None) -> None
      | (None, Some(y)) -> Some(y)
      | (Some(y), None) -> Some(y)
      | (Some(y1), Some(y2)) -> if y1 = y2 then Some(y1) else None
    )
  | _ -> assert false

let getData dat (x:data) =
  let dat' = getData dat x in
  match dat' with
  | None -> assert false
  | Some(y) -> y
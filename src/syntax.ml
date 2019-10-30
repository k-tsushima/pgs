type data = 
	| Con of data * data
	| Int of int
	| Dummy
	| Unit

type bigul =
	| Skip of (data -> data) 
	| Replace
	| Prod of bigul * bigul
	| RearrS of (data -> data) * (data -> data) * bigul
	| RearrV of (data -> data) * (data -> data) * bigul
	| Case of (data -> data -> bool) * (data -> bool) * bigul * bigul
	| Compose of bigul * bigul
	| Var of string
	| Def of string * bigul * bigul

let debug_flg = false

let rec print_data data = match data with
	| Unit -> "unit"
	| Con(d1, d2) -> "con(" ^ (print_data d1) ^ ", " ^ (print_data d2) ^ ")"
	| Int n -> string_of_int n
	| Dummy -> "dummy"

let rec print bx = match bx with
	| Def(_) -> "def"
	| Var(_) -> "var"
	| Skip(_) -> "skip"
	| Replace -> "replace"
	| Prod(_) -> "prod"
	| RearrS(_) -> "rearrS"
	| RearrV(_) -> "rearrV"
	| Compose(bx1, bx2) -> "compose(" ^ (print bx1) ^ "," ^ (print bx2) ^ ")"
	| Case(_) -> "case"
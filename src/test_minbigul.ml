open Syntax
open Bxprog
open Utils
open Minbigul

(* ========================= get ========================= *)

let get_skip1 =
  let s = Con(Int 1, Int 2) in
  let v = get skip1 s [] in
  v

let get_replace = 
  let s = Con(Int 1, Int 2) in
  let v = get Replace s [] in
  v

let get_rearrS =
  let s = Int 1 in
  let v = get rearrS_d s [] in
  v

let get_prod =
  let s = Con(Int 1, Int 2) in
  let v = get prod_rs s [] in
  v

let get_rearrV =
  let s = Con(Int 1, Int 1) in
  let v = get rearrV_d s [] in
  v

let get_phead =
  let s = Con(Int 1, Con(Int 2, Unit)) in
  let v = get phead s [] in
  v

let get_phead2 =
  let s = Con(Int 1, Con(Int 2, Unit)) in
  let v = get phead2 s [] in
  v

let get_replaceall =
  let s = Con(Int 1, Con(Int 1, Unit)) in
  let v = get replaceAlldef s [] in
  v

let get_bsnoc =
  let s = (Con(Int 2, (Con(Int 1, Unit)))) in
  let v = get bsnoc_def s [] in
  v

let get_lassoc_comp_replace n =
  let s = Int 1 in
  let v = get (lassoc_comp Replace n) s [] in
  v

let get_rassoc_comp_replace n =
  let s = Int 1 in
  let v = get (rassoc_comp Replace n) s [] in
  v

let get_lassoc_comp_phead n =
  let s = make_smallest_nested_list (n + 1) in
  let v = get (lassoc_comp phead n) s [] in
  v

let get_rassoc_comp_phead n =
  let s = make_smallest_nested_list (n + 1) in
  let v = get (rassoc_comp phead n) s [] in
  v

let get_lassoc_comp_phead2 n =
  let s = make_consecutive_list n in
  let v = get (lassoc_comp phead2 n) s [] in
  v

let get_rassoc_comp_phead2 n =
  let s = make_consecutive_list n in
  let v = get (rassoc_comp phead2 n) s [] in
  v

(* ========================= put ========================= *)

let put_skip1 =
  let s = Con(Int 1, Int 2) in
  let v = Unit in
  let s' = put skip1 s v [] in
  s'

let put_replace = 
  let s = Con(Int 1, Int 2) in
  let v = Con(Int 3, Int 4) in
  let s' = put Replace s v [] in
  s'

let put_rearrS =
  let s = Int 1 in
  let v = Con(Int 100, Int 100) in
  let s' = put rearrS_d s v [] in
  s'

let put_rearrV =
  let s = Con(Int 1, Int 1) in
  let v = Int 100 in
  let s' = put rearrV_d s v [] in
  s'

let put_prod =
  let s = Con(Int 1, Int 2) in
  let v = Con(Int 100, Unit) in
  let s' = put prod_rs s v [] in
  s'

let put_phead =
  let s = Con(Int 1, Con(Int 2, Unit)) in
  let v = Int 100 in
  let s' = put phead s v [] in
  s'

let put_ptail =
  let s = Con(Int 1, Con(Int 2, Con(Int 3, Con(Int 4, Unit)))) in 
  let v = Con(Int 100, Con(Int 101, Unit)) in 
  let s' = put ptail s v [] in
  s'

let put_phead2 =
  let s = Con(Int 1, Con(Int 2, Unit)) in
  let v = Con(Int 100, Unit) in
  let s' = put phead2 s v [] in
  s'

let put_replaceall =
  let s = Con(Int 1, Con(Int 1, Unit)) in
  let v = Int 100 in
  let s' = put replaceAlldef s v [] in
  s'

let put_bsnoc =
  let s = (Con(Int 2, (Con(Int 1, Unit)))) in
  let v = (Con(Int 3, (Con(Int 4, Unit)))) in
  let s' = put bsnoc_def s v [] in
  s'

let put_lassoc_comp_replace n =
  let s = make_consecutive_list n in
  let v = make_consecutive_list n in
  let s' = put (lassoc_comp Replace n) s v [] in
  s'

let put_rassoc_comp_replace n =
  let s = make_consecutive_list n in
  let v = make_consecutive_list n in
  let s' = put (rassoc_comp Replace n) s v [] in
  s'

let put_lassoc_comp_phead n =
  let s = make_smallest_nested_list (n + 1) in
  let v = Int 100 in
  let s' = put (lassoc_comp phead n) s v [] in
  s'

let put_rassoc_comp_phead n =
  let s = make_smallest_nested_list (n + 1) in
  let v = Int 100 in
  let s' = put (rassoc_comp phead n) s v [] in
  s'

let put_lassoc_comp_phead_2 n =
  let s = make_list_for_phead (n + 1) (100000) in
  let v = Int 100 in
  let s' = put (lassoc_comp phead n) s v [] in
  s'

let put_rassoc_comp_phead_2 n =
  let s = make_list_for_phead (n + 1) (100000) in
  let v = Int 100 in
  let s' = put (rassoc_comp phead n) s v [] in
  s'

let put_lassoc_comp_ptail n = 
  let s = make_consecutive_nested_list (n + 3) in 
  let v = make_consecutive_nested_list (10000) in 
  let s' = put (lassoc_comp ptail n) s v [] in 
  s' 

let put_rassoc_comp_ptail n = 
  let s = make_consecutive_nested_list (n + 3) in 
  let v = make_consecutive_nested_list (10000) in 
  let s' = put (rassoc_comp ptail n) s v [] in 
  s'

let put_lassoc_comp_phead2 n =
  let s = make_consecutive_list n in
  let v = Con(Int 100, Unit) in
  let s' = put (lassoc_comp phead2 n) s v [] in
  s'

let put_rassoc_comp_phead2 n =
  let s = make_consecutive_list n in
  let v = Con(Int 100, Unit) in
  let s' = put (rassoc_comp phead2 n) s v [] in
  s'

let put_breverse n =
  let s = make_consecutive_list n in
  let v = make_consecutive_list n in 
  let s' = put breverse s v [] in 
  s'

let put_lassoc_comp_breverse n =
  let s = make_consecutive_list 100 in 
  let v = make_consecutive_list 100 in 
  let s' = put (lassoc_comp breverse n) s v [] in 
  s'

let put_rassoc_comp_breverse n =
  let s = make_consecutive_list 100 in 
  let v = make_consecutive_list 100 in 
  let s' = put (rassoc_comp breverse n) s v [] in 
  s'

let put_bmapreplace n =
  let s = make_consecutive_list n in
  let v = make_consecutive_list_2 n in
  let s' = put bmapreplace s v [] in 
  s'

let put_lassoc_comp_bsnoc n =
  let s = make_consecutive_list (n + 1) in 
  let v = make_consecutive_list_2 (n + 1) in 
  let s' = put (lassoc_comp bsnoc_def n) s v [] in 
  s'

let put_rassoc_comp_bsnoc n =
  let s = make_consecutive_list (n + 1) in 
  let v = make_consecutive_list_2 (n + 1) in 
  let s' = put (rassoc_comp bsnoc_def n) s v [] in 
  s'

(* ========================= put (complex data )========================= *)

let put_lassoc_comp_bsnoc_nested_list n =
  let s = make_list_of_binary_list (n + 1) in 
  let v = s in 
  let s' = put (lassoc_comp bsnoc_def n) s v [] in 
  s'

let put_rassoc_comp_bsnoc_nested_list n =
  let s = make_list_of_binary_list (n + 1) in 
  let v = s in 
  let s' = put (rassoc_comp bsnoc_def n) s v [] in 
  s'

let put_lassoc_comp_ptail_nested_list n =
  let s = make_list_of_binary_list (n + 1) in 
  let v = s in 
  let s' = put (lassoc_comp ptail n) s v [] in 
  s'

let put_rassoc_comp_ptail_nested_list n =
  let s = make_list_of_binary_list (n + 1) in 
  let v = s in 
  let s' = put (rassoc_comp ptail n) s v [] in 
  s'

let put_lassoc_comp_replace_nested_list n =
  let s = make_binary_list 10000 in 
  let v = make_binary_list 10000 in 
  let s' = put (lassoc_comp Replace n) s v [] in 
  s'

let put_rassoc_comp_replace_nested_list n =
  let s = make_binary_list 10000 in 
  let v = make_binary_list 10000 in 
  let s' = put (rassoc_comp Replace n) s v [] in 
  s'

let put_breverse_nested_list n =
  let s = make_list_of_binary_list n in
  let v = make_list_of_binary_list n in 
  let s' = put breverse s v [] in 
  s'

let put_bmapreplace_nested_list n =
  let s = make_list_of_binary_list n in
  let v = make_list_of_binary_list n in 
  let s' = put bmapreplace s v [] in 
  s'

let put_rassoc_comp_breverse_nested_list n = 
  let s = make_list_of_binary_list n in 
  let v = make_list_of_binary_list n in 
  let s' = put (rassoc_comp breverse n) s v [] in 
  s'
open Syntax
open Bxprog
open Utils
open Minbigul_m

(* ========================= get_m ========================= *)
let get_m_skip1 =
  let s = Con(Int 1, Int 2) in
  let v = get_m skip1 s [] in
  v

let get_m_replace = 
  let s = Con(Int 1, Int 2) in
  let v = get_m Replace s [] in
  v

let get_m_rearrS =
  let s = Int 1 in
  let v = get_m rearrS_d s [] in
  v

let get_m_rearrV =
  let s = Con(Int 1, Int 1) in
  let v = get_m rearrV_d s [] in
  v

let get_m_prod =
  let s = Con(Int 1, Int 2) in
  let v = get_m prod_rs s [] in
  v

let get_m_phead =
  let s = Con(Int 1, Con(Int 2, Unit)) in
  let v = get_m phead s [] in
  v

let get_m_phead2 =
  let s = Con(Int 1, Con(Int 2, Unit)) in
  let v = get_m phead2 s [] in
  v

let get_m_replaceall =
  let s = Con(Int 1, Con(Int 1, Unit)) in
  let v = get_m replaceAlldef s [] in
  v

let get_m_bsnoc =
  let s = Con(Int 2, (Con(Int 1, Unit))) in
  let v = get_m bsnoc_def s [] in
  v

let get_m_lassoc_comp_replace n =
  let s = Int 1 in
  let v = get_m (lassoc_comp Replace n) s [] in
  v

let get_m_rassoc_comp_replace n =
  let s = Int 1 in
  let v = get_m (rassoc_comp Replace n) s [] in
  v

let get_m_lassoc_comp_phead n =
  let s = make_smallest_nested_list (n + 1) in
  let v = get_m (lassoc_comp phead n) s [] in
  v

let get_m_rassoc_comp_phead n =
  let s = make_smallest_nested_list (n + 1) in
  let v = get_m (rassoc_comp phead n) s [] in
  v

let get_m_lassoc_comp_phead2 n =
  let s = make_consecutive_list n in
  let v = get_m (lassoc_comp phead2 n) s [] in
  v

let get_m_rassoc_comp_phead2 n =
  let s = make_consecutive_list n in
  let v = get_m (rassoc_comp phead2 n) s [] in
  v

let get_m_breverse n =
  let s = make_consecutive_list n in
  let v = get_m breverse s [] in  
  v

let get_m_bmapreplace n =
  let s = make_consecutive_list n in
  let v = get_m bmapreplace s [] in  
  v

(* ========================= put_m ========================= *)

let put_m_skip1 =
  let s = Con(Int 1, Int 2) in
  let v = Unit in
  let s' = put_m skip1 s v [] in
  s'

let put_m_replace = 
  let s = Con(Int 1, Int 2) in
  let v = Con(Int 3, Int 4) in
  let s' = put_m Replace s v [] in
  s'

let put_m_rearrS =
  let s = Int 1 in
  let v = Con(Int 100, Int 100) in
  let s' = put_m rearrS_d s v [] in
  s'

let put_m_rearrV =
  let s = Con(Int 1, Int 1) in
  let v = Int 100 in
  let s' = put_m rearrV_d s v [] in
  s'

let put_m_prod =
  let s = Con(Int 1, Int 2) in
  let v = Con(Int 100, Unit) in
  let s' = put_m prod_rs s v [] in
  s'

let put_m_phead =
  let s = Con(Int 1, Con(Int 2, Unit)) in
  let v = Int 100 in
  let s' = put_m phead s v [] in
  s'

let put_m_phead2 =
  let s = Con(Int 1, Con(Int 2, Unit)) in
  let v = Con(Int 100, Unit) in
  let s' = put_m phead2 s v [] in
  s'
let put_m_replaceall =
  let s = Con(Int 1, Con(Int 1, Unit)) in
  let v = Int 100 in
  let s' = put_m replaceAlldef s v [] in
  s'

let put_m_bsnoc =
  let s = (Con(Int 2, (Con(Int 1, Unit)))) in
  let v = (Con(Int 3, (Con(Int 4, Unit)))) in
  let s' = put_m bsnoc_def s v [] in
  s'

let put_m_lassoc_comp_replace n =
  let s = Int 1 in
  let v = Int 100 in
  let s' = put_m (lassoc_comp Replace n) s v [] in
  s'

let put_m_rassoc_comp_replace n =
  let s = Int 1 in
  let v = Int 100 in
  let s' = put_m (rassoc_comp Replace n) s v [] in
  s'

let put_m_lassoc_comp_phead n =
  let s = make_smallest_nested_list (n + 1) in
  let v = Int 100 in
  let s' = put_m (lassoc_comp phead n) s v [] in
  s'

let put_m_rassoc_comp_phead n =
  let s = make_smallest_nested_list (n + 1) in
  let v = Int 100 in
  let s' = put_m (rassoc_comp phead n) s v [] in
  s'

let put_m_lassoc_comp_phead_2 n =
  let s = make_list_for_phead (n + 1) (1000) in
  let v = Int 100 in
  let s' = put_m (lassoc_comp phead n) s v [] in
  s'

let put_m_rassoc_comp_phead_2 n =
  let s = make_list_for_phead (n + 1) (100000) in
  let v = Int 100 in
  let s' = put_m (rassoc_comp phead n) s v [] in
  s'

let put_m_lassoc_comp_ptail n = 
  let s = make_consecutive_list (n + 3) in 
  let v = make_consecutive_list (10000) in 
  let s' = put_m (lassoc_comp ptail n) s v [] in 
  s' 

let put_m_rassoc_comp_ptail n = 
  let s = make_consecutive_list (n + 3) in 
  let v = make_consecutive_list (10000) in 
  let s' = put_m (rassoc_comp ptail n) s v [] in 
  s' 

let put_m_lassoc_comp_phead2 n =
  let s = make_consecutive_list n in
  let v = Con(Int 100, Unit) in
  let s' = put_m (lassoc_comp phead2 n) s v [] in
  s'

let put_m_rassoc_comp_phead2 n =
  let s = make_consecutive_list n in
  let v = Con(Int 100, Unit) in
  let s' = put_m (rassoc_comp phead2 n) s v [] in
  s'

let put_m_breverse n =
  let s = make_consecutive_list n in
  let v = make_consecutive_list n in
  let s' = put_m breverse s v [] in
  s'

let put_m_lassoc_comp_breverse n =
  let s = make_consecutive_list 100 in 
  let v = make_consecutive_list 100 in 
  let s' = put_m (lassoc_comp breverse n) s v [] in 
  s'

let put_m_rassoc_comp_breverse n =
  let s = make_consecutive_list 100 in 
  let v = make_consecutive_list 100 in 
  let s' = put_m (rassoc_comp breverse n) s v [] in 
  s'

let put_m_bmapreplace n =
  let s = make_consecutive_list n in
  let v = make_consecutive_list_2 n in
  let s' = put_m bmapreplace s v [] in 
  s'

let put_m_lassoc_comp_bsnoc n =
  let s = make_consecutive_list (n + 1) in 
  let v = make_consecutive_list_2 (n + 1) in 
  let s' = put_m (lassoc_comp bsnoc_def n) s v [] in 
  s'

let put_m_rassoc_comp_bsnoc n =
  let s = make_consecutive_list (n + 1) in 
  let v = make_consecutive_list_2 (n + 1) in 
  let s' = put_m (rassoc_comp bsnoc_def n) s v [] in 
  s'

(* ========================= put_m (complex data )========================= *)

let put_m_lassoc_comp_bsnoc_nested_list n =
  count_not_found := 0;
  let s = make_list_of_binary_tree n in 
  let v = make_list_of_binary_tree_2 n in 
  let s' = put_m (lassoc_comp bsnoc_def n) s v [] in 
  Printf.printf "%d\n" !count_not_found;
  s'

let put_m_rassoc_comp_bsnoc_nested_list n =
  let s = make_list_of_binary_tree n in 
  let v = make_list_of_binary_tree_2 n in 
  let s' = put_m (rassoc_comp bsnoc_def n) s v [] in 
  s'

let put_m_lassoc_comp_ptail_nested_list n =
  let s = make_list_of_binary_list (n + 1) in 
  let v = s in 
  let s' = put_m (lassoc_comp ptail n) s v [] in 
  s'

let put_m_rassoc_comp_ptail_nested_list n =
  let s = make_list_of_binary_list (n + 1) in 
  let v = s in 
  let s' = put_m (rassoc_comp ptail n) s v [] in 
  s'

let put_m_lassoc_comp_replace_nested_list n =
  let s = make_binary_list 10000 in 
  let v = make_binary_list 10000 in 
  let s' = put_m (lassoc_comp Replace n) s v [] in 
  s'

let put_m_rassoc_comp_replace_nested_list n =
  let s = make_binary_list 10000 in 
  let v = make_binary_list 10000 in 
  let s' = put_m (rassoc_comp Replace n) s v [] in 
  s'

let put_m_breverse_nested_list n =
  count_not_found := 0;
  let s = make_list_of_binary_list n in
  let v = make_list_of_binary_list n in 
  let s' = put_m breverse s v [] in 
  Printf.printf "%d\n" !count_not_found;
  s'

let put_m_bmapreplace_nested_list n =
  let s = make_list_of_binary_list n in
  let v = make_list_of_binary_list n in 
  let s' = put_m bmapreplace s v [] in 
  s'

let put_m_rassoc_comp_breverse_nested_list n = 
  let s = make_list_of_binary_list n in 
  let v = make_list_of_binary_list n in 
  let s' = put_m (rassoc_comp breverse n) s v [] in 
  s'
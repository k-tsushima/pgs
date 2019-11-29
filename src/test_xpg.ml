open Syntax
open Bxprog
open Utils
open Xpg

let xpg_skip1 =
  let s = Con(Int 1, Int 2) in
  let v = Unit in
  let (s, v) = xpg skip1 s v [] in
  (s, v)

let xpg_replace = 
  let s = Con(Int 1, Int 2) in
  let v = Con(Int 3, Int 4) in
  let (s, v) = xpg Replace s v [] in
  (s, v)

let xpg_rearrS =
  let s = Int 1 in
  let v = Con(Int 100, Int 100) in
  let (s, v) = xpg rearrS_d s v [] in
  (s, v)

let xpg_rearrV =
  let s = Con(Int 1, Int 1) in
  let v = Int 100 in
  let (s, v) = xpg rearrV_d s v [] in
  (s, v)

let xpg_prod =
  let s = Con(Int 1, Int 2) in
  let v = Con(Int 100, Unit) in
  let (s, v) = xpg prod_rs s v [] in
  (s, v)

let xpg_phead =
  let s = Con(Int 1, Con(Int 2, Unit)) in
  let v = Int 100 in
  let (s, v) = xpg phead s v [] in
  (s, v)

let xpg_phead2 =
  let s = Con(Int 1, Con(Int 2, Unit)) in
  let v = Con(Int 100, Unit) in
  let (s, v) = xpg phead2 s v [] in
  (s, v)

let xpg_replaceall =
  let s = Con(Int 1, Con(Int 1, Unit)) in
  let v = Int 100 in
  let (s, v) = xpg replaceAlldef s v [] in
  (s, v)

let xpg_bsnoc =
  let s = (Con(Int 2, (Con(Int 1, Unit)))) in
  let v = (Con(Int 3, (Con(Int 4, Unit)))) in
  let (s, v) = xpg bsnoc_def s v [] in
  (s, v)

let xpg_lassoc_comp_replace n =
  let s = make_consecutive_list n in
  let v = make_consecutive_list n in
  let (s, v) = xpg (lassoc_comp Replace n) s v [] in
  (s, v)

let xpg_rassoc_comp_replace n =
  let s = make_consecutive_list n in
  let v = make_consecutive_list n in
  let (s, v) = xpg (rassoc_comp Replace n) s v [] in
  (s, v)

let xpg_lassoc_comp_phead n =
  let s = make_smallest_nested_list (n + 1) in
  let v = Int 100 in
  let (s, v) = xpg (lassoc_comp phead n) s v [] in
  (s, v)

let xpg_rassoc_comp_phead n =
  let s = make_smallest_nested_list (n + 1) in
  let v = Int 100 in
  let (s, v) = xpg (rassoc_comp phead n) s v [] in
  (s, v)

let xpg_lassoc_comp_phead_2 n =
  let s = make_binary_list (n + 1) in
  let v = Int 100 in
  let (s, v) = xpg (lassoc_comp phead n) s v [] in
  (s, v)

let xpg_rassoc_comp_phead_2 n =
  let s = make_list_for_phead (n + 1) (100000) in
  let v = Int 100 in
  let (s, v) = xpg (rassoc_comp phead n) s v [] in
  (s, v)

let xpg_lassoc_comp_ptail n = 
  let s = make_consecutive_list (n + 3) in 
  let v = make_consecutive_list (10000) in 
  let (s, v) = xpg (lassoc_comp ptail n) s v [] in 
  (s, v)

let xpg_rassoc_comp_ptail n = 
  let s = make_consecutive_list (n + 3) in 
  let v = make_consecutive_list (10000) in 
  let (s, v) = xpg (rassoc_comp ptail n) s v [] in 
  (s, v)

let xpg_lassoc_comp_phead2 n =
  let s = make_consecutive_list n in
  let v = Con(Int 100, Unit) in
  let (s, v) = xpg (lassoc_comp phead2 n) s v [] in
  (s, v)

let xpg_rassoc_comp_phead2 n =
  let s = make_consecutive_list n in
  let v = Con(Int 100, Unit) in
  let (s, v) = xpg (rassoc_comp phead2 n) s v [] in
  (s, v)

let xpg_breverse n =
  let s = make_consecutive_list n in
  let v = make_consecutive_list n in 
  let (s, v) = xpg breverse s v [] in 
  (s, v)

let xpg_lassoc_comp_breverse n =
  let s = make_consecutive_list 100 in 
  let v = make_consecutive_list 100 in 
  let s' = xpg (lassoc_comp breverse n) s v [] in 
  s'

let xpg_rassoc_comp_breverse n =
  let s = make_consecutive_list 100 in 
  let v = make_consecutive_list 100 in 
  let s' = xpg (rassoc_comp breverse n) s v [] in 
  s'

let xpg_bmapreplace n =
  let s = make_consecutive_list n in
  let v = make_consecutive_list_2 n in 
  let (s, v) = xpg bmapreplace s v [] in 
  (s, v)

let xpg_lassoc_comp_bsnoc n =
  let s = make_consecutive_list (n + 1) in 
  let v = make_consecutive_list_2 (n + 1) in 
  let (s, v) = xpg (lassoc_comp bsnoc_def n) s v [] in 
  (s, v)

let xpg_rassoc_comp_bsnoc n =
  let s = make_consecutive_list (n + 1) in 
  let v = make_consecutive_list_2 (n + 1) in 
  let (s, v) = xpg (rassoc_comp bsnoc_def n) s v [] in 
  (s, v)

(* ========================= xpg (complex data )========================= *)
let xpg_lassoc_comp_bsnoc_ldata n =
  let s = make_list_of_binary_tree n in 
  let v = s in 
  let (s, v) = xpg (lassoc_comp bsnoc_def n) s v [] in 
  (s, v)

let xpg_rassoc_comp_bsnoc_ldata n =
  let s = make_list_of_binary_tree n in 
  let v = s in 
  let (s, v) = xpg (rassoc_comp bsnoc_def n) s v [] in 
  (s, v)

let xpg_lassoc_comp_ptail_ldata n =
  let s = make_list_of_binary_tree (n + 1) in 
  let v = make_list 1 10 in 
  let (s, v) = xpg (lassoc_comp ptail n) s v [] in 
  (s, v)

let xpg_rassoc_comp_ptail_ldata n =
  let s = make_list_of_binary_tree (n + 1) in 
  let v = make_list 1 10 in 
  let (s, v) = xpg (rassoc_comp ptail n) s v [] in 
  (s, v)

let xpg_lassoc_comp_replace_ldata n =
  let s = make_list_of_binary_tree n in 
  let v = make_list_of_binary_tree n in 
  let (s, v) = xpg (lassoc_comp Replace n) s v [] in 
  (s, v)

let xpg_rassoc_comp_replace_ldata n =
  let s = make_list_of_binary_tree n in 
  let v = make_list_of_binary_tree n in 
  let (s, v) = xpg (rassoc_comp Replace n) s v [] in 
  (s, v)

let xpg_breverse_ldata n =
  let s = make_list_of_binary_tree n in
  let v = s in
  let (s, v) = xpg breverse s v [] in
  (s, v)

let xpg_bmapreplace_ldata n =
  let s = make_list_of_binary_tree n in
  let v = s in
  let (s, v) = xpg bmapreplace s v [] in
  (s, v)
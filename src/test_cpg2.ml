open Syntax
open Bxprog
open Utils
open Cpg2

let cpg2_skip1 =
  let s = Con(Int 1, Int 2) in
  let v = Unit in
  let (s, v) = cpg2 skip1 s v [] in
  (s, v)

let cpg2_replace = 
  let s = Con(Int 1, Int 2) in
  let v = Con(Int 3, Int 4) in
  let (s, v) = cpg2 Replace s v [] in
  (s, v)

let cpg2_rearrS =
  let s = Int 1 in
  let v = Con(Int 100, Int 100) in
  let (s, v) = cpg2 rearrS_d s v [] in
  (s, v)

let cpg2_rearrV =
  let s = Con(Int 1, Int 1) in
  let v = Int 100 in
  let (s, v) = cpg2 rearrV_d s v [] in
  (s, v)

let cpg2_prod =
  let s = Con(Int 1, Int 2) in
  let v = Con(Int 100, Unit) in
  let (s, v) = cpg2 prod_rs s v [] in
  (s, v)

let cpg2_phead =
  let s = Con(Int 1, Con(Int 2, Unit)) in
  let v = Int 100 in
  let (s, v) = cpg2 phead s v [] in
  (s, v)

let cpg2_phead2 =
  let s = Con(Int 1, Con(Int 2, Unit)) in
  let v = Con(Int 100, Unit) in
  let (s, v) = cpg2 phead2 s v [] in
  (s, v)

let cpg2_replaceall =
  let s = Con(Int 1, Con(Int 1, Unit)) in
  let v = Int 100 in
  let (s, v) = cpg2 replaceAlldef s v [] in
  (s, v)

let cpg2_bsnoc =
  let s = (Con(Int 2, (Con(Int 1, Unit)))) in
  let v = (Con(Int 3, (Con(Int 4, Unit)))) in
  let (s, v) = cpg2 bsnoc_def s v [] in
  (s, v)

let cpg2_lassoc_comp_replace n =
  let s = Int 1 in
  let v = Int 100 in
  let (s, v) = cpg2 (lassoc_comp Replace n) s v [] in
  (s, v)

let cpg2_rassoc_comp_replace n =
  let s = Int 1 in
  let v = Int 100 in
  let (s, v) = cpg2 (rassoc_comp Replace n) s v [] in
  (s, v)

let cpg2_lassoc_comp_phead n =
  let s = make_smallest_nested_list (n + 1) in
  let v = Int 100 in
  let (s, v) = cpg2 (lassoc_comp phead n) s v [] in
  (s, v)

let cpg2_rassoc_comp_phead n =
  let s = make_smallest_nested_list (n + 1) in
  let v = Int 100 in
  let (s, v) = cpg2 (rassoc_comp phead n) s v [] in
  (s, v)

let cpg2_lassoc_comp_phead2 n =
  let s = make_consecutive_list n in
  let v = Con(Int 100, Unit) in
  let (s, v) = cpg2 (lassoc_comp phead2 n) s v [] in
  (s, v)

let cpg2_rassoc_comp_phead2 n =
  let s = make_consecutive_list n in
  let v = Con(Int 100, Unit) in
  let (s, v) = cpg2 (rassoc_comp phead2 n) s v [] in
  (s, v)

let cpg2_breverse n =
  let s = make_consecutive_list n in
  let v = make_consecutive_list n in
  let (s, v) = cpg2 breverse s v [] in
  (s, v) 

let cpg2_lassoc_comp_breverse n =
  let s = make_consecutive_list n in
  let v = make_consecutive_list n in
  let (s, v) = cpg2 (lassoc_comp breverse n) s v [] in
  (s, v) 

let cpg2_rassoc_comp_breverse n =
  let s = make_consecutive_list n in
  let v = make_consecutive_list n in
  let (s, v) = cpg2 (rassoc_comp breverse n) s v [] in
  (s, v) 

let cpg2_bmapreplace n =
  let s = make_consecutive_list n in
  let v = make_consecutive_list n in
  let (s, v) = cpg2 bmapreplace s v [] in
  (s, v)

let cpg2_lassoc_comp_bsnoc n =
  let s = make_consecutive_list (n + 1) in
  let v = make_consecutive_list_2 (n + 1) in
  let (s, v) = cpg2 (lassoc_comp bsnoc_def n) s v [] in
  (s, v) 

let cpg2_rassoc_comp_bsnoc n =
  let s = make_consecutive_list (n + 1) in
  let v = make_consecutive_list_2 (n + 1) in
  let (s, v) = cpg2 (rassoc_comp bsnoc_def n) s v [] in
  (s, v) 

(* ========================= cpg2 (complex data )========================= *)
let cpg2_lassoc_comp_bsnoc_nested_list n =
  let s = make_smallest_nested_list n in
  let v = make_smallest_nested_list n in
  let (s, v) = cpg2 (lassoc_comp bsnoc_def n) s v [] in
  (s, v) 

let cpg2_rassoc_comp_bsnoc_nested_list n =
  let s = make_smallest_nested_list n in
  let v = make_smallest_nested_list n in
  let (s, v) = cpg2 (rassoc_comp bsnoc_def n) s v [] in
  (s, v) 

let cpg2_lassoc_comp_replace_nested_list n =
  let s = make_smallest_nested_list n in
  let v = make_smallest_nested_list n in
  let (s, v) = cpg2 (lassoc_comp Replace n) s v [] in
  (s, v) 

let cpg2_rassoc_comp_replace_nested_list n =
  let s = make_smallest_nested_list n in
  let v = make_smallest_nested_list n in
  let (s, v) = cpg2 (rassoc_comp Replace n) s v [] in
  (s, v) 

let cpg2_breverse_nested_list n =
  let s = make_consecutive_nested_list n in
  let v = make_consecutive_nested_list n in
  let (s, v) = cpg2 breverse s v [] in
  (s, v) 

let cpg2_bmapreplace_nested_list n =
  let s = make_consecutive_nested_list n in
  let v = make_consecutive_nested_list n in
  let (s, v) = cpg2 bmapreplace s v [] in
  (s, v)
open Syntax
open Bxprog
open Utils
open Xpg
open Kpg

(* test xpg skip1 *)
let xpg_skip1 =
  let s = Con(Int 1, Int 2) in
  let v = Unit in
  let (s, v) = xpg skip1 s v [] in
  (s, v)

(* test xpg replace *)
let xpg_replace = 
  let s = Con(Int 1, Int 2) in
  let v = Con(Int 3, Int 4) in
  let (s, v) = xpg Replace s v [] in
  (s, v)

(* test xpg rearrS *)
let xpg_rearrS =
  let s = Int 1 in
  let v = Con(Int 100, Int 100) in
  let (s, v) = xpg rearrS_d s v [] in
  (s, v)

(* test xpg rearrV *)
let xpg_rearrV =
  let s = Con(Int 1, Int 1) in
  let v = Int 100 in
  let (s, v) = xpg rearrV_d s v [] in
  (s, v)

(* test xpg prod *)
let xpg_prod =
  let s = Con(Int 1, Int 2) in
  let v = Con(Int 100, Unit) in
  let (s, v) = xpg prod_rs s v [] in
  (s, v)

(* test xpg phead *)
let xpg_phead =
  let s = Con(Int 1, Con(Int 2, Unit)) in
  let v = Int 100 in
  let (s, v) = xpg phead s v [] in
  (s, v)

(* test xpg phead_with_case *)
let xpg_phead_with_case =
  let s = Con(Int 1, Con(Int 2, Unit)) in
  let v = Int 100 in
  let (s, v) = xpg phead_with_case s v [] in
  (s, v)

(* test xpg phead2 *)
let xpg_phead2 =
  let s = Con(Int 1, Con(Int 2, Unit)) in
  let v = Con(Int 100, Unit) in
  let (s, v) = xpg phead2 s v [] in
  (s, v)

(* test xpg phead2_with_case *)
let xpg_phead2_with_case =
  let s = Con(Int 1, Con(Int 2, Unit)) in
  let v = Con(Int 100, Unit) in
  let (s, v) = xpg phead2_with_case s v [] in
  (s, v)

(* test xpg replace all *)
let xpg_replaceall =
  let s = Con(Int 1, Con(Int 1, Unit)) in
  let v = Int 100 in
  let (s, v) = xpg replaceAlldef s v [] in
  (s, v)

(* test xpg snoc *)
let xpg_bsnoc =
  let s = (Con(Int 2, (Con(Int 1, Unit)))) in
  let v = (Con(Int 3, (Con(Int 4, Unit)))) in
  let (s, v) = xpg bsnoc_def s v [] in
  (s, v)

(* test xpg comp of n replace *)
let xpg_lassoc_comp_replace n =
  let s = Int 1 in
  let v = Int 100 in
  let (s, v) = xpg (lassoc_comp Replace n) s v [] in
  (s, v)

let xpg_rassoc_comp_replace n =
  let s = Int 1 in
  let v = Int 100 in
  let (s, v) = xpg (rassoc_comp Replace n) s v [] in
  (s, v)

(* test xpg comp of n phead *)
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

(* test xpg comp of n phead_with_case *)
let xpg_lassoc_comp_phead_with_case n =
  let s = make_smallest_nested_list (n + 1) in
  let v = Int 100 in
  let (s, v) = xpg (lassoc_comp phead_with_case n) s v [] in
  (s, v)

let xpg_rassoc_comp_phead_with_case n =
  let s = make_smallest_nested_list (n + 1) in
  let v = Int 100 in
  let (s, v) = xpg (rassoc_comp phead_with_case n) s v [] in
  (s, v)

(* test xpg comp of n phead2 *)
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

(* test xpg comp of n phead2_with_case *)
let xpg_lassoc_comp_phead2_with_case n =
  let s = make_consecutive_list n in
  let v = Con(Int 100, Unit) in
  let (s, v) = xpg (lassoc_comp phead2_with_case n) s v [] in
  (s, v)

let xpg_rassoc_comp_phead2_with_case n =
  let s = make_consecutive_list n in
  let v = Con(Int 100, Unit) in
  let (s, v) = xpg (rassoc_comp phead2_with_case n) s v [] in
  (s, v)

(* test xpg breverse *)
let xpg_breverse_1 =
  let s = (Con(Int 1, Unit)) in
  let v = (Con(Int 3, Unit)) in
  let (s, v) = xpg breverse s v [] in
  (s, v)

let xpg_breverse_2 =
  let s = (Con(Int 1, Con(Int 2, Unit))) in
  let v = (Con(Int 3, Con(Int 4, Unit))) in
  let (s, v) = xpg breverse s v [] in
  (s, v)

let xpg_breverse n =
  let s = make_consecutive_list n in
  let v = make_consecutive_list n in 
  let (s, v) = xpg breverse s v [] in 
  (s, v)

(* test xpg bmapreplace *)
let xpg_bmapreplace n =
  let s = make_consecutive_list n in
  let v = make_consecutive_list n in 
  let (s, v) = xpg bmapreplace s v [] in 
  (s, v)

(* test xpg bmapreplace_with_case *)
let xpg_bmapreplace_with_case n =
  let s = make_consecutive_list n in
  let v = make_consecutive_list n in 
  let (s, v) = xpg bmapreplace_with_case s v [] in 
  (s, v)

let xpg_bmapreplace_0 =
  let s = Con(Int 1, Con(Int 2, Unit)) in
  let v = Con(Int 3, Con(Int 4, Unit)) in 
  let (s, v) = xpg bmapreplace s v [] in 
  (s, v)

(* test xpg comp of n phead_with_multi_case *)
let xpg_lassoc_comp_phead_with_multi_case n =
  let s = make_smallest_nested_list (n + 1) in
  let v = Int 100 in
  let (s, v) = xpg (lassoc_comp phead_with_multi_case n) s v [] in
  (s, v)

let xpg_rassoc_comp_phead_with_multi_case n =
  let s = make_smallest_nested_list (n + 1) in
  let v = Int 100 in
  let (s, v) = xpg (rassoc_comp phead_with_multi_case n) s v [] in
  (s, v)

let xpg_lassoc_comp_phead_with_multi_case_2 n =
  let s = make_smallest_nested_list (n + 1) in
  let v = Int 100 in
  let (s, v) = xpg (lassoc_comp phead_with_multi_case_2 n) s v [] in
  (s, v)

let xpg_rassoc_comp_phead_with_multi_case_2 n =
  let s = make_smallest_nested_list (n + 1) in
  let v = Int 100 in
  let (s, v) = xpg (rassoc_comp phead_with_multi_case_2 n) s v [] in
  (s, v)

let xpg_lassoc_comp_replace_count n =
  count_xpg := 0;
  count_kpg := 0;
  let s = Int 1 in
  let v = Int 100 in
  let (s, v) = xpg (lassoc_comp Replace n) s v [] in
  !count_xpg + !count_kpg

let xpg_rassoc_comp_replace_count n =
  count_xpg := 0;
  count_kpg := 0;
  let s = Int 1 in
  let v = Int 100 in
  let (s, v) = xpg (rassoc_comp Replace n) s v [] in
  !count_xpg + !count_kpg

let xpg_lassoc_comp_phead_count n =
  count_xpg := 0;
  count_kpg := 0;
  let s = make_smallest_nested_list (n + 1) in
  let v = Int 100 in
  let (s, v) = xpg (lassoc_comp phead n) s v [] in
  !count_xpg + !count_kpg

let xpg_rassoc_comp_phead_count n =
  count_xpg := 0;
  count_kpg := 0;
  let s = make_smallest_nested_list (n + 1) in
  let v = Int 100 in
  let (s, v) = xpg (rassoc_comp phead n) s v [] in
  !count_xpg + !count_kpg

let xpg_breverse_count n =
  count_xpg := 0;
  count_kpg := 0;
  let s = make_consecutive_list n in
  let v = make_consecutive_list n in 
  let (s, v) = xpg breverse s v [] in 
  !count_xpg + !count_kpg

let xpg_bmapreplace_count n =
  count_xpg := 0;
  count_kpg := 0;
  let s = make_consecutive_list n in
  let v = make_consecutive_list n in 
  let (s, v) = xpg bmapreplace s v [] in 
  !count_xpg + !count_kpg
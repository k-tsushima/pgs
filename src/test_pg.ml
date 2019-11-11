open Syntax
open Bxprog
open Utils
open Pg

let pg_skip1 =
  let s = Con(Int 1, Int 2) in
  let v = Unit in
  let (s, v) = pg skip1 s v [] in
  (s, v)

let pg_replace = 
  let s = Con(Int 1, Int 2) in
  let v = Con(Int 3, Int 4) in
  let (s, v) = pg Replace s v [] in
  (s, v)

let pg_rearrS =
  let s = Int 1 in
  let v = Con(Int 100, Int 100) in
  let (s, v) = pg rearrS_d s v [] in
  (s, v)

let pg_rearrV =
  let s = Con(Int 1, Int 1) in
  let v = Int 100 in
  let (s, v) = pg rearrV_d s v [] in
  (s, v)

let pg_prod =
  let s = Con(Int 1, Int 2) in
  let v = Con(Int 100, Unit) in
  let (s, v) = pg prod_rs s v [] in
  (s, v)

let pg_phead =
  let s = Con(Int 1, Con(Int 2, Unit)) in
  let v = Int 100 in
  let (s, v) = pg phead s v [] in
  (s, v)

let pg_phead2 =
  let s = Con(Int 1, Con(Int 2, Unit)) in
  let v = Con(Int 100, Unit) in
  let (s, v) = pg phead2 s v [] in
  (s, v)

let pg_replaceall =
  let s = Con(Int 1, Con(Int 1, Unit)) in
  let v = Int 100 in
  let (s, v) = pg replaceAlldef s v [] in
  (s, v)

let pg_bsnoc =
  let s = (Con(Int 2, (Con(Int 1, Unit)))) in
  let v = (Con(Int 3, (Con(Int 4, Unit)))) in
  let (s, v) = pg bsnoc_def s v [] in
  (s, v)

let pg_lassoc_comp_replace n =
  let s = Int 1 in
  let v = Int 100 in
  let (s, v) = pg (lassoc_comp Replace n) s v [] in
  (s, v)

let pg_rassoc_comp_replace n =
  let s = Int 1 in
  let v = Int 100 in
  let (s, v) = pg (rassoc_comp Replace n) s v [] in
  (s, v)

let pg_lassoc_comp_phead n =
  let s = make_smallest_nested_list (n + 1) in
  let v = Int 100 in
  let (s, v) = pg (lassoc_comp phead n) s v [] in
  (s, v)

let pg_rassoc_comp_phead n =
  let s = make_smallest_nested_list (n + 1) in
  let v = Int 100 in
  let (s, v) = pg (rassoc_comp phead n) s v [] in
  (s, v)

let pg_lassoc_comp_phead_2 n =
  let s = make_list_for_phead (n + 1) (100000) in
  let v = Int 100 in
  let (s, v) = pg (lassoc_comp phead n) s v [] in
  (s, v)

let pg_rassoc_comp_phead_2 n =
  let s = make_list_for_phead (n + 1) (100000) in
  let v = Int 100 in
  let (s, v) = pg (rassoc_comp phead n) s v [] in
  (s, v)

let pg_lassoc_comp_ptail n = 
  let s = make_consecutive_nested_list (n + 3) in 
  let v = make_consecutive_nested_list (2) in 
  let (s, v) = pg (lassoc_comp ptail n) s v [] in 
  (s, v)

let pg_rassoc_comp_ptail n = 
  let s = make_consecutive_nested_list (n + 3) in 
  let v = make_consecutive_nested_list (2) in 
  let (s, v) = pg (rassoc_comp ptail n) s v [] in 
  (s, v)

let pg_lassoc_comp_ptail_2 n = 
  let s = make_consecutive_nested_list (n + 3) in 
  let v = make_consecutive_nested_list (1000) in 
  let (s, v) = pg (lassoc_comp ptail n) s v [] in 
  (s, v)

let pg_rassoc_comp_ptail_2 n = 
  let s = make_consecutive_nested_list (n + 3) in 
  let v = make_consecutive_nested_list (1000) in 
  let (s, v) = pg (rassoc_comp ptail n) s v [] in 
  (s, v)

let pg_lassoc_comp_phead2 n =
  let s = make_consecutive_list n in
  let v = Con(Int 100, Unit) in
  let (s, v) = pg (lassoc_comp phead2 n) s v [] in
  (s, v)

let pg_rassoc_comp_phead2 n =
  let s = make_consecutive_list n in
  let v = Con(Int 100, Unit) in
  let (s, v) = pg (rassoc_comp phead2 n) s v [] in
  (s, v)

let pg_breverse n =
  let s = make_consecutive_list n in
  let v = make_consecutive_list n in 
  let (s, v) = pg breverse s v [] in 
  (s, v)

let pg_lassoc_comp_breverse n =
  let s = make_consecutive_list n in
  let v = make_consecutive_list n in 
  let (s, v) = pg (lassoc_comp breverse n) s v [] in 
  (s, v)

let pg_rassoc_comp_breverse n =
  let s = make_consecutive_list n in
  let v = make_consecutive_list n in 
  let (s, v) = pg (rassoc_comp breverse n) s v [] in 
  (s, v)

let pg_bmapreplace n =
  let s = make_consecutive_list n in
  let v = make_consecutive_list n in 
  let (s, v) = pg bmapreplace s v [] in 
  (s, v)

let pg_lassoc_comp_bsnoc n =
  let s = make_consecutive_list (n + 1) in
  let v = make_consecutive_list_2 (n + 1) in 
  let (s, v) = pg (lassoc_comp bsnoc_def n) s v [] in 
  (s, v)

let pg_rassoc_comp_bsnoc n =
  let s = make_consecutive_list (n + 1) in
  let v = make_consecutive_list_2 (n + 1) in 
  let (s, v) = pg (rassoc_comp bsnoc_def n) s v [] in 
  (s, v)

(* ========================= pg (complex data )========================= *)
let pg_lassoc_comp_bsnoc_nested_list n =
  let s = make_smallest_nested_list n in
  let v = make_smallest_nested_list n in 
  let (s, v) = pg (lassoc_comp bsnoc_def n) s v [] in 
  (s, v)

let pg_rassoc_comp_bsnoc_nested_list n =
  let s = make_smallest_nested_list n in
  let v = make_smallest_nested_list n in 
  let (s, v) = pg (rassoc_comp bsnoc_def n) s v [] in 
  (s, v)

let pg_lassoc_comp_replace_nested_list n =
  let s = make_smallest_nested_list n in
  let v = make_smallest_nested_list n in 
  let (s, v) = pg (lassoc_comp Replace n) s v [] in 
  (s, v)

let pg_rassoc_comp_replace_nested_list n =
  let s = make_smallest_nested_list n in
  let v = make_smallest_nested_list n in 
  let (s, v) = pg (rassoc_comp Replace n) s v [] in 
  (s, v)

let pg_breverse_nested_list n =
  let s = make_consecutive_nested_list n in
  let v = make_consecutive_nested_list n in 
  let (s, v) = pg breverse s v [] in 
  (s, v)

let pg_bmapreplace_nested_list n =
  let s = make_consecutive_nested_list n in
  let v = make_consecutive_nested_list n in 
  let (s, v) = pg bmapreplace s v [] in 
  (s, v)
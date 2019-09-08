open Syntax
open Bxprog
open Utils
open Minbigul

(* test put skip1 *)
let put_skip1 =
  let s = Con(Int 1, Int 2) in
  let v = Unit in
  let s' = put skip1 s v [] in
  s'

(* test get skip1 *)
let get_skip1 =
  let s = Con(Int 1, Int 2) in
  let v = get skip1 s [] in
  v

(* test put replace *)
let put_replace = 
  let s = Con(Int 1, Int 2) in
  let v = Con(Int 3, Int 4) in
  let s' = put Replace s v [] in
  s'

(* test get replace *)
let get_replace = 
  let s = Con(Int 1, Int 2) in
  let v = get Replace s [] in
  v

(* test put rearrS *)
let put_rearrS =
  let s = Int 1 in
  let v = Con(Int 100, Int 100) in
  let s' = put rearrS_d s v [] in
  s'

(* test get rearrS *)
let get_rearrS =
  let s = Int 1 in
  let v = get rearrS_d s [] in
  v

(* test put rearrV *)
let put_rearrV =
  let s = Con(Int 1, Int 1) in
  let v = Int 100 in
  let s' = put rearrV_d s v [] in
  s'

(* test get rearrV *)
let get_rearrV =
  let s = Con(Int 1, Int 1) in
  let v = get rearrV_d s [] in
  v

(* test put prod *)
let put_prod =
  let s = Con(Int 1, Int 2) in
  let v = Con(Int 100, Unit) in
  let s' = put prod_rs s v [] in
  s'

(* test get prod *)
let get_prod =
  let s = Con(Int 1, Int 2) in
  let v = get prod_rs s [] in
  v

(* test put phead *)
let put_phead =
  let s = Con(Int 1, Con(Int 2, Unit)) in
  let v = Int 100 in
  let s' = put phead s v [] in
  s'

(* test get phead *)
let get_phead =
  let s = Con(Int 1, Con(Int 2, Unit)) in
  let v = get phead s [] in
  v

(* test put phead2 *)
let put_phead2 =
  let s = Con(Int 1, Con(Int 2, Unit)) in
  let v = Con(Int 100, Unit) in
  let s' = put phead2 s v [] in
  s'

(* test get phead2 *)
let get_phead2 =
  let s = Con(Int 1, Con(Int 2, Unit)) in
  let v = get phead2 s [] in
  v

(* test put replace all *)
let put_replaceall =
  let s = Con(Int 1, Con(Int 1, Unit)) in
  let v = Int 100 in
  let s' = put replaceAlldef s v [] in
  s'

(* test get replace all *)
let get_replaceall =
  let s = Con(Int 1, Con(Int 1, Unit)) in
  let v = get replaceAlldef s [] in
  v

(* test put bsnoc *)
let put_bsnoc =
  let s = (Con(Int 2, (Con(Int 1, Unit)))) in
  let v = (Con(Int 3, (Con(Int 4, Unit)))) in
  let s' = put bsnoc_def s v [] in
  s'

(* test get bsnoc *)
let get_bsnoc =
  let s = (Con(Int 2, (Con(Int 1, Unit)))) in
  let v = get bsnoc_def s [] in
  v

(* test put comp of n replace *)
let put_lassoc_comp_replace n =
  let s = Int 1 in
  let v = Int 100 in
  let s' = put (lassoc_comp Replace n) s v [] in
  s'

let put_rassoc_comp_replace n =
  let s = Int 1 in
  let v = Int 100 in
  let s' = put (rassoc_comp Replace n) s v [] in
  s'

(* test get comp of n replace *)
let get_lassoc_comp_replace n =
  let s = Int 1 in
  let v = get (lassoc_comp Replace n) s [] in
  v

let get_rassoc_comp_replace n =
  let s = Int 1 in
  let v = get (rassoc_comp Replace n) s [] in
  v

(* test put comp of n phead *)
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

(* test put comp of n phead_with_case *)
let put_lassoc_comp_phead_with_case n =
  let s = make_smallest_nested_list (n + 1) in
  let v = Int 100 in
  let s' = put (lassoc_comp phead_with_case n) s v [] in
  s'

let put_rassoc_comp_phead_with_case n =
  let s = make_smallest_nested_list (n + 1) in
  let v = Int 100 in
  let s' = put (rassoc_comp phead_with_case n) s v [] in
  s'

(* test get comp of n phead *)
let get_lassoc_comp_phead n =
  let s = make_smallest_nested_list (n + 1) in
  let v = get (lassoc_comp phead n) s [] in
  v

let get_rassoc_comp_phead n =
  let s = make_smallest_nested_list (n + 1) in
  let v = get (rassoc_comp phead n) s [] in
  v

(* test get comp of n phead_with_case *)
let get_lassoc_comp_phead_with_case n =
  let s = make_smallest_nested_list (n + 1) in
  let v = get (lassoc_comp phead_with_case n) s [] in
  v

let get_rassoc_comp_phead_with_case n =
  let s = make_smallest_nested_list (n + 1) in
  let v = get (rassoc_comp phead_with_case n) s [] in
  v

(* test put comp of n phead2 *)
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

(* test put comp of n phead2_with_case *)
let put_lassoc_comp_phead2_with_case n =
  let s = make_consecutive_list n in
  let v = Con(Int 100, Unit) in
  let s' = put (lassoc_comp phead2_with_case n) s v [] in
  s'

let put_rassoc_comp_phead2_with_case n =
  let s = make_consecutive_list n in
  let v = Con(Int 100, Unit) in
  let s' = put (rassoc_comp phead2_with_case n) s v [] in
  s'

(* test get comp of n phead2 *)
let get_lassoc_comp_phead2 n =
  let s = make_consecutive_list n in
  let v = get (lassoc_comp phead2 n) s [] in
  v

let get_rassoc_comp_phead2 n =
  let s = make_consecutive_list n in
  let v = get (rassoc_comp phead2 n) s [] in
  v

(* test get comp of n phead2_with_case *)
let get_lassoc_comp_phead2_with_case n =
  let s = make_consecutive_list n in
  let v = get (lassoc_comp phead2_with_case n) s [] in
  v

let get_rassoc_comp_phead2_with_case n =
  let s = make_consecutive_list n in
  let v = get (rassoc_comp phead2_with_case n) s [] in
  v

(* test put breverse *)
let put_breverse n =
  let s = make_consecutive_list n in
  let v = make_consecutive_list n in 
  let s' = put breverse s v [] in 
  s'

(* test get breverse *)
let get_breverse n =
  let s = make_consecutive_list n in
  let v = get breverse s [] in 
  v

(* test put bmapreplace *)
let put_bmapreplace n =
  let s = make_consecutive_list n in
  let v = make_consecutive_list n in
  let s' = put bmapreplace s v [] in 
  s'

(* test get bmapreplace *)
let get_bmapreplace n =
  let s = make_consecutive_list n in
  let v = get bmapreplace s [] in 
  v

(* test put bmapreplace_with_case *)
let put_bmapreplace_with_case n =
  let s = make_consecutive_list n in
  let v = make_consecutive_list n in
  let s' = put bmapreplace_with_case s v [] in 
  s'

(* test get bmapreplace_with_case *)
let get_bmapreplace_with_case n =
  let s = make_consecutive_list n in
  let v = get bmapreplace_with_case s [] in 
  v

(* test put comp of n phead_with_multi_case *)
let put_lassoc_comp_phead_with_multi_case n =
  let s = make_smallest_nested_list (n + 1) in
  let v = Int 100 in
  let s' = put (lassoc_comp phead_with_multi_case n) s v [] in
  s'

let put_rassoc_comp_phead_with_multi_case n =
  let s = make_smallest_nested_list (n + 1) in
  let v = Int 100 in 
  let s' = put (rassoc_comp phead_with_multi_case n) s v [] in
  s'

let put_lassoc_comp_phead_with_multi_case_2 n =
  let s = make_smallest_nested_list (n + 1) in
  let v = Int 100 in
  let s' = put (lassoc_comp phead_with_multi_case_2 n) s v [] in
  s'

let put_rassoc_comp_phead_with_multi_case_2 n =
  let s = make_smallest_nested_list (n + 1) in
  let v = Int 100 in 
  let s' = put (rassoc_comp phead_with_multi_case_2 n) s v [] in
  s'

(* let put_breverse_count n =
  count_put := 0;
  count_get := 0;
  let s = make_consecutive_list n in
  let v = make_consecutive_list n in 
  let s' = put breverse s v [] in 
  (!count_put, !count_get)

let put_bmapreplace_count n =
  count_put := 0;
  count_get := 0;
  let s = make_consecutive_list n in
  let v = make_consecutive_list n in 
  let s' = put bmapreplace s v [] in 
  (!count_put, !count_get)

let put_lassoc_comp_phead_count n =
  count_put := 0;
  count_get := 0;
  let s = make_smallest_nested_list (n + 1) in
  let v = Int 100 in
  let s' = put (lassoc_comp phead n) s v [] in
  (!count_put, !count_get)

let put_rassoc_comp_phead_count n =
  count_put := 0;
  count_get := 0;
  let s = make_smallest_nested_list (n + 1) in
  let v = Int 100 in
  let s' = put (rassoc_comp phead n) s v [] in
  (!count_put, !count_get)

let put_lassoc_comp_replace_count n =
  count_put := 0;
  count_get := 0;
  let s = Int 1 in
  let v = Int 100 in
  let s' = put (lassoc_comp Replace n) s v [] in
  (!count_put, !count_get)

let put_rassoc_comp_replace_count n =
  count_put := 0;
  count_get := 0;
  let s = Int 1 in
  let v = Int 100 in
  let s' = put (rassoc_comp Replace n) s v [] in
  (!count_put, !count_get) *)

(* let put_lassoc_comp_phead_get_time n =
  get_time := 0.0;
  let s = make_smallest_nested_list (n + 1) in
  let v = Int 100 in
  let s' = put (lassoc_comp phead n) s v [] in
  !get_time

let put_lassoc_comp_replace_get_time n =
  get_time := 0.0;
  let s = Int 1 in
  let v = Int 100 in
  let s' = put (lassoc_comp Replace n) s v [] in
  !get_time

let put_rassoc_comp_replace_get_time n =
  get_time := 0.0;
  let s = Int 1 in
  let v = Int 100 in
  let s' = put (rassoc_comp Replace n) s v [] in
  !get_time

let put_rassoc_comp_phead_get_time n =
  get_time := 0.0;
  let s = make_smallest_nested_list (n + 1) in
  let v = Int 100 in
  let s' = put (rassoc_comp phead n) s v [] in
  !get_time
  *)
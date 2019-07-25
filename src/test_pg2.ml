open Syntax
open Bxprog
open Utils
open Pg2

(* test pg2 skip1 *)
let pg2_skip1 =
  let s = Con(Int 1, Int 2) in
  let v = Unit in
  let (s, v) = pg2 skip1 s v [] in
  (s, v)

(* test pg2 replace *)
let pg2_replace = 
  let s = Con(Int 1, Int 2) in
  let v = Con(Int 3, Int 4) in
  let (s, v) = pg2 Replace s v [] in
  (s, v)

(* test pg2 rearrS *)
let pg2_rearrS =
  let s = Int 1 in
  let v = Con(Int 100, Int 100) in
  let (s, v) = pg2 rearrS_d s v [] in
  (s, v)

(* test pg2 rearrV *)
let pg2_rearrV =
  let s = Con(Int 1, Int 1) in
  let v = Int 100 in
  let (s, v) = pg2 rearrV_d s v [] in
  (s, v)

(* test pg2 prod *)
let pg2_prod =
  let s = Con(Int 1, Int 2) in
  let v = Con(Int 100, Unit) in
  let (s, v) = pg2 prod_rs s v [] in
  (s, v)

(* test pg2 phead *)
let pg2_phead =
  let s = Con(Int 1, Con(Int 2, Unit)) in
  let v = Int 100 in
  let (s, v) = pg2 phead s v [] in
  (s, v)

(* test pg2 phead_with_case *)
let pg2_phead_with_case =
  let s = Con(Int 1, Con(Int 2, Unit)) in
  let v = Int 100 in
  let (s, v) = pg2 phead_with_case s v [] in
  (s, v)

(* test pg2 phead2 *)
let pg2_phead2 =
  let s = Con(Int 1, Con(Int 2, Unit)) in
  let v = Con(Int 100, Unit) in
  let (s, v) = pg2 phead2 s v [] in
  (s, v)

(* test pg2 phead2_with_case *)
let pg2_phead2_with_case =
  let s = Con(Int 1, Con(Int 2, Unit)) in
  let v = Con(Int 100, Unit) in
  let (s, v) = pg2 phead2_with_case s v [] in
  (s, v)

(* test pg2 replace all *)
let pg2_replaceall =
  let s = Con(Int 1, Con(Int 1, Unit)) in
  let v = Int 100 in
  let (s, v) = pg2 replaceAlldef s v [] in
  (s, v)

(* test pg2 snoc *)
let pg2_bsnoc =
  let s = (Con(Int 2, (Con(Int 1, Unit)))) in
  let v = (Con(Int 3, (Con(Int 4, Unit)))) in
  let (s, v) = pg2 bsnoc_def s v [] in
  (s, v)

(* test pg2 comp of n replace *)
let pg2_lassoc_comp_replace n =
  let s = Int 1 in
  let v = Int 100 in
  let (s, v) = pg2 (lassoc_comp Replace n) s v [] in
  (s, v)

let pg2_rassoc_comp_replace n =
  let s = Int 1 in
  let v = Int 100 in
  let (s, v) = pg2 (rassoc_comp Replace n) s v [] in
  (s, v)

(* test pg2 comp of n phead *)
let pg2_lassoc_comp_phead n =
  let s = make_smallest_nested_list (n + 1) in
  let v = Int 100 in
  let (s, v) = pg2 (lassoc_comp phead n) s v [] in
  (s, v)

let pg2_rassoc_comp_phead n =
  let s = make_smallest_nested_list (n + 1) in
  let v = Int 100 in
  let (s, v) = pg2 (rassoc_comp phead n) s v [] in
  (s, v)

(* test pg2 comp of n phead_with_case *)
let pg2_lassoc_comp_phead_with_case n =
  let s = make_smallest_nested_list (n + 1) in
  let v = Int 100 in
  let (s, v) = pg2 (lassoc_comp phead_with_case n) s v [] in
  (s, v)

let pg2_rassoc_comp_phead_with_case n =
  let s = make_smallest_nested_list (n + 1) in
  let v = Int 100 in
  let (s, v) = pg2 (rassoc_comp phead_with_case n) s v [] in
  (s, v)

(* test pg2 comp of n phead2 *)
let pg2_lassoc_comp_phead2 n =
  let s = make_consecutive_list n in
  let v = Con(Int 100, Unit) in
  let (s, v) = pg2 (lassoc_comp phead2 n) s v [] in
  (s, v)

let pg2_rassoc_comp_phead2 n =
  let s = make_consecutive_list n in
  let v = Con(Int 100, Unit) in
  let (s, v) = pg2 (rassoc_comp phead2 n) s v [] in
  (s, v)

(* test pg2 comp of n phead2_with_case *)
let pg2_lassoc_comp_phead2_with_case n =
  let s = make_consecutive_list n in
  let v = Con(Int 100, Unit) in
  let (s, v) = pg2 (lassoc_comp phead2_with_case n) s v [] in
  (s, v)

let pg2_rassoc_comp_phead2_with_case n =
  let s = make_consecutive_list n in
  let v = Con(Int 100, Unit) in
  let (s, v) = pg2 (rassoc_comp phead2_with_case n) s v [] in
  (s, v)

(* test pg2 breverse *)
let pg2_breverse_1 =
  let s = (Con(Int 1, Unit)) in
  let v = (Con(Int 3, Unit)) in
  let (s, v) = pg2 breverse s v [] in
  (s, v)

let pg2_breverse_2 =
  let s = (Con(Int 1, Con(Int 2, Unit))) in
  let v = (Con(Int 3, Con(Int 4, Unit))) in
  let (s, v) = pg2 breverse s v [] in
  (s, v)

let pg2_breverse n =
  let s = make_consecutive_list n in
  let v = make_consecutive_list n in 
  let (s, v) = pg2 breverse s v [] in 
  (s, v)

(* test pg2 bmapreplace *)
let pg2_bmapreplace n =
  let s = make_consecutive_list n in
  let v = make_consecutive_list n in 
  let (s, v) = pg2 bmapreplace s v [] in 
  (s, v)

(* test pg2 bmapreplace_with_case *)
let pg2_bmapreplace_with_case n =
  let s = make_consecutive_list n in
  let v = make_consecutive_list n in 
  let (s, v) = pg2 bmapreplace_with_case s v [] in 
  (s, v)

let pg2_bmapreplace_0 =
  let s = Con(Int 1, Con(Int 2, Unit)) in
  let v = Con(Int 3, Con(Int 4, Unit)) in 
  let (s, v) = pg2 bmapreplace s v [] in 
  (s, v)
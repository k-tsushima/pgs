open Syntax
open Bxprog
open Utils
open Cpg

(* test cpg skip1 *)
let cpg_skip1 =
  let s = Con(Int 1, Int 2) in
  let v = Unit in
  let (ks, kv, s, v) = cpg skip1 (fun _ -> s) id s v [] in
  (s, v)

(* test cpg replace *)
let cpg_replace = 
  let s = Con(Int 1, Int 2) in
  let v = Con(Int 3, Int 4) in
  let (ks, kv, s, v) = cpg Replace (fun _ -> s) id s v [] in
  (s, v)

(* test cpg rearrS *)
let cpg_rearrS =
  let s = Int 1 in
  let v = Con(Int 100, Int 100) in
  let (ks, kv, s, v) = cpg rearrS_d (fun _ -> s) id s v [] in
  (s, v)

(* test cpg rearrV *)
let cpg_rearrV =
  let s = Con(Int 1, Int 1) in
  let v = Int 100 in
  let (ks, kv, s, v) = cpg rearrV_d (fun _ -> s) id s v [] in
  (s, v)

(* test cpg prod *)
let cpg_prod =
  let s = Con(Int 1, Int 2) in
  let v = Con(Int 100, Unit) in
  let (ks, kv, s, v) = cpg prod_rs (fun _ -> s) id s v [] in
  (s, v)

(* test cpg phead *)
let cpg_phead =
  let s = Con(Int 1, Con(Int 2, Unit)) in
  let v = Int 100 in
  let (ks, kv, s, v) = cpg phead (fun _ -> s) id s v [] in
  (s, v)

(* test cpg phead_with_case *)
let cpg_phead_with_case =
  let s = Con(Int 1, Con(Int 2, Unit)) in
  let v = Int 100 in
  let (ks, kv, s, v) = cpg phead_with_case (fun _ -> s) id s v [] in
  (s, v)

(* test cpg phead2 *)
let cpg_phead2 =
  let s = Con(Int 1, Con(Int 2, Unit)) in
  let v = Con(Int 100, Unit) in
  let (ks, kv, s, v) = cpg phead2 (fun _ -> s) id s v [] in
  (s, v)

(* test cpg phead2_with_case *)
let cpg_phead2_with_case =
  let s = Con(Int 1, Con(Int 2, Unit)) in
  let v = Con(Int 100, Unit) in
  let (ks, kv, s, v) = cpg phead2_with_case (fun _ -> s) id s v [] in
  (s, v)

(* test replace all *)
let cpg_replaceall =
  let s = Con(Int 1, Con(Int 1, Unit)) in
  let v = Int 100 in
  let (ks, kv, s, v) = cpg replaceAlldef (fun _ -> s) id s v [] in
  (s, v)

(* test bsnoc *)
let cpg_bsnoc =
  let s = (Con(Int 2, (Con(Int 1, Unit)))) in
  let v = (Con(Int 3, (Con(Int 4, Unit)))) in
  let (ks, kv, s, v) = cpg bsnoc_def (fun _ -> s) id s v [] in
  (s, v)

(* test comp of n replace *)
let cpg_lassoc_comp_replace n =
  let s = Int 1 in
  let v = Int 100 in
  let (ks, kv, s, v) = cpg (lassoc_comp Replace n) (fun _ -> s) id s v [] in
  (s, v)

let cpg_rassoc_comp_replace n =
  let s = Int 1 in
  let v = Int 100 in
  let (ks, kv, s, v) = cpg (rassoc_comp Replace n) (fun _ -> s) id s v [] in
  (s, v)

(* test comp of n phead *)
let cpg_lassoc_comp_phead n =
  let s = make_smallest_nested_list (n + 1) in
  let v = Int 100 in
  let (ks, kv, s, v) = cpg (lassoc_comp phead n) (fun _ -> s) id s v [] in
  (s, v)

let cpg_rassoc_comp_phead n =
  let s = make_smallest_nested_list (n + 1) in
  let v = Int 100 in
  let (ks, kv, s, v) = cpg (rassoc_comp phead n) (fun _ -> s) id s v [] in
  (s, v)

(* test comp of n phead_with_case *)
let cpg_lassoc_comp_phead_with_case n =
  let s = make_smallest_nested_list (n + 1) in
  let v = Int 100 in
  let (ks, kv, s, v) = cpg (lassoc_comp phead_with_case n) (fun _ -> s) id s v [] in
  (s, v)

let cpg_rassoc_comp_phead_with_case n =
  let s = make_smallest_nested_list (n + 1) in
  let v = Int 100 in
  let (ks, kv, s, v) = cpg (rassoc_comp phead_with_case n) (fun _ -> s) id s v [] in
  (s, v)

(* test comp of n phead2 *)
let cpg_lassoc_comp_phead2 n =
  let s = make_consecutive_list n in
  let v = Con(Int 100, Unit) in
  let (ks, kv, s, v) = cpg (lassoc_comp phead2 n) (fun _ -> s) id s v [] in
  (s, v)

let cpg_rassoc_comp_phead2 n =
  let s = make_consecutive_list n in
  let v = Con(Int 100, Unit) in
  let (ks, kv, s, v) = cpg (rassoc_comp phead2 n) (fun _ -> s) id s v [] in
  (s, v)

(* test comp of n phead2_with_case*)
let cpg_lassoc_comp_phead2_with_case n =
  let s = make_consecutive_list n in
  let v = Con(Int 100, Unit) in
  let (ks, kv, s, v) = cpg (lassoc_comp phead2_with_case n) (fun _ -> s) id s v [] in
  (s, v)

let cpg_rassoc_comp_phead2_with_case n =
  let s = make_consecutive_list n in
  let v = Con(Int 100, Unit) in
  let (ks, kv, s, v) = cpg (rassoc_comp phead2_with_case n) (fun _ -> s) id s v [] in
  (s, v)

(* test breverse *)
let cpg_breverse_1 =
  let s = (Con(Int 1, Unit)) in
  let v = (Con(Int 3, Unit)) in
  let (ks, kv, s, v) = cpg breverse (fun _ -> s) id s v [] in
  (s, v)

(* test breverse *)
let cpg_breverse_3 =
  let s = (Con(Int 1, Con(Int 2, Con(Int 3, Unit)))) in
  let v = (Con(Int 4, Con(Int 5, Con(Int 6, Unit)))) in
  let (ks, kv, s, v) = cpg breverse (fun _ -> s) id s v [] in
  (s, v)

(* test breverse *)
let cpg_breverse_cpg_3 =
  let s = (Con(Int 1, Con(Int 2, Con(Int 3, Unit)))) in
  let v = (Con(Int 4, Con(Int 5, Con(Int 6, Unit)))) in
  let (ks, kv, s, v) = cpg breverse (fun _ -> s) id s v [] in
  (s, v)

let cpg_breverse n =
  let s = make_consecutive_list n in
  let v = make_consecutive_list n in
  let (ks, kv, s, v) = cpg breverse (fun _ -> s) id s v [] in
  (s, v) 

(* test bmapreplace *)
let cpg_bmapreplace n =
  let s = make_consecutive_list n in
  let v = make_consecutive_list n in
  let (ks, kv, s, v) = cpg bmapreplace (fun _ -> s) id s v [] in
  (s, v)

(* test bmapreplace_with_case *)
let cpg_bmapreplace_with_case n =
  let s = make_consecutive_list n in
  let v = make_consecutive_list n in
  let (ks, kv, s, v) = cpg bmapreplace_with_case (fun _ -> s) id s v [] in
  (s, v)

(* let cpg_lassoc_comp_replace_count n =
    count_cpg := 0;
  let s = Int 1 in
  let v = Int 100 in
  let (ks, kv, s, v) = cpg (lassoc_comp Replace n) (fun _ -> s) id s v [] in
  !count_cpg

let cpg_rassoc_comp_replace_count n =
    count_cpg := 0;
  let s = Int 1 in
  let v = Int 100 in
  let (ks, kv, s, v) = cpg (rassoc_comp Replace n) (fun _ -> s) id s v [] in
  !count_cpg

(* test comp of n phead *)
let cpg_lassoc_comp_phead_count n =
    count_cpg := 0;
  let s = make_smallest_nested_list (n + 1) in
  let v = Int 100 in
  let (ks, kv, s, v) = cpg (lassoc_comp phead n) (fun _ -> s) id s v [] in
  !count_cpg

let cpg_rassoc_comp_phead_count n =
    count_cpg := 0;
  let s = make_smallest_nested_list (n + 1) in
  let v = Int 100 in
  let (ks, kv, s, v) = cpg (rassoc_comp phead n) (fun _ -> s) id s v [] in
  !count_cpg

let cpg_breverse_count n =
    count_cpg := 0;
  let s = make_consecutive_list n in
  let v = make_consecutive_list n in
  let (ks, kv, s, v) = cpg breverse (fun _ -> s) id s v [] in
  !count_cpg

let cpg_bmapreplace_count n =
    count_cpg := 0;
  let s = make_consecutive_list n in
  let v = make_consecutive_list n in
  let (ks, kv, s, v) = cpg bmapreplace (fun _ -> s) id s v [] in
  !count_cpg *)
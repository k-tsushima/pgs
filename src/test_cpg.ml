open Syntax
open Bxprog
open Utils
open Cpg

let cpg_skip1 =
  let s = Con(Int 1, Int 2) in
  let v = Unit in
  let (ks, kv, s, v) = cpg skip1 (fun _ -> s) id s v [] in
  (s, v)

let cpg_replace = 
  let s = Con(Int 1, Int 2) in
  let v = Con(Int 3, Int 4) in
  let (ks, kv, s, v) = cpg Replace (fun _ -> s) id s v [] in
  (s, v)

let cpg_rearrS =
  let s = Int 1 in
  let v = Con(Int 100, Int 100) in
  let (ks, kv, s, v) = cpg rearrS_d (fun _ -> s) id s v [] in
  (s, v)

let cpg_rearrV =
  let s = Con(Int 1, Int 1) in
  let v = Int 100 in
  let (ks, kv, s, v) = cpg rearrV_d (fun _ -> s) id s v [] in
  (s, v)

let cpg_prod =
  let s = Con(Int 1, Int 2) in
  let v = Con(Int 100, Unit) in
  let (ks, kv, s, v) = cpg prod_rs (fun _ -> s) id s v [] in
  (s, v)

let cpg_phead =
  let s = Con(Int 1, Con(Int 2, Unit)) in
  let v = Int 100 in
  let (ks, kv, s, v) = cpg phead (fun _ -> s) id s v [] in
  (s, v)

let cpg_phead2 =
  let s = Con(Int 1, Con(Int 2, Unit)) in
  let v = Con(Int 100, Unit) in
  let (ks, kv, s, v) = cpg phead2 (fun _ -> s) id s v [] in
  (s, v)

let cpg_replaceall =
  let s = Con(Int 1, Con(Int 1, Unit)) in
  let v = Int 100 in
  let (ks, kv, s, v) = cpg replaceAlldef (fun _ -> s) id s v [] in
  (s, v)

let cpg_bsnoc =
  let s = (Con(Int 2, (Con(Int 1, Unit)))) in
  let v = (Con(Int 3, (Con(Int 4, Unit)))) in
  let (ks, kv, s, v) = cpg bsnoc_def (fun _ -> s) id s v [] in
  (s, v)

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

let cpg_lassoc_comp_phead_2 n =
  let s = make_binary_list (n + 1) in
  let v = Int 100 in
  let (ks, kv, s, v) = cpg (lassoc_comp phead n) (fun _ -> s) id s v [] in
  (s, v)

let cpg_rassoc_comp_phead_2 n =
  let s = make_list_for_phead (n + 1) (100000) in
  let v = Int 100 in
  let (ks, kv, s, v) = cpg (rassoc_comp phead n) (fun _ -> s) id s v [] in
  (s, v)

let cpg_lassoc_comp_ptail n = 
  let s = make_consecutive_list (n + 3) in 
  let v = make_consecutive_list (10000) in 
  let (ks, kv, s, v) = cpg (lassoc_comp ptail n) (fun _ -> s) id s v [] in 
  (s, v)

let cpg_rassoc_comp_ptail n = 
  let s = make_consecutive_list (n + 3) in 
  let v = make_consecutive_list (10000) in 
  let (ks, kv, s, v) = cpg (rassoc_comp ptail n) (fun _ -> s) id s v [] in 
  (s, v)

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

let cpg_breverse n =
  let s = make_consecutive_list n in
  let v = make_consecutive_list n in
  let (ks, kv, s, v) = cpg breverse (fun _ -> s) id s v [] in
  (s, v) 

let cpg_lassoc_comp_breverse n =
  let s = make_consecutive_list n in
  let v = make_consecutive_list n in
  let (ks, kv, s, v) = cpg (lassoc_comp breverse n) (fun _ -> s) id s v [] in
  (s, v) 

let cpg_rassoc_comp_breverse n =
  let s = make_consecutive_list n in
  let v = make_consecutive_list n in
  let (ks, kv, s, v) = cpg (rassoc_comp breverse n) (fun _ -> s) id s v [] in
  (s, v) 

let cpg_bmapreplace n =
  let s = make_consecutive_list n in
  let v = make_consecutive_list n in
  let (ks, kv, s, v) = cpg bmapreplace (fun _ -> s) id s v [] in
  (s, v)

let cpg_lassoc_comp_bsnoc n =
  let s = make_consecutive_list (n + 1) in
  let v = make_consecutive_list_2 (n + 1) in
  let (ks, kv, s, v) = cpg (lassoc_comp bsnoc_def n) (fun _ -> s) id s v [] in
  (s, v) 

let cpg_rassoc_comp_bsnoc n =
  let s = make_consecutive_list (n + 1) in
  let v = make_consecutive_list_2 (n + 1) in
  let (ks, kv, s, v) = cpg (rassoc_comp bsnoc_def n) (fun _ -> s) id s v [] in
  (s, v) 

(* ========================= cpg (complex data )========================= *)
let cpg_lassoc_comp_bsnoc_ldata n =
  let s = make_list_of_binary_tree n in
  let v = s in
  let (ks, kv, s, v) = cpg (lassoc_comp bsnoc_def n) (fun _ -> s) id s v [] in
  (s, v) 

let cpg_rassoc_comp_bsnoc_ldata n =
  let s = make_list_of_binary_tree n in
  let v = s in
  let (ks, kv, s, v) = cpg (rassoc_comp bsnoc_def n) (fun _ -> s) id s v [] in
  (s, v) 

let cpg_lassoc_comp_ptail_ldata n =
  let s = make_list_of_binary_tree (n + 1) in 
  let v = make_list 1 10 in 
  let (ks, kv, s, v) = cpg (lassoc_comp ptail n) (fun _ -> s) id s v [] in 
  (s, v)

let cpg_rassoc_comp_ptail_ldata n =
  let s = make_list_of_binary_tree (n + 1) in 
  let v = make_list 1 10 in 
  let (ks, kv, s, v) = cpg (rassoc_comp ptail n) (fun _ -> s) id s v [] in 
  (s, v)

let cpg_lassoc_comp_replace_ldata n =
  let s = make_list_of_binary_tree n in
  let v = make_list_of_binary_tree n in
  let (ks, kv, s, v) = cpg (lassoc_comp Replace n) (fun _ -> s) id s v [] in
  (s, v) 

let cpg_rassoc_comp_replace_ldata n =
  let s = make_list_of_binary_tree n in
  let v = make_list_of_binary_tree n in
  let (ks, kv, s, v) = cpg (rassoc_comp Replace n) (fun _ -> s) id s v [] in
  (s, v) 

let cpg_breverse_ldata n =
  let s = make_list_of_binary_tree n in
  let v = s in
  let (ks, kv, s, v) = cpg breverse (fun _ -> s) id s v [] in
  (s, v) 

let cpg_bmapreplace_ldata n =
  let s = make_list_of_binary_tree n in
  let v = s in
  let (ks, kv, s, v) = cpg bmapreplace (fun _ -> s) id s v [] in
  (s, v)
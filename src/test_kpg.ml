open Syntax
open Bxprog
open Utils
open Kpg

let kpg_skip1 =
  let s = Con(Int 1, Int 2) in
  let v = Unit in
  let (ks, kv, ks', kv', s', v') = kpg skip1 (fun _ -> s) id id id s v [] in
  (ks' s', kv' v')

let kpg_replace = 
  let s = Con(Int 1, Int 2) in
  let v = Con(Int 3, Int 4) in
  let (ks, kv, ks', kv', s', v') = kpg Replace (fun _ -> s) id id id s v [] in
  (ks' s', kv' v')

let kpg_rearrS =
  let s = Int 1 in
  let v = Con(Int 100, Int 100) in
  let (ks, kv, ks', kv', s', v') = kpg rearrS_d (fun _ -> s) id id id s v [] in
  (ks' s', kv' v')

let kpg_rearrV =
  let s = Con(Int 1, Int 1) in
  let v = Int 100 in
  let (ks, kv, ks', kv', s', v') = kpg rearrV_d (fun _ -> s) id id id s v [] in
  (ks' s', kv' v')

let kpg_prod =
  let s = Con(Int 1, Int 2) in
  let v = Con(Int 100, Unit) in
  let (ks, kv, ks', kv', s', v') = kpg prod_rs (fun _ -> s) id id id s v [] in
  (ks' s', kv' v')

let kpg_phead =
  let s = Con(Int 1, Con(Int 2, Unit)) in
  let v = Int 100 in
  let (ks, kv, ks', kv', s', v') = kpg phead (fun _ -> s) id id id s v [] in
  (ks' s', kv' v')

let kpg_phead2 =
  let s = Con(Int 1, Con(Int 2, Unit)) in
  let v = Con(Int 100, Unit) in
  let (ks, kv, ks', kv', s', v') = kpg phead2 (fun _ -> s) id id id s v [] in
  (ks' s', kv' v')

let kpg_replaceall =
  let s = Con(Int 1, Con(Int 1, Unit)) in
  let v = Int 100 in
  let (ks, kv, ks', kv', s', v') = kpg replaceAlldef (fun _ -> s) id id id s v [] in
  (ks' s', kv' v')

let kpg_bsnoc =
  let s = (Con(Int 2, (Con(Int 1, Unit)))) in
  let v = (Con(Int 3, (Con(Int 4, Unit)))) in
  let (ks, kv, ks', kv', s', v') = kpg bsnoc_def (fun _ -> s) id id id s v [] in
  (ks' s', kv' v')

let kpg_lassoc_comp_replace n =
  let s = Int 1 in
  let v = Int 100 in
  let (ks, kv, ks', kv', s', v') = kpg (lassoc_comp Replace n) (fun _ -> s) id id id s v [] in
  (ks' s', kv' v')

let kpg_rassoc_comp_replace n =
  let s = Int 1 in
  let v = Int 100 in
  let (ks, kv, ks', kv', s', v') = kpg (rassoc_comp Replace n) (fun _ -> s) id id id s v [] in
  (ks' s', kv' v')

let kpg_lassoc_comp_phead n =
  let s = make_smallest_nested_list (n + 1) in
  let v = Int 100 in
  let (ks, kv, ks', kv', s', v') = kpg (lassoc_comp phead n) (fun _ -> s) id id id s v [] in
  (ks' s', kv' v')

let kpg_rassoc_comp_phead n =
  let s = make_smallest_nested_list (n + 1) in
  let v = Int 100 in
  let (ks, kv, ks', kv', s', v') = kpg (rassoc_comp phead n) (fun _ -> s) id id id s v [] in
  (ks' s', kv' v')

let kpg_lassoc_comp_phead_2 n =
  let s = make_binary_list (n + 1) in
  let v = Int 100 in
  let (ks, kv, ks', kv', s', v') = kpg (lassoc_comp phead n) (fun _ -> s) id id id s v [] in
  (ks' s', kv' v')

let kpg_rassoc_comp_phead_2 n =
  let s = make_list_for_phead (n + 1) (100000) in
  let v = Int 100 in
  let (ks, kv, ks', kv', s', v') = kpg (rassoc_comp phead n) (fun _ -> s) id id id s v [] in
  (ks' s', kv' v')

let kpg_lassoc_comp_ptail n = 
  let s = make_consecutive_list (n + 3) in 
  let v = make_consecutive_list (10000) in 
  let (ks, kv, ks', kv', s', v') = kpg (lassoc_comp ptail n) (fun _ -> s) id id id s v [] in 
  (ks' s', kv' v')

let kpg_rassoc_comp_ptail n = 
  let s = make_consecutive_list (n + 3) in 
  let v = make_consecutive_list (10000) in 
  let (ks, kv, ks', kv', s', v') = kpg (rassoc_comp ptail n) (fun _ -> s) id id id s v [] in 
  (ks' s', kv' v')

let kpg_lassoc_comp_phead2 n =
  let s = make_consecutive_list n in
  let v = Con(Int 100, Unit) in
  let (ks, kv, ks', kv', s', v') = kpg (lassoc_comp phead2 n) (fun _ -> s) id id id s v [] in
  (ks' s', kv' v')

let kpg_rassoc_comp_phead2 n =
  let s = make_consecutive_list n in
  let v = Con(Int 100, Unit) in
  let (ks, kv, ks', kv', s', v') = kpg (rassoc_comp phead2 n) (fun _ -> s) id id id s v [] in
  (ks' s', kv' v')

let kpg_breverse n =
  let s = make_consecutive_list n in
  let v = make_consecutive_list n in
  let (ks, kv, ks', kv', s', v') = kpg breverse (fun _ -> s) id id id s v [] in
  (ks' s', kv' v') 

let kpg_lassoc_comp_breverse n =
  let s = make_consecutive_list n in
  let v = make_consecutive_list n in
  let (ks, kv, ks', kv', s', v') = kpg (lassoc_comp breverse n) (fun _ -> s) id id id s v [] in
  (ks' s', kv' v') 

let kpg_rassoc_comp_breverse n =
  let s = make_consecutive_list n in
  let v = make_consecutive_list n in
  let (ks, kv, ks', kv', s', v') = kpg (rassoc_comp breverse n) (fun _ -> s) id id id s v [] in
  (ks' s', kv' v') 

let kpg_bmapreplace n =
  let s = make_consecutive_list n in
  let v = make_consecutive_list n in
  let (ks, kv, ks', kv', s', v') = kpg bmapreplace (fun _ -> s) id id id s v [] in
  (ks' s', kv' v')

let kpg_lassoc_comp_bsnoc n =
  let s = make_consecutive_list (n + 1) in
  let v = make_consecutive_list_2 (n + 1) in
  let (ks, kv, ks', kv', s', v') = kpg (lassoc_comp bsnoc_def n) (fun _ -> s) id id id s v [] in
  (ks' s', kv' v') 

let kpg_rassoc_comp_bsnoc n =
  let s = make_consecutive_list (n + 1) in
  let v = make_consecutive_list_2 (n + 1) in
  let (ks, kv, ks', kv', s', v') = kpg (rassoc_comp bsnoc_def n) (fun _ -> s) id id id s v [] in
  (ks' s', kv' v') 

(* ========================= kpg (complex data )========================= *)
let kpg_lassoc_comp_bsnoc_ldata n =
  let s = make_list_of_binary_tree n in
  let v = s in
  let (ks, kv, ks', kv', s', v') = kpg (lassoc_comp bsnoc_def n) (fun _ -> s) id id id s v [] in
  (ks' s', kv' v') 

let kpg_rassoc_comp_bsnoc_ldata n =
  let s = make_list_of_binary_tree n in
  let v = s in
  let (ks, kv, ks', kv', s', v') = kpg (rassoc_comp bsnoc_def n) (fun _ -> s) id id id s v [] in
  (ks' s', kv' v')

let kpg_lassoc_comp_ptail_ldata n =
  let s = make_list_of_binary_tree (n + 1) in 
  let v = make_list 1 10 in 
  let (ks, kv, ks', kv', s', v') = kpg (lassoc_comp ptail n) (fun _ -> s) id id id s v [] in 
  (ks' s', kv' v')

let kpg_rassoc_comp_ptail_ldata n =
  let s = make_list_of_binary_tree (n + 1) in 
  let v = make_list 1 10 in 
  let (ks, kv, ks', kv', s', v') = kpg (rassoc_comp ptail n) (fun _ -> s) id id id s v [] in 
  (ks' s', kv' v')

let kpg_lassoc_comp_replace_ldata n =
  let s = make_list_of_binary_tree n in
  let v = make_list_of_binary_tree n in
  let (ks, kv, ks', kv', s', v') = kpg (lassoc_comp Replace n) (fun _ -> s) id id id s v [] in
  (ks' s', kv' v') 

let kpg_rassoc_comp_replace_ldata n =
  let s = make_list_of_binary_tree n in
  let v = make_list_of_binary_tree n in
  let (ks, kv, ks', kv', s', v') = kpg (rassoc_comp Replace n) (fun _ -> s) id id id s v [] in
  (ks' s', kv' v')

let kpg_breverse_ldata n =
  let s = make_list_of_binary_tree n in
  let v = s in
  let (ks, kv, ks', kv', s', v') = kpg breverse (fun _ -> s) id id id s v [] in
  (ks' s', kv' v') 

let kpg_bmapreplace_ldata n =
  let s = make_list_of_binary_tree n in
  let v = s in
  let (ks, kv, ks', kv', s', v') = kpg bmapreplace (fun _ -> s) id id id s v [] in
  (ks' s', kv' v')
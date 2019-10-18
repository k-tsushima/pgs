open Syntax
open Bxprog
open Utils
open Kpg

(* test kpg skip1 *)
let kpg_skip1 =
  let s = Con(Int 1, Int 2) in
  let v = Unit in
  let (ks, kv, ks', kv', s', v') = kpg skip1 (fun _ -> s) id id id s v [] in
  (ks' s', kv' v')

(* test kpg replace *)
let kpg_replace = 
  let s = Con(Int 1, Int 2) in
  let v = Con(Int 3, Int 4) in
  let (ks, kv, ks', kv', s', v') = kpg Replace (fun _ -> s) id id id s v [] in
  (ks' s', kv' v')

(* test kpg rearrS *)
let kpg_rearrS =
  let s = Int 1 in
  let v = Con(Int 100, Int 100) in
  let (ks, kv, ks', kv', s', v') = kpg rearrS_d (fun _ -> s) id id id s v [] in
  (ks' s', kv' v')

(* test kpg rearrV *)
let kpg_rearrV =
  let s = Con(Int 1, Int 1) in
  let v = Int 100 in
  let (ks, kv, ks', kv', s', v') = kpg rearrV_d (fun _ -> s) id id id s v [] in
  (ks' s', kv' v')

(* test kpg prod *)
let kpg_prod =
  let s = Con(Int 1, Int 2) in
  let v = Con(Int 100, Unit) in
  let (ks, kv, ks', kv', s', v') = kpg prod_rs (fun _ -> s) id id id s v [] in
  (ks' s', kv' v')

(* test kpg phead *)
let kpg_phead =
  let s = Con(Int 1, Con(Int 2, Unit)) in
  let v = Int 100 in
  let (ks, kv, ks', kv', s', v') = kpg phead (fun _ -> s) id id id s v [] in
  (ks' s', kv' v')

(* test kpg phead2 *)
let kpg_phead2 =
  let s = Con(Int 1, Con(Int 2, Unit)) in
  let v = Con(Int 100, Unit) in
  let (ks, kv, ks', kv', s', v') = kpg phead2 (fun _ -> s) id id id s v [] in
  (ks' s', kv' v')

(* test replace all *)
let kpg_replaceall =
  let s = Con(Int 1, Con(Int 1, Unit)) in
  let v = Int 100 in
  let (ks, kv, ks', kv', s', v') = kpg replaceAlldef (fun _ -> s) id id id s v [] in
  (ks' s', kv' v')

(* test bsnoc *)
let kpg_bsnoc =
  let s = (Con(Int 2, (Con(Int 1, Unit)))) in
  let v = (Con(Int 3, (Con(Int 4, Unit)))) in
  let (ks, kv, ks', kv', s', v') = kpg bsnoc_def (fun _ -> s) id id id s v [] in
  (ks' s', kv' v')

(* test comp of n replace *)
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

(* test comp of n phead *)
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

(* test comp of n phead2 *)
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

(* test breverse *)
let kpg_breverse n =
  let s = make_consecutive_list n in
  let v = make_consecutive_list n in
  let (ks, kv, ks', kv', s', v') = kpg breverse (fun _ -> s) id id id s v [] in
  (ks' s', kv' v') 

(* test comp of n breverse *)
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

(* test bmapreplace *)
let kpg_bmapreplace n =
  let s = make_consecutive_list n in
  let v = make_consecutive_list n in
  let (ks, kv, ks', kv', s', v') = kpg bmapreplace (fun _ -> s) id id id s v [] in
  (ks' s', kv' v')
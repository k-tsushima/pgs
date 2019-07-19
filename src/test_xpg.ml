open Syntax
open Bxprog
open Utils
open Xpg

(* test xpg skip1 *)
let xpg_skip1 =
  let s = Con(Int 1, Int 2) in
  let v = Unit in
  let (ks, kv, ks', kv', s', v') = xpg skip1 (fun _ -> s) (fun _ -> v) id id s v [] in
    (ks' s', kv' v')

(* test xpg replace *)
let xpg_replace = 
  let s = Con(Int 1, Int 2) in
  let v = Con(Int 3, Int 4) in
  let (ks, kv, ks', kv', s', v') = xpg Replace (fun _ -> s) (fun _ -> v) id id s v [] in
    (ks' s', kv' v')

(* test xpg rearrS *)
let xpg_rearrS =
  let s = Int 1 in
  let v = Con(Int 100, Int 100) in
  let (ks, kv, ks', kv', s', v') = xpg rearrS_d (fun _ -> s) (fun _ -> v) id id s v [] in
    (ks' s', kv' v')

(* test xpg rearrV *)
let xpg_rearrV =
  let s = Con(Int 1, Int 1) in
  let v = Int 100 in
  let (ks, kv, ks', kv', s', v') = xpg rearrV_d (fun _ -> s) (fun _ -> v) id id s v [] in
    (ks' s', kv' v')

(* test xpg prod *)
let xpg_prod =
  let s = Con(Int 1, Int 2) in
  let v = Con(Int 100, Unit) in
  let (ks, kv, ks', kv', s', v') = xpg prod_rs (fun _ -> s) (fun _ -> v) id id s v [] in
    (ks' s', kv' v')

(* test xpg phead *)
let xpg_phead =
  let s = Con(Int 1, Con(Int 2, Unit)) in
  let v = Int 100 in
  let (ks, kv, ks', kv', s', v') = xpg phead (fun _ -> s) (fun _ -> v) id id s v [] in
    (ks' s', kv' v')

(* test replace all *)
let xpg_replaceall =
  let s = Con(Int 1, Con(Int 1, Unit)) in
  let v = Int 100 in
  let (ks, kv, ks', kv', s', v') = xpg replaceAlldef (fun _ -> s) (fun _ -> v) id id s v [] in
    (ks' s', kv' v')

(* test bsnoc *)
let xpg_bsnoc =
  let s = (Con(Int 2, (Con(Int 1, Unit)))) in
  let v = (Con(Int 3, (Con(Int 4, Unit)))) in
  let (ks, kv, ks', kv', s', v') = xpg bsnoc_def (fun _ -> s) (fun _ -> v) id id s v [] in
    (ks' s', kv' v')

(* test comp of n replace *)
let xpg_lassoc_comp_replace n =
  let s = Int 1 in
  let v = Int 100 in
  let (ks, kv, ks', kv', s', v') = xpg (lassoc_comp Replace n) (fun _ -> s) (fun _ -> v) id id s v [] in
   (ks' s', kv' v')

let xpg_rassoc_comp_replace n =
	let s = Int 1 in
	let v = Int 100 in
	let (ks, kv, ks', kv', s', v') = xpg (rassoc_comp Replace n) (fun _ -> s) (fun _ -> v) id id s v [] in
		(ks' s', kv' v')

(* test comp of n phead *)
let xpg_lassoc_comp_phead n =
  let s = make_smallest_nested_list (n + 1) in
  let v = Int 100 in
  let (ks, kv, ks', kv', s', v') = xpg (lassoc_comp phead n) (fun _ -> s) (fun _ -> v) id id s v [] in
   (ks' s', kv' v')

let xpg_rassoc_comp_phead n =
let s = make_smallest_nested_list (n + 1) in
let v = Int 100 in
let (ks, kv, ks', kv', s', v') = xpg (rassoc_comp phead n) (fun _ -> s) (fun _ -> v) id id s v [] in
  (ks' s', kv' v')

(* test breverse *)
let xpg_breverse_1 =
  let s = (Con(Int 1, Unit)) in
  let v = (Con(Int 3, Unit)) in
  let (ks, kv, ks', kv', s', v') = xpg breverse (fun _ -> s) (fun _ -> v) id id s v [] in
    (ks' s', kv' v')

(* test breverse *)
let xpg_breverse_3 =
  let s = (Con(Int 1, Con(Int 2, Con(Int 3, Unit)))) in
  let v = (Con(Int 4, Con(Int 5, Con(Int 6, Unit)))) in
  let (ks, kv, ks', kv', s', v') = xpg breverse (fun _ -> s) (fun _ -> v) id id s v [] in
    (ks' s', kv' v')

(* test breverse *)
let xpg_breverse_cpg_3 =
  let s = (Con(Int 1, Con(Int 2, Con(Int 3, Unit)))) in
  let v = (Con(Int 4, Con(Int 5, Con(Int 6, Unit)))) in
  let (ks, kv, ks', kv', s', v') = xpg breverse (fun _ -> s) (fun _ -> v) id id s v [] in
    (ks' s', kv' v')

let xpg_breverse n =
  let s = make_consecutive_list n in
  let v = make_consecutive_list n in
  let (ks, kv, ks', kv', s', v') = xpg breverse (fun _ -> s) (fun _ -> v) id id s v [] in
    (ks' s', kv' v')

let xpg_breverse_cpg n =
  let s = make_consecutive_list n in
  let v = make_consecutive_list n in
  let (ks, kv, ks', kv', s', v') = xpg breverse (fun _ -> s) (fun _ -> v) id id s v [] in
    (ks s, kv v)